;;; semantic-dispatch.scm
;;;
;;; Procedures to invoke the appropriate semantic functions
;;; based on the syntax of the provided expression.
;;;
;;; Copyright (C) 2002 Anton van Straaten <anton@appsolutions.com>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License,
;;; version 2, as published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, see http://www.gnu.org/copyleft/gpl.html
;;;
;;; -------------------------------------------------------------------------
;;;


;; Primary non-top-level dispatch function for expression meaning
;; todo: prevent dup symbols in arg lists; other syntactical
;; checks.

(define (expression-meaning E)
  (if (not (pair? E))
      (if (symbol? E)
          (expression-meaning-identifier E)
          (expression-meaning-constant E))
      (if (list? (ds:rest E))
          (case (ds:first E)
            ((lambda) (expression-guard-min E 3
                       (lambda ()
                         (expression-meaning-abstraction
                          (transform-internal-definitions (ds:rest E))))))
            ;; version without internal definitions:
            ;; (expression-meaning-abstraction (ds:rest E))
            ((if)     (expression-meaning-if (ds:rest E)))
            ((set!)   (expression-guard E 3
                       (lambda ()
                         (expression-meaning-assignment
                          (ds:second E) (ds:third E)))))
            ((quote)  (expression-guard E 2
                       (lambda ()
                         (expression-meaning-quote
                          (ds:second E)))))
            ;; above are defined by DS; below are derived
            ((let)    (expression-guard-min E 3
                       (lambda ()
                         (if (symbol? (ds:second E))
                             (expression-meaning-named-let
                              (ds:second E)
                              (ds:third E)
                              (cdddr E))
                             (expression-meaning-let
                              (ds:second E)
                              (cddr E))))))
            ((letrec) (expression-guard-min E 3
                       (lambda ()
                         (expression-meaning-letrec (ds:second E) (cddr E)))))
            ((begin)  (expression-meaning-begin (ds:rest E)))
            (else     (expression-meaning-application
                       (ds:first E) (ds:rest E))))
          (malformed-expression (ds:first E)))))

;; constant-meaning
;;
;; The definition of this function was deliberately omitted from
;; the Scheme DS, to avoid complicating the semantics. Similarly,
;; it is defined here by effectively snarfing constants from the
;; host Scheme implementation.
;;
;; The meaning of a constant is the constant itself.

(define (constant-meaning K) K)

;; quoted expressions must be transferred from host Scheme to store
(define (expression-meaning-quote E)
  (lambda (r k)
    (lambda (s)
      ((ds:host-value->value E ds:immutable-cons k) s))))

;; ds:immutable-cons is a direct copy of the auxiliary function 'cons',
;; with a reference to 'true' changed to 'false'.  This supports the
;; creation of immutable pairs.  'cons' was copied to avoid modifying
;; a DS-defined procedure.
;;
;; immutable-cons : E* -> K -> C
(define ds:immutable-cons
  (ds:twoarg
   (lambda (e1 e2 k)
     (lambda (s)
       (if (ds:location? (ds:new s))
           ((lambda (s-prime)
              (if (ds:location? (ds:new s-prime))
                  ((ds:send (ds:inject-value
                             (ds:sequence (ds:project-location (ds:new s))
                                          (ds:project-location (ds:new s-prime))
                                          ds:false))
                            k)
                   (ds:update (ds:project-location (ds:new s-prime)) e2 s-prime))
                  (ds:wrong "out of memory" s-prime)))
            (ds:update (ds:project-location (ds:new s)) e1 s))
           ((ds:wrong "out of memory") s))))))

(define (expression-sequence-meaning E*)
  (if (zero? (length E*))
      (expression-meaning-null)
      (expression-meaning-sequence (ds:first E*) (ds:rest E*))))

(define (command-sequence-meaning G*)
  (if (zero? (length G*))
      (command-meaning-null)
      (command-meaning-sequence (ds:first G*) (ds:rest G*))))

(define (expression-meaning-if E)
  (case (length E)
    ((2) (expression-meaning-if-then (ds:first E) (ds:second E)))
    ((3) (expression-meaning-if-then-else (ds:first E) (ds:second E) (ds:third E)))
    (else (malformed-expression 'if))))

(define (expression-meaning-abstraction E)
  (if (< (length E) 2)
  (begin (display E)
      (malformed-expression 'lambda))      ; this won't handle transformed syntax; do at higher level
      (let* ((param-list (ds:first E))
             (expr-pair (split-list-at-end (ds:rest E)))
             (G* (car expr-pair))
             (E0 (cdr expr-pair)))
        (cond
          ((list? param-list)
           (expression-meaning-abstraction-fixed-arity param-list G* E0))
          ((pair? param-list)
           (let ((params-pair (split-list-at-end param-list)))
             (expression-meaning-abstraction-variable-arity (car params-pair)
                                                            (cdr params-pair)
                                                            G*
                                                            E0)))
          (else (expression-meaning-abstraction-list-arity param-list G* E0))))))
