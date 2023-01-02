;;; toplevel-dispatch.scm
;;;
;;; Procedures to parse top-level Scheme expressions and invoke the
;;; appropriate lower-level handlers.
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


(define (expression-meaning-toplevel E)
  (if (not (pair? E))
      (expression-meaning E)
      (case (ds:first E)
        ((begin)
         ;; top-level begin logic was adapated
         ;; from lambda command sequence
         ;; processing
         (let* ((expr-pair (split-list-at-end (ds:rest E)))
                (G* (car expr-pair))
                (E0 (cdr expr-pair)))
           (newline) (print G*)
           (newline) (print E0)
           (lambda (r k)
             ((meaning-toplevel-command-sequence G*)
              r
              ;; delay required to avoid out-of-order evaluation of final
              ;; expression
              (delay ((expression-meaning-toplevel E0) r k))))))
        ((define)
         (expression-guard-min
          E 3
          (lambda ()
            (if (pair? (cadr E))
                (expression-meaning-procedure-definition
                 (ds:second E) (cddr E))
                (expression-guard
                 E 3
                 (lambda ()
                   (expression-meaning-toplevel-definition
                    (ds:second E) (ds:third E))))))))
        (else
         (expression-meaning E)))))

;; The following were adapted from the equivalent semantic functions,
;; to support processing of commands in a top-level 'begin' expression.

;; adapted from meaning-command-sequence
(define (meaning-toplevel-command-sequence G*)
  (if (zero? (ds:length G*))
      (command-meaning-null)
      (toplevel-command-meaning-sequence (ds:first G*) (ds:rest G*))))

;; adapted from command-meaning-sequence
(define (toplevel-command-meaning-sequence G0 G*)
  (lambda (r theta)
    ((expression-meaning-toplevel G0)
     r
     (lambda (e*) ((meaning-toplevel-command-sequence G*) r theta)))))
