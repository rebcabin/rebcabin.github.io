;;; Definitions for the semantic functions from the R5RS DS, Section 7.2.3
;;;
;;; This module is intended to remain closely equivalent to the R5RS DS,
;;; and should not be changed without good reason.
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


(define (expression-meaning-constant K)
  (lambda (r kappa)
    (ds:send (constant-meaning K) kappa)))

(define (expression-meaning-identifier I)
  (lambda (r k)
    (ds:hold (ds:lookup r I)
             (ds:single 
              (lambda (e) 
                (if (eq? e ds:undefined)
                    (ds:wrong "undefined variable")
                    (ds:send e k)))))))

(define (expression-meaning-application E0 E*)
  (lambda (r k)
    ((expression-sequence-meaning
      (ds:permute (ds:append (ds:sequence E0) E*)))
     r
     (lambda (e*) 
       ((lambda (e*) ((ds:applicate (ds:first e*) (ds:rest e*)) k))
        (ds:unpermute e*))))))

(define (expression-meaning-abstraction-fixed-arity I* G* E0)
  (lambda (r k)
    (lambda (s)
      (if (ds:location? (ds:new s))
          ((ds:send 
            (ds:inject-value
             (ds:sequence 
              (ds:project-location (ds:new s))
              (lambda (e* k-prime)
                (if (= (ds:length e*) (ds:length I*))
                    (ds:tievals 
                     (lambda (a*)
                       ((lambda (r-prime)
                          ((command-sequence-meaning G*)
                           r-prime
                           ; delay required to avoid out-of-order evaluation of final expression
                           (delay ((expression-meaning E0) r-prime k-prime))))
                        (ds:extends r I* a*)))
                     e*)
                    (ds:wrong "wrong number of arguments"))))) 
            k)
           (ds:update (ds:project-location (ds:new s)) ds:unspecified s))
          ((ds:wrong "out of memory") s)))))

(define (expression-meaning-abstraction-variable-arity I* I G* E0)
  (lambda (r k)
    (lambda (s)
      (if (ds:location? (ds:new s))
          ((ds:send 
            (ds:inject-value 
             (ds:sequence 
              (ds:project-location (ds:new s))
              (lambda (e* k-prime)
                (if (>= (ds:length e*) (ds:length I*))
                    (ds:tievalsrest
                     (lambda (a*)
                       ((lambda (r-prime)
                          ((command-sequence-meaning G*) 
                           r-prime 
                            ; delay required to avoid out-of-order evaluation of final expression
                           (delay ((expression-meaning E0) r-prime k-prime))))
                        (ds:extends r (ds:append I* (ds:sequence I)) a*)))
                     e*
                     (ds:length I*))
                    (ds:wrong "too few arguments")))))
            k)
           (ds:update (ds:project-location (ds:new s)) ds:unspecified s))
          ((ds:wrong "out of memory") s)))))

(define (expression-meaning-abstraction-list-arity I G* E0)
  (expression-meaning-abstraction-variable-arity (ds:sequence) I G* E0))

(define (expression-meaning-if-then-else E0 E1 E2)
  (lambda (r k)
    ((expression-meaning E0) 
     r 
     (ds:single (lambda (e) 
                  (if (ds:truish e) 
                      ((expression-meaning E1) r k)
                      ((expression-meaning E2) r k)))))))

(define (expression-meaning-if-then E0 E1)
  (lambda (r k)
    ((expression-meaning E0) 
     r 
     (ds:single (lambda (e) 
                  (if (ds:truish e) 
                      ((expression-meaning E1) r k)
                      (ds:send ds:unspecified k)))))))

;;; R5RS comment: Here and elsewhere, any expressed value other
;;; than 'undefined' may be used in place of 'unspecified'.

(define (expression-meaning-assignment I E)
  (lambda (r k)
    ((expression-meaning E) 
     r 
     (ds:single (lambda (e) 
                  (ds:assign (ds:lookup r I) 
                             e 
                             (ds:send ds:unspecified k)))))))

(define (expression-meaning-null)
  (lambda (r k)
    (k (ds:sequence))))

(define (expression-meaning-sequence E0 E*)
  (lambda (r k)
    ((expression-meaning E0) 
     r 
     ;; note lowercase e0 shadows uppercase E0 in case-insensitive Scheme; OK in this instance.
     (ds:single (lambda (e0) 
                  ((expression-sequence-meaning E*) 
                   r 
                   (lambda (e*) 
                     (k (ds:append (ds:sequence e0) e*)))))))))

;; Use of 'force' is required to compensate for delay, used above to avoid
;; out-of-order evaluation of the final expression in a command sequence.
(define (command-meaning-null)
  (lambda (r theta) (force theta)))

(define (command-meaning-sequence G0 G*)
  (lambda (r theta)
    ((expression-meaning G0) 
     r
     (lambda (e*) ((command-sequence-meaning G*) r theta)))))
