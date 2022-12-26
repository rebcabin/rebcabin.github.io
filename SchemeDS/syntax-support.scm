;;; syntax-support.scm
;;;
;;; Support procedures for syntactic transformation
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


;; split-list-at-end returns a pair whose car is a list consisting of all but the 
;; last element of the source list, and whose cdr is the last element of the source 
;; list.  Using set-cdr! would be more efficient, but we're avoiding mutation.
(define (split-list-at-end l)
  (let loop ((l l)
             (m '()))
    (if (pair? l)
        (if (null? (cdr l))
            (cons (reverse m) (car l))
            (loop (cdr l) (cons (car l) m)))
        (cons (reverse m) l))))

(define (malformed-expression i)
  (lambda (e k)
    (ds:wrong (string-append (symbol->string i) ": malformed expression"))))

;; Check that an expression has specified number of elements.
;; If OK, evaluates thunk and returns result, else returns error procedure.
(define (expression-guard E n thunk)
  (if (= (length E) n) 
      (thunk)
      (malformed-expression (car E))))

;; Check that an expression has at least the specified number of elements.
;; If OK, evaluates thunk and returns result, else returns error procedure.
(define (expression-guard-min E n thunk)
  (if (>= (length E) n)
      (thunk)
      (malformed-expression (car E))))
