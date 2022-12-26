;;; prelude.scm
;;;
;;; Loaded by DS interpreter during initialization (not loaded by host Scheme).
;;; Deliberately kept small, to avoid enlarging interpreter's store too much.
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


(define (not x) (if x #f #t))
(define (null? x) (eqv? x '()))
(define (negative? n) (< n 0))
(define (positive? n) (> n 0))
(define (zero? n) (= n 0))

(define (caar l) (car (car l)))
(define (cadr l) (car (cdr l)))
(define (cdar l) (cdr (car l)))
(define (cddr l) (cdr (cdr l)))

(define call/cc call-with-current-continuation)

;; silent repl, not currently used
;(define quiet-repl
;  (lambda (port)
;    (main-repl port #f (lambda (x) #f))))

;; default repl, invoked bootstrap-source in interpreter-repl.scm
(define chatty-repl
  (lambda (port)
    (define repl-input-prompt ">> ")
    (define repl-output-text "Value: ")
    (define display-value 
      (lambda (v)
        (newline)
        (display repl-output-text)(write v)(newline)))

    (main-repl port
               (lambda () (display repl-input-prompt))
               display-value)))

; end of prelude
