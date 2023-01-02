;;; interpreter-repl.scm
;;;
;;; REPL and support procedures for interpreter based on R5RS DS implementation
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


;; Accepts program as s-expression. Invokes semantic function
;; 'expression-meaning' to obtain a function M, which represents
;; the denotational meaning of the specified program. This meaning
;; function is used to evaluate the program. Result is printed,
;; along with internal information such as a dump of the store.
;;
;; E : expression
;; K : continuation - will have access to store
;; X : exit continuation.  Note this determines handling of (exit) and
;;     (exit errorlevel); latter not yet supported in procedures below.
(define (eval-expression E K X)
  (let* ((context (initialize-global-context X))
         (U (car context))
         (S (cdr context)))
    (((expression-meaning-toplevel E) U K) S)))


;; repl - Invoke the DS Interpreter's REPL
(define (repl)
  (call-with-current-continuation
   (lambda (c)
     (eval-expression bootstrap-source
                      c
                      ;; exit continuation
                      (lambda (e k) (c e))))))


;; The following are defined in the host Scheme environment, to
;; support use by ds:wrong Note LAML can't handle global names
;; with characters that aren't allowed in filenames, like * and <.
;; Wanted to use *current-failure-continuation* below.
(define failure-continuation-value #f)

(define current-failure-continuation
  (lambda (e k)
    (lambda (s)
      (if (= (ds:length e) 1)
          (set! failure-continuation-value (ds:first e)))
      ((ds:send (ds:sequence failure-continuation-value) k) s))))

(define ds:wrong
  (lambda (x)
    (display x)  ; allows display before store is supplied, in
                 ; case of mismatches
    (newline)
    (lambda (s)
      (((ds:applicate
         failure-continuation-value
         (ds:sequence))
        #f) s)))) ;; #f is for unused continuation

;; Called from non-DS procedures when execution context can't
;; easily support ds:wrong. todo: improve error handling logic
(define ds:wrong-wrong
  (lambda (x)
    (display x) ; allows display before store is supplied, in case
                ; of mismatches
    (newline)
    (lambda (s)
      (display "This never executes - no store supplied.")
      (((ds:applicate failure-continuation-value (ds:sequence)) #f) s))
    ;; return uninitialized location; bogus, since not associated
    ;; with an address
    (ds:sequence ds:unspecified ds:false)))


;; The following is the first code evaluated by the interpreter
;; itself. This code defines the interpreter's repl, the 'load'
;; procedure, and then loads subsequent definitions from
;; prelude.scm.
(define bootstrap-source
  '(begin
     ;; a parameterizerable repl - see chatty-repl in prelude.scm
     ;; for example of use
     (define (main-repl port prompt-proc display-proc)
       (let reploop ()
         (if prompt-proc
             (begin
               (newline)
               (prompt-proc)))
         (let ((p (read port)))
           (if (eof-object? p)
               #f   ; todo: use level-sensitive exit continuation?
               (begin
                 (call-with-current-continuation
                  (lambda (c)
                    (current-failure-continuation c)
                    (display-proc
                     (eval p
                           (interaction-environment)))))
                 (reploop))))))

     (define (load filename)
       (let ((port (open-input-file filename)))
         (main-repl port #f (lambda (x) #f))
         (close-input-port port)
         #t))

     (load "prelude.scm")

     (chatty-repl (current-input-port))))
