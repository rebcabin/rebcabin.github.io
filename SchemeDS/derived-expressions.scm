;;; derivedexpressions.scm 
;;;
;;; Derived expression transformations
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


;;; It would be convenient to use quasiquote for the syntactic 
;;; transformations in this module, but LAML doesn't support it. 
;;;
;;; todo: instead of #f as initial values below, use 'undefined'.  However,
;;; will then need an expressable representation, even if only internal.
;;;
;;; TODO: let*
;;;
;;; (let* ((a x)
;;;        (b y)
;;;        (c z))
;;;   body
;;;   ...)
;;; 
;;; =>
;;; 
;;; ((lambda (a)
;;;    ((lambda (b)  
;;;       ((lambda (c)
;;;          body) z)) y)) x)

(define (expression-meaning-begin E*)
  (expression-meaning-application (cons 'lambda (cons '() E*)) (ds:sequence)))

;; convert-let-variables - support procedure for 'let' transformations
;; 
;; Convert the variable/value list for the let family of expressions 
;; into a lists of names and values.  Returns (names . values).
;;
;; Note this reverses the order of both the variables and values.  This
;; maintains correlation, so is mostly acceptable for let & letrec. However, 
;; named let and let* require evaluation sequence to be respected, and must 
;; use reverse to obtain desired correct sequence.
(define (convert-let-variables V*)
  (let loop ((names '())
             (values '())
             (V* V*))             
    (if (null? V*)
        (cons names values)
        (loop (cons (caar V*) names)
              (cons (cadar V*) values)
              (cdr V*)))))


;; Transform plain 'let' 
;;
;; (let ((a x)
;;       (b y)
;;       (c z)) 
;;   body1 
;;   body2
;;   ...)
;;
;; => 
;;
;; ((lambda (a b c))
;;    body1 
;;    body2
;;    ...)
;;  x y z)
;;
;; Applies argument values to appropriately constructed lambda expression
(define (expression-meaning-let V* E*)
  (let* ((names-values (convert-let-variables V*))
         (names (car names-values))
         (values (cdr names-values)))
    (expression-meaning-application 
     (cons 'lambda (cons names E*))
     values)))


;; Transform named 'let'
;; 
;; (let name ((names values)...)
;;    expr
;;    ...)
;; 
;; =>
;;
;; ((lambda (name) 
;;    (set! name 
;;      (lambda (names...)
;;        expr
;;        ...))
;;    (name values...))
;;  ds:undefined)
(define (expression-meaning-named-let name V* E*)
  (let* ((params-values (convert-let-variables V*))
         (params (reverse (car params-values)))
         (values (reverse (cdr params-values))))
    (expression-meaning-application 
     (list 'lambda (list name) 
        (list 'set! name
          (cons 'lambda (cons params E*)))
        (cons name values))
     '(#f))))        


;; Transform 'letrec':
;; 
;; (letrec ((a x)
;;          (b y))
;;    body
;;    ...)
;;    
;; =>
;;
;; ((lambda (a b)
;;    (set! a x)
;;    (set! b y)
;;    body)
;;  <undefined>...)
;;
;; TODO: R5RS requires something more like:
;;
;; ((lambda (a b)
;;    (let ((a-temp x)
;;          (b-temp y))
;;      (set! a a-temp)
;;      (set! b b-temp))
;;    body)
;;  <undefined>...)
;; 
;;
(define (transform-letrec names values e*)
  (cons 
   (cons 'lambda
         (cons names 
               (append (reverse (map (lambda (I v) (list 'set! I v)) names values))
                       e*)))
   (map (lambda (v) #f) values)))


(define (expression-meaning-letrec V* E*)
  (let* ((names-values (convert-let-variables V*))
         (names (car names-values))
         (values (cdr names-values))
         (expr (transform-letrec names values E*)))
    (expression-meaning-application (car expr) (cdr expr))))
    

;; transform-internal-definitions is called with a lambda expression, 
;; minus the 'lambda', i.e. (([args...]) [cmds...] expr).  The command
;; list may contain zero or more internal definitions.  This procedure
;; converts any such definitions into a letrec-like lambda form.
;; 
;; todo: improve syntax validation: defines with no expr, defines in body
(define (transform-internal-definitions E*)
  (if (< (length E*) 2)
      (malformed-expression 'lambda)
      (let ((param-names (ds:first E*))
            (body-commands (ds:rest E*)))
        (let loop ((commands body-commands)
                   (names '())
                   (values '()))
          (let ((command (ds:first commands)))
            (if (and (pair? command)
                     (eq? (ds:first command) 'define))
                ; it's a define; first check the type
                (let ((sig (ds:second command)))
                  (if (pair? sig)
                      ; it's a procedure definition
                      (loop (ds:rest commands)
                            (cons (ds:first sig) names)
                            (cons (cons 'lambda (cons (ds:rest sig) (cddr command)))
                                  values))
                      ; it's a simple definition
                      (loop (ds:rest commands)
                            (cons sig names)
                            (cons (ds:third command) values))))
                ; got it, return final expression
                (if (null? names)
                    ; no defines, return unaltered
                    E*
                    ; convert to letrec-style lambda, embedded within the current lambda
                    (list param-names (transform-letrec names values commands)))))))))
