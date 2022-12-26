;;; library-procedures
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


;;; This module snarfs various important Scheme library procedures from the host Scheme.
;;; Any procedures defined here must be bound in the environment before being usable 
;;; from within the language.  This is currently done in global-environment.scm.
;;;
;;; Further snarfing could be done more efficiently with the use of macros.
;;; However, the resulting impact on portability across Scheme implementation 
;;; should be checked.
;;;
;;; The procedures below work, but fall short of being canonical in terms of their use
;;; of the project & inject functions, checking of parameter count, etc.  Also, the 
;;; return value of snarfed procedures should be converted to avoid exposing host values.

(define dss:eval
  (ds:twoarg
    (lambda (e r-prime k)
      (lambda (s)
        ;; e is converted below to a host scheme sexp for evaluation at toplevel
        (let* ((M (expression-meaning-toplevel (ds:value->host-value e s))))
          ((M r-prime k) s))))))

;; setcar : E* -> K -> C
(define ds:setcdr
  (ds:twoarg 
    (lambda (e1 e2 k)
      (if (ds:pair? e1)
          (if (ds:third (ds:project-pair e1))
	      (ds:assign (ds:second (ds:project-pair e1)) e2 (ds:send ds:unspecified k))
	      (ds:wrong "immutable argument to 'set-cdr!'"))
	  (ds:wrong "non-pair argument to 'set-cdr!'")))))

;; greater : E* -> K -> C
;; based on ds:less and thus limited to two args
(define dss:greater
  (ds:twoarg
    (lambda (e1 e2 k)
      (if (and (ds:number? e1) (ds:number? e2))
          (ds:send (if (> (ds:project-number e1) (ds:project-number e2)) #t #f) k)
	  (ds:wrong "non-numeric argument to >")))))

;; numequals : E* -> K -> C
;; based on ds:less and thus limited to two args
(define dss:numequals
  (ds:twoarg
    (lambda (e1 e2 k)
      (if (and (ds:number? e1) (ds:number? e2))
          (ds:send (if (= (ds:project-number e1) (ds:project-number e2)) #t #f) k)
	  (ds:wrong "non-numeric argument to =")))))

;; subtract : E* -> K -> C
;; based on ds:add and thus limited to two args
(define dss:subtract
  (ds:twoarg
    (lambda (e1 e2 k)
      (if (and (ds:number? e1) (ds:number? e2))
          (ds:send (ds:inject-value (- (ds:project-number e1) (ds:project-number e2))) k)
	  (ds:wrong "non-numeric argument to -")))))

;; multiply : E* -> K -> C
;; based on ds:add and thus limited to two args
(define dss:multiply
  (ds:twoarg
    (lambda (e1 e2 k)
      (if (and (ds:number? e1) (ds:number? e2))
          (ds:send (ds:inject-value (* (ds:project-number e1) (ds:project-number e2))) k)
	  (ds:wrong "non-numeric argument to *")))))

;; read : E -> K -> C
(define dss:read
   (lambda (e* k)
     (ds:host-value->value (apply read e*) ds:cons k)))

;; display : E -> K -> C
;; todo: support version which accepts port
(define dss:display
  (ds:onearg
    (lambda (e k)
      (lambda (s)
        ((ds:send (ds:inject-value (display-value e s)) k) s)))))

(define (display-value e s)
  (display (ds:value->host-value e s)))

(define dss:write
  (ds:onearg
    (lambda (e k)
      (lambda (s)
        ((ds:send (ds:inject-value (write-value e s)) k) s)))))

(define (write-value e s)
  (write (ds:value->host-value e s)))

(define dss:open-input-file
  (ds:onearg
    (lambda (e k)
      (lambda (s)
        ((ds:send (ds:inject-value (open-input-file e)) k) s)))))

(define dss:current-input-port
  (lambda (e k)
    (lambda (s)
      ((ds:send (ds:inject-value (current-input-port)) k) s))))

(define dss:close-input-port
  (ds:onearg
    (lambda (e k)
      (lambda (s)
        ((ds:send (ds:inject-value (close-input-port e)) k) s)))))

(define dss:eof-object?
  (ds:onearg
    (lambda (e k)
      (lambda (s)
        ((ds:send (ds:inject-value (eof-object? e)) k) s)))))

;; snarfing newline, rather than implementing it within the interpreter as
;; simply (display #\newline), prevents its use from cluttering up the store.  
;; todo: support port arg
(define dss:newline
  (lambda (e k)
    (lambda (s)
      ((ds:send (ds:inject-value (newline)) k) s))))

(define dss:procedure?
   (lambda (e k)
     (ds:send (ds:procedure? (ds:first e)) k)))

(define dss:pair?
   (lambda (e k)
     (ds:send (ds:pair? (ds:first e)) k)))

(define dss:number?
   (lambda (e k)
     (ds:send (ds:number? (ds:first e)) k)))

(define dss:symbol?
   (lambda (e k)
     (ds:send (ds:symbol? (ds:first e)) k)))

(define dss:string?
   (lambda (e k)
     (ds:send (ds:string? (ds:first e)) k)))

(define dss:char?
   (lambda (e k)
     (ds:send (ds:char? (ds:first e)) k)))

;; booleans are part of the misc domain M, so there's no ds:boolean? procedure
(define dss:boolean?
   (lambda (e k)
     (ds:send (boolean? (ds:first e)) k)))

(define dss:port?
   (lambda (e k)
     (ds:send (port? (ds:first e)) k)))
