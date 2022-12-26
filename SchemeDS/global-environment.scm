;;; Definitions to support a global environment.
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


;;;	Uses mutation.
;;;
;;;	semantic/dispatch functions:
;;;p		expression-meaning-toplevel-definition
;;;p		expression-meaning-procedure-definition
;;;
;;;	global environment definition:
;;;		base-environment
;;;		global-environment
;;;		initialize-global-context
;;;		global-environment-lookup
;;;		dse:initial-environment
;;;g		dss:interaction-environment
;;;
;;;	global environment initialization:
;;;		extend-global-environment!
;;;		define-vars
;;;		define-global-var

(define dss:interaction-environment
  (lambda (e k)
    ; todo: validate (= e zero-length)
    (ds:send dse:initial-environment k)))

;; Initial environment data.  This is not an environment itself - it is 
;; used to initialize the environment.  Should be organized by type of 
;; procedure, to support R5RS standard environments.  todo.
(define (base-environment)

  ; r5rs-ds-environment: only what ds provides
  ;
  ; quasiquote use avoided because of laml
  (list 
    (cons 'cons ds:cons)
    (cons 'car ds:car)
    (cons 'cdr ds:cdr)
    (cons 'list ds:list)
    (cons 'eqv? ds:eqv)
    (cons 'eq? ds:eqv)         ; finer distinctions not important for DS purposes
    (cons 'set-car! ds:setcar)
    (cons 'set-cdr! ds:setcdr)
    (cons '+ ds:add)
    (cons '< ds:less)
    (cons 'apply ds:apply)
    (cons 'values ds:values)
    (cons 'call-with-current-continuation ds:cwcc)
    (cons 'call-with-values ds:cwv)

    ; following are standard R5RS operations, not defined in the DS.

    (cons 'boolean? dss:boolean?)
    (cons 'number? dss:number?)
    (cons 'symbol? dss:symbol?)
    (cons 'string? dss:string?)
    (cons 'char? dss:char?)
    (cons 'procedure? dss:procedure?)
    (cons 'pair? dss:pair?)
    (cons 'port? dss:port?)
    ; TODO: map other existing type predicates
    (cons '> dss:greater)
    (cons '- dss:subtract)
    (cons '* dss:multiply)
    (cons '= dss:numequals)
    (cons 'eval dss:eval)
    (cons 'open-input-file dss:open-input-file)
    (cons 'current-input-port  dss:current-input-port)
    (cons 'close-input-port dss:close-input-port)
    (cons 'eof-object? dss:eof-object?)

    ; r5rs library procedures

    (cons 'read dss:read)
    (cons 'display dss:display)
    (cons 'write dss:write)
    (cons 'newline dss:newline)

    ; r5rs optional procedures

    (cons 'interaction-environment dss:interaction-environment)

    ; extensions

    (cons 'current-failure-continuation current-failure-continuation)
    (cons 'dump-store dsi:dump-store)
    (cons 'store-size dsi:store-size)))

;; the actual global environment
(define global-environment '())

;; initialize-context
;;
;; initializes environment and store, and returns pair: (U . S)
;;
;; Exit-continuation determines handling of (exit) and (exit errorlevel).
(define (initialize-global-context exit-continuation)
  (set! global-environment '())
  (define-vars (cons dse:initial-environment (ds:initial-store))
               (cons (cons 'exit exit-continuation)
    		     (base-environment))))

;; global-environment-lookup : I -> L
;;
;; Global definitions mutate the global environment.
(define global-environment-lookup
  (lambda (I)
    (let ((p (assq I global-environment)))
      (if p (cdr p) #f))))

;; initial-environment : I -> L
;;
;; Local environment lookups delegate to the global environment via this function.
(define (dse:initial-environment-global I)
  (let ((a (global-environment-lookup I)))
    (if a 
        a
        (ds:wrong-wrong (string-append "undefined identifier: '" (symbol->string I) "'. " )))))


;; expression-meaning-toplevel-definition 
;; 
;; Meaning of 'define' for global environment, i.e. top-level definitions.  Uses mutation.
;; todo: validate against other meaning functions
(define (expression-meaning-toplevel-definition I E*)
  (lambda (r k)
    (lambda (s)
      (let* ((a (global-environment-lookup I))
             (s-prime 
               (if (not a)
                   ; Mutate global-environment - add (I . L) pair
                   (if (ds:location? (ds:new s))
                     (begin
                       (set! global-environment (cons (cons I (ds:new s)) global-environment))
                       ;; have to allocate the location, otherwise evaluation of expression in assignment could
                       ;; use same location.  Might optimize this by not using expression-meaning-assignment.
                       (ds:update (ds:project-location (ds:new s)) #f s))
                     ((ds:wrong "out of memory") s))
                   s)))
        (((expression-meaning-assignment I E*)
          r
          k)
         s-prime)))))

(define (expression-meaning-procedure-definition sig E*)
  (expression-meaning-toplevel-definition (car sig) 
                                          (cons 'lambda 
                                                (cons (cdr sig) E*))))

;;; The following procedures are used during initialization.
;;; They may duplicate DS-level functions to some extent.

;; Define variables specified in name-value-pairlist, in environment and store 
;; specified by context-pair.  Uses specified defining-proc to support other 
;; environment implementations (e.g. no global environment).
;;
;; context-pair: (U . S)
;; name-value-pairlist: ((I . v)...)
(define (define-vars context-pair name-value-pairlist)
  (if (null? name-value-pairlist)
      context-pair
      (define-vars (dse:global-definer context-pair (car name-value-pairlist))
                   (cdr name-value-pairlist))))


(define (extend-global-environment! I L)
  (set! global-environment (cons (cons I L) global-environment)))

;; Define variable in global environment
;;
;; Does not extend provided environment - global environment is mutated instead.
;;
;; context-pair: (U . S)
;; name-value-pair: (I . v)
;;
;; TODO! The second use of 'new' below doesn't conform to the DS behavior, in that it 
;; uses the location of the variable holding the procedure as the procedure's unique 
;; identifier.  This has the potential for failure in some real situations, but isn't 
;; a problem in the current interpreter, especially since this function is only used
;; during bootstrapping.
;;
;; TODO! This procedure is specific to procedure values, and can't define other types
;; at present, due to use of (L x prox) format below.
;;
;; todo: check ds kosherness
;; todo: handle allocation failure
(define (define-global-var context-pair name-value-pair)
  (let* ((r (car context-pair))
         (s (cdr context-pair))
         (I (car name-value-pair))
         (v (cdr name-value-pair))
         (L (ds:new s)))
    (extend-global-environment! I L)
    (cons r 
          (ds:update L
                     (ds:inject-value (ds:sequence 
                                       (ds:project-location (ds:new s))  
                                       v))
                     s))))

;; following were to support pluggable environments; not fully implemented
(define dse:initial-environment dse:initial-environment-global)
(define dse:global-definer define-global-var)
