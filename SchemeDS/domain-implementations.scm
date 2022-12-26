;;; Implementation-specific procedures
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


(define ds:identity (lambda (x) x))


;;; E, the domain of expressed values

(define ds:inject-value ds:identity)


;;; Sequence interface
;;;
;;; These procedures implement the 'sequence' data type used by the semantics.
;;; Currently implemented simply as lists in host Scheme.

(define ds:append append)
(define ds:first car)
(define ds:length length)
(define ds:rest cdr)
(define ds:second cadr)
(define ds:sequence list)
(define ds:third caddr)
(define ds:sequence? pair?)


;;; M, the domain of miscellaneous values
;;;
;;; Members are {false, true, null, undefined, unspecified}

(define ds:false #f)
(define ds:true #t)
(define ds:undefined '*undefined*)
(define ds:unspecified '*unspecified*)

;; The value of '() is required for the auxiliary function 'list' to 
;; work correctly, given our direct mapping of types to the host Scheme.
(define ds:null '())

(define ds:misc-domain 
  (list ds:false ds:true ds:null ds:undefined ds:unspecified))

(define ds:misc?
  (lambda (m)
    (if (member m ds:misc-domain)
        #t
        #f)))

(define ds:project-misc ds:identity)


;;; L, the domain of locations
;;; 
;;; Assumes all integers are locations, but not all in use

(define ds:location? integer?)
(define ds:location-eq? =)
(define ds:project-location ds:identity)


;;; F, the domain of procedure values

(define ds:project-procedure ds:identity)

(define ds:procedure?
  (lambda (e)
    (if (and (ds:sequence? e)             ;; required guard
             (= (ds:length e) 2)          ;; overcautious
             (ds:location? (ds:first e))  ;; really paranoid
             (procedure? (ds:second e)))  ;; the real test
        #t
        #f)))


;;; R, the domain of numbers

(define ds:number? number?)
(define ds:project-number ds:identity)

;;; Ep, the domain of pairs.

;; Pairs are sequences of <LxLxT>.  Following is a complete test, which is 
;; overkill in the current implementation - simply testing length would 
;; be sufficient.
;;
;; However, implementation of a vector type would change this.  Given the
;; current naive implementation of locations, for example, a two-element 
;; vector might look like a pair.
(define (ds:pair? e)
  (if (and (ds:sequence? e)
  	   (= (ds:length e) 3)
           (ds:location? (ds:first e))
           (ds:location? (ds:second e))
           (boolean? (ds:third e)))
      #t
      #f))

(define ds:project-pair ds:identity)


;;; Q, the domain of symbols

(define ds:symbol? symbol?)
(define ds:project-symbol ds:identity)


;;; Es, the domain of strings 

(define ds:string? string?)
(define ds:project-string ds:identity)


;;; H, the domain of characters

(define ds:char? char?)
(define ds:project-char ds:identity)


;;; S, the domain of stores
;;;
;;; todo: move to own file to support pluggability.  
;;; Closely coupled to location implementation.

;; new : S -> (L + {error})
;;
;; No allowance for error here - the supply of natural numbers is fairly extensive...
;;
;; Borrowed from Queinnec.  Note that the use of (s 0) means that every newly 
;; allocated location causes the previous value of (s 0) to be replaced, resulting 
;; in a store procedure with a call chain double the length it might otherwise be, 
;; which has cost on every access to the store.  See below for Clinger's alternative.
(define ds:new-impl
  (lambda (s)
    (+ 1 (s 0))))

;; Will Clinger posted the following implementation of 'new' on c.l.s.
;; By looping through entire store to find an unallocated location, it avoids 
;; the issue mentioned above - performance cost is shifted from every store 
;; access, to allocation time.  Might be interesting to measure the performance 
;; difference...
;;
;;    (define (new s)
;;      (define (loop s a)
;;        (if (second (s a))
;;            (loop s (+ a 1))
;;            a))
;;      (loop s 0))

;; expand-store : S -> L -> S
;;
;; Store new largest memory address in location zero
;; and implement unused locations
;;
;; Adapted from Queinnec
(define (ds:expand-store s a-max)
  (ds:substitute s a-max 0))

;; initial-store implements a store for which all integers are 
;; valid locations, but are initially flagged as not in use.
;;
;; Adapted from Queinnec
(define (ds:initial-store)
  (ds:expand-store (lambda (a)
                     (if (ds:location? a)
                         (ds:sequence ds:unspecified ds:false)
                         (ds:wrong-wrong "Invalid location")))
                   0))

;; Implements substitution operation - f[v/s]
;;
;; f:function; v:value; s:selector
;; from R5RS 'notation': r[x/i] - substitution r with x for i
;;
;; Used by:
;;   update: s[e/a]
;;   extends: r[a/ide]
;;
;; Relies on eqv? being appropriate for all argument types
(define ds:substitute
  (lambda (f v s)
    (lambda (s1)
      (if (eqv? s s1) v (f s1)))))

;; Implements substitution for location/value bindings.
;; If the specified location is unallocated, the store is 
;; expanded via ds:expand-store.
;;
;; Corresponds to s[e/a] in the DS
;;
;; Note that the sequence of arguments here is determined 
;; by the order required from the output of the L2T tool.
(define ds:substitute-location
  (lambda (s e a)
    (ds:substitute
     (if (ds:location-eq? a (ds:new-impl s))
         (ds:expand-store s a)
         s)
     e
     a)))
