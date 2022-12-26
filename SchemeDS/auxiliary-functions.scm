;;; Definitions for the auxiliary functions from the R5RS DS, Section 7.2.4
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


;; lookup : U -> Ide -> L
(define ds:lookup (lambda (r I) (r I)))

;; extends : U -> Ide* -> L* -> U
(define ds:extends 
  (lambda (r I* a*)
    (if (= (ds:length I*) 0)
        r
        (ds:extends (ds:substitute r (ds:first a*) (ds:first I*))
                    (ds:rest I*) 
                    (ds:rest a*)))))

;; wrong : X -> C
;; defined in dsrepl.scm

;; send : E -> K -> C
(define ds:send (lambda (e k) (k (ds:sequence e))))

;; single : (E -> C) -> K
;; checks for a single return value and extracts it from sequence
(define ds:single
  (lambda (psi)
    (lambda (e*)
      (if (= (ds:length e*) 1)
          (psi (ds:first e*))
          (ds:wrong "wrong number of return values")))))

;; new : S -> (L + {error})    [implementation-dependent]
(define ds:new ds:new-impl) 

;; hold : L -> K -> C
(define ds:hold
  (lambda (a k)
    (lambda (s)
      ((ds:send (ds:first (s a)) k) s))))

;; assign : L -> E -> C -> C
(define ds:assign
  (lambda (a e theta)
    (lambda (s)
      (theta (ds:update a e s)))))

;; update : L -> E -> S -> S
(define ds:update
  (lambda (a e s)
    (ds:substitute-location
     s
     (ds:sequence e ds:true)
     a)))

;; tievals : (L* -> C) -> E* -> C
(define ds:tievals 
  (lambda (psi e*)
    (lambda (s)
      (if (= (ds:length e*) 0)
          ((psi (ds:sequence)) s)
          (if (ds:location? (ds:new s))
              ((ds:tievals (lambda (a*) (psi (ds:append (ds:sequence (ds:project-location (ds:new s))) a*)))
                           (ds:rest e*))
               (ds:update (ds:project-location (ds:new s)) (ds:first e*) s))
              ((ds:wrong "out of memory") s))))))
  
;; tievalsrest : (L* -> C) -> E* -> N -> C
(define ds:tievalsrest
  (lambda (psi e* n)
    (ds:list (ds:dropfirst e* n)
             (ds:single 
              (lambda (e) 
                (ds:tievals psi (ds:append (ds:takefirst e* n) (ds:sequence e))))))))

(define ds:dropfirst
  (lambda (l n)
    (if (= n 0)
        l
        (ds:dropfirst (ds:rest l) (- n 1)))))

(define ds:takefirst
  (lambda (l n)
    (if (= n 0)
        (ds:sequence)
        (ds:append (ds:sequence (ds:first l))
                   (ds:takefirst (ds:rest l) (- n 1))))))

;; truish : E -> T
(define ds:truish
  (lambda (e)
    (if (eq? e ds:false)
        ds:false
        ds:true)))

;; permute : Exp* -> Exp*       [implementation-dependent]
(define ds:permute ds:identity)

;; unpermute : E* -> E*         [inverse of permute]
(define ds:unpermute ds:identity)

;; applicate : E -> E* -> K -> C
(define ds:applicate
  (lambda (e e*)
    (lambda (k)
      (if (ds:procedure? e)
          ((ds:project-procedure (ds:second e)) e* k)
          (ds:wrong "bad procedure")))))

;; onearg : (E -> K -> C) -> (E* -> K -> C)
(define ds:onearg 
  (lambda (z)
    (lambda (e* k)
      (if (= (ds:length e*) 1)
          (z (ds:first e*) k)
          (ds:wrong "wrong number of arguments")))))

;; twoarg : (E -> E -> K -> C) -> (E* -> K -> C)
(define ds:twoarg
  (lambda (z)
    (lambda (e* k)
      (if (= (ds:length e*) 2)
          (z (ds:first e*) (ds:second e*) k)
          (ds:wrong "wrong number of arguments")))))
  
;; list : E* -> K -> C
(define ds:list
  (lambda (e* k)
    (if (= (ds:length e*) 0)
        (ds:send ds:null k)
        (ds:list (ds:rest e*) 
                 (ds:single 
                  (lambda (e) 
                    (ds:cons (ds:sequence (ds:first e*) e) 
                             k)))))))

;; cons : E* -> K -> C
(define ds:cons
  (ds:twoarg 
   (lambda (e1 e2 k)
     (lambda (s)
       (if (ds:location? (ds:new s))
           ((lambda (s-prime)
              (if (ds:location? (ds:new s-prime))
                  ((ds:send (ds:inject-value 
                             (ds:sequence (ds:project-location (ds:new s))
                                          (ds:project-location (ds:new s-prime))
                                          ds:true))
                            k)
                   (ds:update (ds:project-location (ds:new s-prime)) e2 s-prime))
                  (ds:wrong "out of memory" s-prime)))
            (ds:update (ds:project-location (ds:new s)) e1 s))
           ((ds:wrong "out of memory") s))))))

;; less : E* -> K -> C
(define ds:less
  (ds:twoarg
    (lambda (e1 e2 k)
      (if (and (ds:number? e1) (ds:number? e2))
          (ds:send (if (< (ds:project-number e1) (ds:project-number e2)) #t #f) k)
	  (ds:wrong "non-numeric argument to <")))))

;; add : E* -> K -> C
(define ds:add
  (ds:twoarg
    (lambda (e1 e2 k)
      (if (and (ds:number? e1) (ds:number? e2))
          (ds:send (ds:inject-value (+ (ds:project-number e1) (ds:project-number e2))) k)
	  (ds:wrong "non-numeric argument to +")))))

;; car : E* -> K -> C
(define ds:car
  (ds:onearg
    (lambda (e k)
      (if (ds:pair? e)
          (ds:hold (ds:first (ds:project-pair e)) k)
	  (ds:wrong "non-pair argument to 'car'")))))
	  
;; cdr : E* -> K -> C
(define ds:cdr
  (ds:onearg
    (lambda (e k)
      (if (ds:pair? e)
          (ds:hold (ds:second (ds:project-pair e)) k)
	  (ds:wrong "non-pair argument to 'cdr'")))))

;; setcar : E* -> K -> C
(define ds:setcar
  (ds:twoarg 
    (lambda (e1 e2 k)
      (if (ds:pair? e1)
          (if (ds:third (ds:project-pair e1))
	      (ds:assign (ds:first (ds:project-pair e1)) e2 (ds:send ds:unspecified k))
	      (ds:wrong "immutable argument to 'set-car!'"))
	  (ds:wrong "non-pair argument to 'set-car!'")))))

;; E* -> K -> C
(define ds:eqv
  (ds:twoarg (lambda (e1 e2 k)
    (if (and (ds:misc? e1) (ds:misc? e2))
        (ds:send (if (eq? (ds:project-misc e1) (ds:project-misc e2))
                     ds:true ds:false)
                 k)
    (if (and (ds:symbol? e1) (ds:symbol? e2))
        (ds:send (if (eq? (ds:project-symbol e1) (ds:project-symbol e2)) 
                     ds:true ds:false)
                 k)
    (if (and (ds:char? e1) (ds:char? e2))
        (ds:send (if (eq? (ds:project-char e1) (ds:project-char e2)) 
                     ds:true ds:false)
                 k)
    (if (and (ds:number? e1) (ds:number? e2))
        (ds:send (if (eq? (ds:project-number e1) (ds:project-number e2)) 
                     ds:true ds:false)
                 k)
    (if (and (ds:pair? e1) (ds:pair? e2))
        (ds:send ((lambda (p1 p2) 
                    (if (and (ds:location-eq? (ds:first p1) (ds:first p2))
                             (ds:location-eq? (ds:second p1) (ds:second p2)))
                        ds:true
                        ds:false))
                  (ds:project-pair e1)
                  (ds:project-pair e2))
                k)
    ;; R5RS includes a vector type test with no action, i.e.:
    ;; (if (and (ds:vector? e1) (ds:vector? e2))
    ;;     ...)
    ;;
    ;; R5RS similarly leaves string comparison undefined, 
    ;; but we want it for completeness:
    (if (and (ds:string? e1) (ds:string? e2))
        (ds:send (if (eq? (ds:project-string e1) (ds:project-string e2))
                     ds:true ds:false)
                 k)
    (if (and (ds:procedure? e1) (ds:procedure? e2))
        (ds:send (if (ds:location-eq? (ds:first (ds:project-procedure e1))
                                      (ds:first (ds:project-procedure e2)))
                     ds:true ds:false)
                 k)
        (ds:send ds:false k)))))))))))

;; apply : E* -> K -> C
(define ds:apply
  (ds:twoarg
    (lambda (e1 e2 k)
      (if (ds:procedure? e1)
          (ds:valueslist (ds:sequence e2)
	                 (lambda (e*) 
			   ((ds:applicate e1 e*) k)))
	  (ds:wrong "bad procedure argument to apply")))))

;; valueslist : E* -> K -> C
(define ds:valueslist
  (ds:onearg 
    (lambda (e k)
      (if (ds:pair? e)
          (ds:cdr (ds:sequence e)
                  (lambda (e*)
                    (ds:valueslist e*
        	      (lambda (e*)
                	(ds:car (ds:sequence e)
                        	(ds:single 
				  (lambda (e)
				    (k (ds:append (ds:sequence e)
                                        	      e*)))))))))
          (if (eq? e ds:null)
	      (k (ds:sequence))
	      (ds:wrong "non-list argument to values-list"))))))

;; cwcc : E* -> K -> C
(define ds:cwcc
  (ds:onearg
    (lambda (e k)
      (if (ds:procedure? e)
          (lambda (s)
	    (if (ds:location? (ds:new s))
	        (((ds:applicate e
                                (ds:sequence 
			         (ds:inject-value
                                  (ds:sequence (ds:project-location (ds:new s))
				               (lambda (e* k-prime)
					         (k e*))))))
                  k)
                 (ds:update (ds:project-location (ds:new s))
			    ds:unspecified
                            s))
	        (ds:wrong "out of memory" s)))
          (ds:wrong "bad procedure argument")))))

;; values : E* -> K -> C
(define ds:values
  (lambda (e* k) (k e*)))

;; cwv : E* -> K -> C
(define ds:cwv
  (ds:twoarg
    (lambda (e1 e2 k)
      ((ds:applicate e1 (ds:sequence))
       (lambda (e*) 
         ((ds:applicate e2 e*) k))))))
         ;; final k above was missing in R5RS DS (typo)
