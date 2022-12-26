;;; value-conversion.scm
;;; 
;;; Procedures for converting between DS values and host Scheme values.
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


;; Convert an expressed value E to a value in host Scheme
;;
;; E -> host value
(define (ds:value->host-value e s)
  (if (ds:pair? e)
      (ds:pair->host-pair e s)
      e))

;;; following are local

(define (ds:pair->host-pair e s)
  (let ((n (ds:length e)))
    (if (ds:pair? e)
        ;;; ds pair is sequence of LxLxT
        (cons (ds:location-value->host-value (s (ds:first e)) s)
              (ds:location-value->host-value (s (ds:second e)) s))
        (if (ds:procedure? e)
            ;; ds procedure is sequence of (L x proc) - L uniquely identifies a procedure
            ;; todo: insert own "Procedure:" tag for consistency across host Schemes?
            e
            ((ds:wrong "internal error: invalid pair format!") s)))))

;; Converts the ExT value bound to a location, to a value in host Scheme
;; 
;; (E x T) -> host value, where stores S = L -> (E x T)
(define (ds:location-value->host-value e s)
  (if (eq? (ds:second e) ds:true)
      (ds:value->host-value (ds:first e) s)
      ((ds:wrong "internal error: access to unused location") s)))

;; Convert a host value into an expressed value in a store.
;;
;; The cons-proc parameter specifies the cons procedure to be used.
;; This supports creation of immutable pairs.  See ds:immutable-cons.
(define ds:host-value->value
  (lambda (e* cons-proc k)
    ;; inner loop to prevent passing cons-proc around unnecessarily
    (let loop ((e* e*)
               (k k))
      (if (pair? e*)
          (loop (ds:first e*) 
                (ds:single
                 (lambda (e1)
                   (loop (ds:rest e*) 
                         (ds:single
                          (lambda (e2)
                            (cons-proc (ds:sequence e1 e2) k)))))))
          (ds:send e* k)))))
          ; todo: use (constant-meaning e*) above (which is a no-op currently)?
