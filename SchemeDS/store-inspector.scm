;;; store-inspector.scm
;;;
;;; Procedures to support inspection of stores
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


(define (dump-store s start)
  (display "  ")(display start)(display ": ")(display (s start))(newline)
  (let ((size (s 0))
        (next (+ 1 start)))
    (if (<= next size)
        (dump-store s next))))

(define dsi:dump-store
  (lambda (e k)
    (lambda (s)
      (dump-store s 
                  (if (zero? (ds:length e)) 
                             0 
                             (ds:first e)))
      ((ds:send (ds:sequence) k) s))))

(define dsi:store-size
  (lambda (e k)
    (lambda (s)
      ((ds:send (s 0) k) s))))

