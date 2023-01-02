;;; repl.scm
;;;
;;; Loads the necessary modules to implement a full REPL based on the DS core.
;;; Handles dependency issues for compilers sensitive to such things.
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


;; forward declarations for domain-implementations.scm
(define ds:wrong #f)
(define ds:wrong-wrong #f)

(load "domain-implementations.scm")
(load "auxiliary-functions.scm")

;; forwards for semantic-functions.scm
(define constant-meaning #f)
(define expression-sequence-meaning #f)
(define expression-meaning #f)
(define command-sequence-meaning #f)

(load "semantic-functions.scm")
(load "syntax-support.scm")
(load "semantic-dispatch.scm")

(load "value-conversion.scm")
(load "library-procedures.scm")
(load "derived-expressions.scm")

;; forwards for global-environment.scm
(define expression-meaning-toplevel #f)
(define current-failure-continuation #f)

(load "global-environment.scm")
(load "toplevel-dispatch.scm")
(load "interpreter-repl.scm")

;; optional stuff
(load "store-inspector.scm")

(newline)
(print "hello?")
(load "factorial.scm")
(newline)
(print (factorial 12))
(newline)


;; run the REPL
(print "about to run DS repl")
(repl)
(print "done running DS repl")
