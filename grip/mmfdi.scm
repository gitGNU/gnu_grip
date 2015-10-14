;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2015
;;;; David Pirotte <david at altosw dot be>

;;;; This file is part of Grip.
;;;; A Grip of Really Important Procedures.

;;;; Grip is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; Grip is distributed in the hope that it will be useful WARRANTY;
;;;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;;;; A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;;; details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with Grip.  If not, see <http://www.gnu.org/licenses/>.
;;;;

;;; Commentary:

;; This code id from Ludovic Courtès, who suggested this approach on irc
;; https://gnunet.org/bot/log/guile/2015-05-15, thanks Ludovic.  Some
;; schemers don't mind deriving identifiers from other identifiers,
;; others do.  Ian Price, on irc that same day, quoted Ryan Culpepper
;; who came up, wrt these "unhigienic" macros, with the term "morally
;; hygienic".

;; Although I personally probably won't use it, I decided to create this
;; small module, as another example [there are some in guile's manual
;; already, of syntax-rules not being sufficient...

;; I also decided to call it mmfdi [make make from derived
;; identifier], because goops defines make [as make-instance]: not only
;; does it suits its function better, but it definitely avoids the
;; potential clash with goops.

;;; Code:


(define-module (grip mmfdi)
  :export (mmfdi))


(define-syntax mmfdi
  (lambda (s)
    (syntax-case s ()
      ((_ type args ...)
       (with-syntax ((m (datum->syntax
			 #'type
			 (symbol-append 'make-
					(syntax->datum #'type)))))
	 #'(m args ...))))))


#!

;; missing good example/mini tests

!#
