;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2016
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

;;; Code:


(define-module (grip gnome filechooser)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (prompt-for-filename))


(define (prompt-for-filename parent title mode folder fname)
  (let ((w (make <gtk-file-chooser-dialog>
             #:action mode
	     #:title title
	     #:destroy-with-parent #t)))
    (for-each
	(lambda (id)
	  (add-button w
		      (gtk-stock-id id)
		      (genum->value (make <gtk-response-type> #:value id))))
	'(cancel ok))
    (if parent (set-transient-for w parent))
    (if folder (set-current-folder w folder))
    (if fname (set-current-name w fname))
    (show w)
    (catch 'any
	   (lambda ()
	     (let* ((response (genum->symbol
			       (make <gtk-response-type> #:value (run w))))
		    (filename (get-filename w)))
	       ;; (format #t "Response: ~S~%" response)
	       (destroy w)
	       (case response
		 ((ok) (throw 'any filename))
		 ((cancel delete-event) (throw 'any #f)))))
	   (lambda (key value)
	     value))))


#!

(prompt-for-filename #f
		     "Connect to ..."
		     ;; 'create-folder
		     ;; 'open
		     'save
		     "/tmp"
		     "sqlite.alto.db")

!#
