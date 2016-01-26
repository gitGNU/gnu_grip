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


(define-module (grip gnome key-press)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)

  #:export (get-key-press-infos
	    display-key-press-infos))


(define (get-key-press-infos window event)
  (let ((has-focus (get-focus window))
	(key-val (gdk-event-key:keyval event)))
    (values has-focus key-val (gdk-keyval-name key-val))))

(define* (display-key-press-infos window event #:key (port #f))
  (receive (has-focus key-val key-name)
      (get-key-press-infos window event)
    (format (if port port (current-output-port))
	    "Key press, focus: ~S value: ~A, name: ~A~%"
	    has-focus key-val key-name)))
