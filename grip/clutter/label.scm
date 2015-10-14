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

;;       <- stands for ->
;; clue                   clutter example
;; clues                  clutter examples set
;; clus                   clutter support

;;; Code:


(define-module (grip clutter label)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)
  #:use-module (grip clutter colour)

  #:export (#;clus-get-char-width
	    clus-make-label))


#;(define (clus-get-char-width char font)
  (get-width (make <clutter-text>
	       #:font-name font
	       #:text (string char))))

(define* (clus-make-label text font colour #:optional markup?)
  (let ((l (make <clutter-text>
	     #:font-name font
	     #:text text
	     #:color (if (string? colour)(get-colour colour) colour))))
    (when markup? (set-use-markup l #t))
    (receive (w h)
	(get-size l)
      (values l w h))))
