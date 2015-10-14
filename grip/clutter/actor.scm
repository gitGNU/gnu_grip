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


(define-module (grip clutter actor)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip utils)
  #:use-module (grip clutter colour)

  #:export (clus-make-actor))


(g-export set-canvas
	  clear-position
	  get-children-named
	  get-child-named)


(define* (clus-make-actor w h #:key (colour #f))
  (let ((rectangle (make <clutter-actor> #:width w #:height h)))
    (when colour
      (set-background-color rectangle
			    (if (string? colour) (get-colour colour) colour)))
    rectangle))

(define-method (set-canvas (self <clutter-actor>) w h draw)
  (let ((canvas (make <clutter-canvas>
		  #:width (inexact->exact w)
		  #:height (inexact->exact h))))
      (set-content self canvas)
      (connect canvas 'draw
	       (lambda (canvas cr w h)
		 (draw self canvas cr w h)
		 #f)) ;; stops the event to be propagated
      (invalidate canvas))) ;; triggers the 'draw signal

(define-method (clear-position (self <clutter-actor>))
  (set-position self 0.0 0.0))

(define-method (get-children-named (self <clutter-actor>) name)
  (filter-map (lambda (child)
		(let ((its-name (get-name child)))
		  (and its-name
		       (string=? its-name name)
		       child)))
      (get-children self)))

(define-method (get-child-named (self <clutter-actor>) name)
  (let ((kids (get-children-named self name)))
    (and kids
	 (when (> (length kids) 1)
	   (dimfi "WARNING: I found several kids named" name))
	 (car kids))))


#!

(do ((i 0
	(+ i 1))
     (signals (get-signals <clutter-actor>)
	      (cdr signals)))
    ((null? signals))
  (display (string-append (number->string i) " "
			  (slot-ref (car signals) 'name) "\n")))

!#
