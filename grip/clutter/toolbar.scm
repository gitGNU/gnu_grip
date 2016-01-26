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

;;       <- stands for ->
;; clue                   clutter example
;; clues                  clutter examples set
;; clus                   clutter support

;; This code needs review, virtual slots for clutter ones, add-child...
;; But it won't be as good as it could until goops is fixed.

;;; Code:


(define-module (grip clutter toolbar)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome gw gdk) ;; gdk-pixbuf
  #:use-module (gnome clutter)
  #:use-module (grip reexport)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip utils)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter gravity)
  #:use-module (grip clutter orientation)
  #:use-module (grip clutter actor)
  #:use-module (grip clutter image)
  #:use-module (grip clutter text)
  #:use-module (grip clutter label)
  #:use-module (grip clutter stage)
  #:use-module (grip clutter apple)
  #:use-module (grip clutter box)
  #:use-module (grip clutter toolbar-item)
  #:use-module (grip clutter tooltip)

  #:export (<clus-toolbar>))


(g-export !items

	  set-gravity
	  !gravity)


(eval-when (expand load eval)
  (re-export-public-interface (grip clutter toolbar-item)
			      (grip clutter tooltip)))


;;;
;;; Toolbar
;;;

(define-class <clus-toolbar> (<clus-box>)
  (items #:accessor !items #:init-keyword #:items #:init-value #f))

(define-method (initialize (self <clus-toolbar>) initargs)
  #;(dimfi "initialize <clus-toolbar>" initargs)
  (next-method)
  (set-spacing self 2)
  (set-homogeneous self #t)
  (do ((i 0 (+ i 1))
       (items (!items self) (cdr items)))
      ((null? items)
       (set-reactive self #t))
    (add-child self (car items)))
  #;(connect toolbar
	     'enter-event
	     (lambda (a e)
	       (save-easing-state a)
	       (set-opacity a 128)
	       (restore-easing-state a)
	       #f)) ;; yes, please propagate the event
    #;(connect toolbar
	     'leave-event
	     (lambda (a e)
	       (save-easing-state a)
	       (set-opacity a 64)
	       (restore-easing-state a)
	       #f))) ;; do not stop the event propagation


(define-method (set-gravity (self <clus-toolbar>) gravity)
  ;; (dimfi "setter !gravity <clus-toolbar>" self gravity)
  (next-method)
  (for-each (lambda (toolbar-item)
	      (ensure-tooltips-on-stage toolbar-item))
    (!items self)))
