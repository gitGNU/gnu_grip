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
  #:use-module (grip clutter grid)
  #:use-module (grip clutter toolbar-item)
  #:use-module (grip clutter tooltip)

  #:export (<clus-toolbar>))


(g-export ;; !dummy
	  !items

	  !orientation
	  !gravity
	  !gravity-pole
	  ;; !gravity-force
	  !gravity-resistance-north
	  !gravity-resistance-west
	  !gravity-resistance-mode)

(eval-when (expand load eval)
  (re-export-public-interface (grip clutter toolbar-item)
			      (grip clutter tooltip)))


;;;
;;; Toolbar
;;;

(define-class <clus-toolbar> (<clus-apple>)
  (grid #:getter !grid
	#:allocation #:virtual
	#:slot-ref (lambda (self)
		     (get-first-child self))
	#:slot-set! (lambda (self value)
		      'nothing))
  (grid-layout #:getter !grid-layout
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (get-layout-manager (!grid self)))
	       #:slot-set! (lambda (self value)
			     'nothing))

  (orientation #:accessor !orientation
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (get-orientation (!grid-layout self)))
	       #:slot-set! (lambda (self value)
			     (set! (!orientation self) value)))
  (items #:accessor !items #:init-keyword #:items #:init-value #f))

(define-method (initialize (self <clus-toolbar>) initargs)
  #;(dimfi "initialize <clus-toolbar>" initargs)
  (next-method)
  (let ((items (!items self))
	(grid (!grid self))
	(layout (!grid-layout self))
	(i 0))
    ;; (set! (!row-spacing grid) 2)
    (set! (!column-spacing grid) 4)
    (for-each (lambda (item)
		(attach layout item i 0 1 1)
		(set! i (+ i 1)))
	items)
    (auto-resize self)
    (set-reactive self #t)
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
	       #f)))) ;; yes, please propagate the event

;; the following methods, the ones which just call (next-method),
;; should not be redefined, but for the time being we have to: this is
;; indeed a goops known and yet unresolved bug: getters, setters and
;; accessors are _not_ properly recomputed for subclasses [unlike
;; ordinary methods, which could be a good debug starting point].

(define-method (show-gravity (self <clus-toolbar>) port)
  (next-method))

(define-method ((setter !orientation) (self <clus-toolbar>) orientation)
  (next-method))

(define-method ((setter !gravity) (self <clus-toolbar>) gravity)
  (next-method)
  (for-each (lambda (toolbar-item)
	      (ensure-tooltips-on-stage toolbar-item))
    (!items self)))

(define-method ((setter !gravity-pole) (self <clus-toolbar>) gravity-pole)
  (next-method))

#;(define-method ((setter !gravity-force) (self <clus-toolbar>) gravity-force)
  (next-method))

#;(define-method ((setter !gravity-resistance) (self <clus-toolbar>) grf)
  (next-method))

(define-method ((setter !gravity-resistance-north) (self <clus-toolbar>) grf)
  (next-method))

(define-method ((setter !gravity-resistance-west) (self <clus-toolbar>) grf)
  (next-method))

(define-method ((setter !gravity-resistance-mode) (self <clus-toolbar>) uniform?)
  (next-method))
