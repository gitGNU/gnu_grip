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

;; This needs review

;;; Code:


(define-module (grip clutter drag)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip keyword)
  #:use-module (grip utils)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  
  #:export (<clus-drag>
	    install-drag-action))


(g-export !colour)


;;;
;;; Drag
;;;

(define-class <clus-drag> (<clutter-actor>)
  (colour #:accessor !colour
	  #:init-keyword #:colour
	  #:allocation #:virtual
	  #:slot-ref (lambda (self)
		       (get-background-color self))
	  #:slot-set! (lambda (self value)
			(set-background-color self value))))

(define-method (initialize (self <clus-drag>) initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-drag>)))
    (if (null? virtual-kw)
	(next-method)
	(begin
	  (next-method self kw)
	  (let-keywords virtual-kw #t
			((colour #f))
			(when colour (set! (!colour self) colour)))))
    (set-reactive self #t)
    (set-opacity self 196)
    (install-drag-action self)))

(define (install-drag-action drag)
  (receive (width height)
	(get-size drag)
      (let ((drag-copy (make <clutter-actor>
			 #:width width
			 #:height height
			 #:background-color %tango-chameleon-dark))
	    (drag-action (clutter-drag-action-new)))
	(set-opacity drag-copy 64)
	(hide drag-copy)
	(connect drag
		 'enter-event
		 (lambda (a e)
		   #;(dimfi "enter" a e)
		   (save-easing-state drag)
		   (set-opacity drag 255)
		   (restore-easing-state drag)
		   #f)) ;; yes, please propagate the event
	(connect drag
		 'leave-event
		 (lambda (a e)
		   #;(dimfi "leave" a e)
		   (save-easing-state drag)
		   (set-opacity drag 196)
		   (restore-easing-state drag)
		   #f)) ;; yes, please propagate the event
	(connect drag-action
		 'drag-begin
		 (lambda (d r event-x event-y modifiers)
		   #;(dimfi "drag-begin" d r event-x event-y modifiers)
		   (if (memq 'shift-mask (gflags->symbol-list modifiers))
		       (receive (x y)
			   (get-position r)
			 (or (get-stage drag-copy)
			     (add-child (get-stage drag) drag-copy))
			 (set-position drag-copy x y)
			 (show drag-copy)
			 (set-drag-handle d drag-copy))
		       (set-drag-handle d drag))))
	(connect drag-action
		 'drag-end
		 (lambda (d r event-x event-y modifiers)
		   #;(dimfi "drag-end" d r event-x event-y modifiers)
		   (if (eq? (get-drag-handle d) drag-copy)
		       (receive (x y)
			   (get-position drag-copy)
			 (save-easing-state drag)
			 (set-position drag x y)
			 (restore-easing-state drag)
			 (hide drag-copy)))))
	(add-action drag drag-action))))
