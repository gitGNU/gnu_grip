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

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (<clus-drag>
	    #;install-drag-action))


(g-export !colour
	  !e-action
	  !l-action
	  !db-action
	  !de-action)


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
			(set-background-color self value)))
  (opacity-on-enter #:accessor !opacity-on-enter
		    #:init-keyword #:opacity-on-enter
		    #:init-value 255)
  (opacity-on-leave #:accessor !opacity-on-leave
		    #:init-keyword #:opacity-on-leave
		    #:init-value 196)
  (e-action #:accessor !e-action #:init-keyword #:e-action #:init-value identities)
  (l-action #:accessor !l-action #:init-keyword #:l-action #:init-value identities)
  (db-action #:accessor !db-action #:init-keyword #:db-action #:init-value identities)
  (de-action #:accessor !de-action #:init-keyword #:de-action #:init-value identities))

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
    (set-opacity self (!opacity-on-leave self))
    (install-drag-action self)))

(define (install-drag-action drag)
  (let ((drag-action (clutter-drag-action-new)))
    (connect drag
	     'enter-event
	     (lambda (actor event)
	       (save-easing-state drag)
	       (set-opacity drag (!opacity-on-enter drag))
	       (restore-easing-state drag)
	       ((!e-action drag) actor event)
	       #f)) ;; yes, please propagate the event
    (connect drag
	     'leave-event
	     (lambda (actor event)
	       (save-easing-state drag)
	       (set-opacity drag (!opacity-on-leave drag))
	       (restore-easing-state drag)
	       ((!l-action drag) actor event)
	       #f)) ;; yes, please propagate the event
    (connect drag-action
	     'drag-begin
	     (lambda (da d event-x event-y modifiers)
	       ((!db-action drag) da d event-x event-y modifiers)))
    (connect drag-action
	     'drag-end
	     (lambda (da d event-x event-y modifiers)
	       ((!de-action drag) da d event-x event-y modifiers)))
    (add-action drag drag-action)))
