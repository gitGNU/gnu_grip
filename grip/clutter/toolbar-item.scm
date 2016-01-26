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

;; This code needs review, but it won't be as good as it could until
;; goops is fixed.

;;; Code:


(define-module (grip clutter toolbar-item)
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
  #:use-module (grip clutter actor)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter gravity)
  #:use-module (grip clutter image)
  #:use-module (grip clutter label)
  #:use-module (grip clutter stage)
  #:use-module (grip clutter text)
  #:use-module (grip clutter apple)
  #:use-module (grip clutter box)
  #:use-module (grip clutter toolbar)
  #:use-module (grip clutter tooltip)
  #:use-module (grip clutter utils)

  #:export (<clus-toolbar-item>
	    clus-fullscreen-toolbar-item))


(g-export !on-enter-bg
	  !on-leave-bg
	  !on-click-bg
	  !icon
	  !tip
	  !tooltip
	  !1icon
	  !1tooltip
	  !2icon
	  !2tip
	  !2tooltip
	  !left-cb
	  !middle-cb
	  !right-cb
	  
	  ensure-tooltips-on-stage)


(eval-when (expand load eval)
  (re-export-public-interface (grip clutter tooltip)))


;;;
;;; Toolbar item
;;;

(define-class <clus-toolbar-item> (<clutter-actor>)
  (on-enter-bg #:accessor !on-enter-bg #:init-keyword #:on-enter-bg
	       #:init-value *clus-toolbar-item-on-enter-bg*)
  (on-leave-bg #:accessor !on-leave-bg #:init-keyword #:on-leave-bg
	       #:init-value *clus-toolbar-item-on-leave-bg*)
  (on-click-bg #:accessor !on-click-bg #:init-keyword #:on-click-bg
	       #:init-value #f)
  (icon #:accessor !icon #:init-keyword #:icon #:init-value #f)
  (tip #:accessor !tip #:init-keyword #:tip #:init-value #f)
  (tooltip #:accessor !tooltip #:init-keyword #:tooltip #:init-value #f)
  (1icon #:accessor !1icon #:init-keyword #:1icon #:init-value #f)
  (1tooltip #:accessor !1tooltip #:init-keyword #:1tooltip #:init-value #f)
  (2icon #:accessor !2icon #:init-keyword #:2icon #:init-value #f)
  (2tip #:accessor !2tip #:init-keyword #:2tip #:init-value #f)
  (2tooltip #:accessor !2tooltip #:init-keyword #:2tooltip #:init-value #f)
  (left-cb #:accessor !left-cb #:init-keyword #:left-cb #:init-value identity)
  (middle-cb #:accessor !middle-cb #:init-keyword #:middle-cb #:init-value identity)
  (right-cb #:accessor !right-cb #:init-keyword #:right-cb #:init-value identity))

(define-method (initialize (self <clus-toolbar-item>) initargs)
  (next-method)
  (let* ((icon (make <clus-image> #:filename (!icon self)))
	 (width (get-width icon))
	 (height (get-height icon))
	 (tip (!tip self))
	 (tooltip (and tip (make <clus-tooltip> #:text tip)))
	 (2icon (and (!2icon self) (make <clus-image> #:filename (!2icon self))))
	 (2tip (!2tip self))
	 (2tooltip (and 2tip (make <clus-tooltip> #:text 2tip))))
    (set-background-color self (get-colour (!on-leave-bg self)))
    (set-size self width height)
    (set-content self icon)
    (set-reactive self #t)
    (set! (!on-click-bg self) (get-darker-colour (!on-leave-bg self)))
    (set! (!icon self) icon)
    (set! (!1icon self) icon)
    (set! (!2icon self) 2icon)
    (when tooltip
      (set! (!tooltip self) tooltip)
      (set! (!1tooltip self) tooltip)
      (set-position tooltip width height)
      (add-child self tooltip)
      (hide tooltip))
    (when 2tooltip
      (set! (!2tooltip self) 2tooltip)
      (set-position 2tooltip width height)
      (add-child self 2tooltip)
      (hide 2tooltip))
    (connect self
	     'enter-event
	     (lambda (actor event)
	       (let ((item-on-hold (clus-stage-get-item-on-hold))
		     (tooltip (!tooltip self)))
		 (cond ((and item-on-hold (eq? item-on-hold self))
			(set-background-color self (!on-click-bg self))
			(when tooltip (show tooltip)))
		       (item-on-hold 'nothing)
		       (else
			(set-background-color self (get-colour (!on-enter-bg self)))
			(when tooltip (show tooltip)))))
	       #f)) ;; yes, please propagate the event
    (connect self
	     'leave-event
	     (lambda (a e)
	       (let ((tooltip (!tooltip self)))
		 (set-background-color self (get-colour (!on-leave-bg self)))
		 (when tooltip (hide tooltip)))
	       #f)) ;; yes, please propagate the event
    (connect self
	     'button-press-event
	     (lambda (actor event)
	       (clus-stage-set-item-on-hold self)
	       (set-background-color self (!on-click-bg self))
	       #t)) ;; stops the event to be propagated
    (connect self
	     'button-release-event
	     (lambda (actor event)
	       (let ((item-on-hold (clus-stage-get-item-on-hold))
		     (tooltip (!tooltip self)))
		 (when (and item-on-hold
			    (eq? item-on-hold self))
		   (set-background-color self (get-colour (!on-leave-bg self)))
		   (case (get-button event)
		     ((1) ((!left-cb self) self))
		     ((2) ((!middle-cb self) self))
		     ((3) ((!right-cb self) self))))
		 (when tooltip (hide tooltip))
		 (clus-stage-set-item-on-hold #f))
	       #t)))) ;; stops the event to be propagated

(define (ensure-tooltip-on-stage tooltip tbi tbi-w tbi-h)
  (let ((toolbar (get-parent tbi)))
    (and toolbar
	 (let ((gravity (!gravity toolbar))
	       (north-pole? (eq? (!gravity-pole toolbar) 'north))
	       (strong-g? (eq? (!gravity-force toolbar) 'strong))
	       (tooltip-w (get-width tooltip))
	       (tooltip-h (get-height tooltip)))
	   #;(dimfi tooltip (get-x tooltip) (get-y tooltip) tooltip-w tooltip-h)
	   ;; set-x
	   (case gravity
	     ((north-west west south-west north center south) (set-x tooltip tbi-w))
	     #;((north south) (set-x tooltip (+ (- (/ tooltip-w 2)) (/ tbi-w 2))))
	     ((north-east east south-east) (set-x tooltip (- tooltip-w))))
	   ;; set-y
	   (case gravity
	     ((north-west north north-east west center east) (set-y tooltip tbi-h))
	     #;((west east) (set-y tooltip (+ (- (/ tooltip-h 2)) (/ tbi-h 2))))
	     ((south-west south south-east) (set-y tooltip (- tooltip-h))))))))

(define-method (ensure-tooltips-on-stage (self <clus-toolbar-item>))
  (receive (tbi-w tbi-h)
      (get-size self)
    (let ((1tooltip (!1tooltip self))
	  (2tooltip (!2tooltip self)))
      (when 1tooltip (ensure-tooltip-on-stage 1tooltip self tbi-w tbi-h))
      (when 2tooltip (ensure-tooltip-on-stage 2tooltip self tbi-w tbi-h)))))


;;;
;;; General purpose toolbar item makers
;;;

(define (clus-fullscreen-toolbar-item stage)
  (make <clus-toolbar-item>
    #:icon (get-icon-filename "view-fullscreen.png")
    #:tip "Enter fullscreen mode"
    #:2icon (get-icon-filename "view-restore.png")
    #:2tip "Exit fullscreen mode"
    #:left-cb (lambda (self)
		(if (get-fullscreen stage)
		    (begin
		      (set-fullscreen stage #f)
		      (hide (!2tooltip self))
		      (set! (!tooltip self) (!1tooltip self))
		      (set-content self (!1icon self)))
		    (begin
		      (set-fullscreen stage #t)
		      (hide (!1tooltip self))
		      (set! (!tooltip self) (!2tooltip self))
		      (set-content self (!2icon self)))))))
