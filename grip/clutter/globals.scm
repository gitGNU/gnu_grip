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

;; This code needs review.

;;; Code:


(define-module (grip clutter globals)
  #:use-module (grip clutter colour)

  #:export (*clutter-version*
	    *clutter-binding*

	    *clus-stage-width*
	    *clus-stage-height*
	    *clus-stage-colour*

	    *clus-label-padding*
	    *clus-label-font*
	    *clus-label-fg*
	    #;*clus-label-bg*

	    *clus-tooltip-font*
	    *clus-tooltip-bg*
	    *clus-tooltip-fg*
	    *clus-tooltip-border*

	    *clus-toolbar-item-on-enter-bg*
	    *clus-toolbar-item-on-leave-bg*
	    *clus-toolbar-colour*
	    *clus-toolbar-border*
	    *clus-toolbar-spacing*

	    *clus-apple-layout*
	    *clus-apple-box-layout-spacing*
	    *clus-apple-box-layout-orientation*
	    *clus-apple-bin-layout-x-align*
	    *clus-apple-bin-layout-y-align*
	    *clus-apple-gravity*
 	    *clus-apple-gravity-pole*
 	    *clus-apple-gravity-force*

	    *clus-gravity-resistance-min*
	    *clus-gravity-resistance-max*
	    *clus-gravity-resistance-normalization-quotient*

	    *clus-text-bin-layout-x-align*
	    *clus-text-bin-layout-y-align*

	    *clus-text-class-rendering-colour*))


;; this should come from guile-clutter itself, but right now there is
;; no effective, micro, minor, major version variable for a specific
;; binding set.
(define *clutter-version* "1.12.2.1")

(define *clutter-binding* "Guile-Clutter is a direct binding of the clutter library for the guile
language.  This is the binding for the clutter library version ~A.")

(define *clus-stage-width* 600)
(define *clus-stage-height* 400)
(define *clus-stage-colour* #;"OldLace" '(60 60 60 255))

(define *clus-label-padding* 0)
(define *clus-label-font* "Dejavu Sans 9")
(define *clus-label-fg* "Gainsboro")
#;(define *clus-label-bg* *clus-stage-colour*)

(define *clus-tooltip-font* "Dejavu Sans 9")
(define *clus-tooltip-bg* "Black")
(define *clus-tooltip-fg* "Gainsboro")
(define *clus-tooltip-border* 6)

(define *clus-toolbar-colour* "Gainsboro")
(define *clus-toolbar-item-on-enter-bg* "WhiteSmoke")
(define *clus-toolbar-item-on-leave-bg* "Gainsboro")
(define *clus-toolbar-border* 2)
(define *clus-toolbar-spacing* 4)

(define *clus-apple-layout* 'box)
(define *clus-apple-box-layout-spacing* 4)
(define *clus-apple-box-layout-orientation* 'horizontal)
(define *clus-apple-bin-layout-x-align* 'center)
(define *clus-apple-bin-layout-y-align* 'center)
(define *clus-apple-gravity* 'north-west)
(define *clus-apple-gravity-pole* 'north)
(define *clus-apple-gravity-force* 'strong)

(define *clus-gravity-resistance-min* 0.0)
(define *clus-gravity-resistance-max* 1.0)
(define *clus-gravity-resistance-normalization-quotient* 4)

(define *clus-text-bin-layout-x-align* 'center)
(define *clus-text-bin-layout-y-align* 'center)

(define *clus-text-class-rendering-colour* '(152 251 152 255))
