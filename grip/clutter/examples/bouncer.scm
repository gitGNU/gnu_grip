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


(define-module (grip clutter examples bouncer)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip clutter label)

  #:export (clue-bouncer-help-message
	    clue-bouncer-help-label))


(define %clue-bouncer-help-message
  "Easing mode: <span foreground=\"LightSkyBlue\">~A</span>
Left click to tween
Right click to change the easing mode")

(define (clue-bouncer-help-message easing-mode)
  (format #f "~?" %clue-bouncer-help-message (list easing-mode)))

(define (clue-bouncer-help-label font colour easing-mode)
  (receive (label lw lh)
      (clus-make-label (clue-bouncer-help-message easing-mode)
		       font colour 'use-markup)
    (values label lw lh)))
