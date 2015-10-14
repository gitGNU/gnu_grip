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


(define-module (grip clutter examples spline)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (cairo)
  #:use-module (gnome clutter)
  #:use-module (grip nbs)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter actor)

  #:export (<clus-spline>))

(define-class <clus-spline> (<clutter-actor>)
  (radius #:getter radius #:init-value #f))
  
(define* (spline-draw-spline cr x y x1 y1 x2 y2 x3 y3
			     #:key (line-width 10)
			     (r 0.2)
			     (g 1.0)
			     (b 0.2)
			     (a 0.6))
  (cairo-set-source-rgba cr r g b a)
  (cairo-set-line-width cr line-width)
  (cairo-move-to cr x y)
  (cairo-curve-to cr x1 y1 x2 y2 x3 y3)
  (cairo-stroke cr))

(define* (spline-draw-marks cr x y x1 y1 x2 y2 x3 y3
			    #:key (line-width 6)
			    (r 1.0)
			    (g 0.2)
			    (b 0.2)
			    (a 0.6))
  (cairo-set-source-rgba cr r g b a)
  (cairo-set-line-width cr line-width)
  (cairo-move-to cr x y)
  (cairo-line-to cr x1 y1)
  (cairo-move-to cr x2 y2)
  (cairo-line-to cr x3 y3)
  (cairo-stroke cr))

(define-method (draw (self <clus-spline>) canvas cr w h)
  (receive (x y x1 y1 x2 y2 x3 y3)
      (values 25.6 128.0 102.4 230.4 153.6 25.6 230.4 128.0)
    (cairo-save cr)
    (cairo-set-operator cr 'clear)
    (cairo-paint cr)
    (cairo-restore cr)
    (cairo-set-operator cr 'over)
    ;; (cairo-scale cr w h)
    (spline-draw-spline cr x y x1 y1 x2 y2 x3 y3)
    (spline-draw-marks cr x y x1 y1 x2 y2 x3 y3)))

(define-method (initialize (self <clus-spline>) initargs)
  (next-method)
  (set-background-color self (get-colour "Black"))
  (receive (w h)
      (get-size self)
    (set-canvas self w h draw)
    (set-translation self (/ w -2) (/ h -2) 0.0)))
