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

;; this needs review.

;;; Code:


(define-module (grip clutter bouncer)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (cairo)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip nbs)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter animation)
  #:use-module (grip clutter label)
  #:use-module (grip clutter actor)

  #:export (<clus-bouncer>))


(g-export !colour
	  !cairo-pattern)


(define-class <clus-bouncer> (<clutter-actor>)
  (colour #:accessor !colour #:init-keyword #:colour #:init-value #f)
  (r #:getter !r #:init-value #f)
  (g #:getter !g #:init-value #f)
  (b #:getter !b #:init-value #f)
  (a #:getter !a #:init-value #f)
  (radius #:getter !radius #:init-value #f)
  (radius/2 #:getter !radius/2 #:init-value #f)
  (cairo-pattern #:getter !cairo-pattern #:init-value #f))

(define-method ((setter !colour) (self <clus-bouncer>) rgba)
  (slot-set! self 'colour rgba)
  (set-rgba self rgba)
  (invalidate (get-content self)))

(define-method (set-rgba (self <clus-bouncer>) rgba)
  (slot-set! self 'r (/ (get-rgba-r rgba) 255))
  (slot-set! self 'g (/ (get-rgba-g rgba) 255))
  (slot-set! self 'b (/ (get-rgba-b rgba) 255))
  (slot-set! self 'a (/ (get-rgba-a rgba) 255)))

(define-method (set-cairo-pattern (self <clus-bouncer>) w h)
  (let* ((radius (max w h))
	 (radius/2 (/ radius 2)))
    (slot-set! self 'radius radius)
    (slot-set! self 'radius/2 radius/2)
    (slot-set! self 'cairo-pattern
	       (cairo-pattern-create-radial radius/2 radius/2 0 radius radius radius))))

(define-method (draw (self <clus-bouncer>) canvas cr w h)
  (let ((red (!r self))
	(green (!g self))
	(blue (!b self))
	(alpha (!a self))
	(rad (!radius self))
	(rad/2 (!radius/2 self))
	(pattern (!cairo-pattern self)))
    (cairo-set-operator cr 'clear)
    (cairo-paint cr)
    (cairo-set-operator cr 'over)
    (cairo-set-source-rgba cr red green blue alpha)
    (cairo-arc cr rad/2 rad/2 rad/2 0 %2pi)
    (cairo-pattern-add-color-stop-rgba pattern 0 red green blue alpha)
    (cairo-pattern-add-color-stop-rgba pattern 0.85 red green blue 0.25)
    (cairo-set-source cr pattern)
    (cairo-fill-preserve cr)))

(define-method (initialize (self <clus-bouncer>) initargs)
  (next-method)
  (receive (w h)
      (get-size self)
    (set-rgba self (!colour self))
    (set-cairo-pattern self w h)
    (set-canvas self w h draw)
    (set-pivot-point self 0.5 0.5)
    (set-translation self (/ w -2) (/ h -2) 0.0)
    (set-reactive self #t)))
