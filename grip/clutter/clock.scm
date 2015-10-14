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

;; this needs review.  also users should be able to specify their
;; colour set [backgtround, rail, seconds minites hours].

;;; Code:


(define-module (grip clutter clock)
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

  #:export (<clus-clock>))


(define (clus-clock-item-colour item)
  (let ((oport (current-output-port)))
    (case item
      ((background) (get-colour "CornflowerBlue"))
      ((rail) (get-colour "Black"))
      ((seconds) (get-colour "Chocolate" #:alpha 196))
      ((minutes hours) (get-colour "DarkGreen" #:alpha 196))
      (else
       (format oport "Warning: no such clock item: ~A\n" item)))))

(define-class <clus-clock> (<clutter-actor>)
  (radius #:getter radius #:init-value #f)
  (radius/2 #:getter radius/2 #:init-value #f))

(define-method (set-radius (self <clus-clock>) w h)
  (let* ((radius (max w h))
	 (radius/2 (/ radius 2)))
    (slot-set! self 'radius radius)
    (slot-set! self 'radius/2 radius/2)))

(define (clock-draw-black-rail cr)
  (receive (r g b a)
      (get-rgba-values (clus-clock-item-colour 'rail) #:float #t)
    (cairo-set-source-rgb cr r g b)
    (cairo-translate cr 0.5 0.5)
    (cairo-arc cr 0 0 0.4 0 %2pi)
    (cairo-stroke cr)))

(define (clock-draw-seconds-indicator cr seconds)
  (receive (r g b a)
      (get-rgba-values (clus-clock-item-colour 'seconds) #:float #t)
    (cairo-set-source-rgba cr r g b a)
    (cairo-move-to cr 0 0)
    (cairo-arc cr (* (sin seconds) 0.4) (* (cos seconds) -0.4) 0.05 0 %2pi)
    (cairo-fill cr)))

(define (clock-draw-minutes-hand cr minutes)
  (receive (r g b a)
      (get-rgba-values (clus-clock-item-colour 'minutes) #:float #t)
    (cairo-set-source-rgba cr r g b a)
    (cairo-move-to cr 0 0)
    (cairo-line-to cr (* (sin minutes) 0.4) (* (cos minutes) -0.4))
    (cairo-stroke cr)))

(define (clock-draw-hours-hand cr hours)
  (receive (r g b a)
      (get-rgba-values (clus-clock-item-colour 'hours) #:float #t)
    (cairo-set-source-rgba cr r g b a)
    (cairo-move-to cr 0 0)
    (cairo-line-to cr (* (sin hours) 0.2) (* (cos hours) -0.2))
    (cairo-stroke cr)))

(define-method (draw (self <clus-clock>) canvas cr w h)
  (let* ((rad (radius self))
	 (rad/2 (radius/2 self))
	 (time (localtime (current-time)))
	 (seconds (/ (* (tm:sec time) %pi) 30))
	 (minutes (/ (* (tm:min time) %pi) 30))
	 (hours (/ (* (tm:hour time) %pi) 6)))
    ;; (dimfi "time is: " (tm:hour time) "hrs " (tm:min time) "min " (tm:sec time) "sec")
    (cairo-save cr)
    (cairo-set-operator cr 'clear)
    (cairo-paint cr)
    (cairo-restore cr)
    (cairo-set-operator cr 'over)
    (cairo-scale cr w h)
    (cairo-set-line-cap cr 'round)
    (cairo-set-line-width cr 0.1)
    (clock-draw-black-rail cr)
    (clock-draw-seconds-indicator cr seconds)
    (clock-draw-minutes-hand cr minutes)
    (clock-draw-hours-hand cr hours)))

(define-method (initialize (self <clus-clock>) initargs)
  (next-method)
  (set-background-color self (clus-clock-item-colour 'background))
  (receive (w h)
      (get-size self)
    (set-radius self w h)
    (set-canvas self w h draw)
    (set-pivot-point self 0.5 0.5)
    (set-translation self (/ w -2) (/ h -2) 0.0)))
