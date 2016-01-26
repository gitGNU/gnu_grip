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

;;; Code:


(define-module (grip clutter gravity)
  #:use-module (grip lists)
  #:use-module (grip clutter globals)
  
  #:export (%clus-gravities
	    clus-gravity?
	    %clus-gravity-poles
	    clus-gravity-pole?
	    %clus-gravity-forces
	    clus-gravity-force?
	    %clus-gravity-resistance-modes
	    clus-gravity-resistance-mode?
	    clus-gravity-valid-resistance-factor?
	    clus-get-next))


(define %clus-gravities
  '(north-west north north-east east south-east south south-west west center))

(define (clus-gravity? gravity)
  (memq gravity %clus-gravities))

(define %clus-gravity-poles
  '(north west))

(define (clus-gravity-pole? pole)
  (memq pole %clus-gravity-poles))

(define %clus-gravity-forces
  '(strong weak))

(define (clus-gravity-force? force)
  (memq force %clus-gravity-forces))

(define %clus-gravity-resistance-modes
  '(asym min max))

(define (clus-gravity-resistance-mode? mode)
  (memq mode %clus-gravity-resistance-modes))

(define (clus-gravity-valid-resistance-factor? factor)
  (and (>= factor *clus-gravity-resistance-min*)
       (<= factor *clus-gravity-resistance-max*)))

(define (clus-get-next what current)
  (let ((llist (case what
		 ((gravity) %clus-gravities)
		 ((pole) %clus-gravity-poles)
		 ((force) %clus-gravity-forces)
		 ((r-mode) %clus-gravity-resistance-modes))))
    (list-ref llist
	      (modulo (+ (list-pos current llist eq?) 1)
		      (length llist)))))
