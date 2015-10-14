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


(define-module (grip clutter constraint)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)

  #:export (clus-make-align-constraint
	    clus-make-bind-constraint
	    clus-bind
	    #;clus-make-path-constraint
	    clus-make-snap-constraint
	    clus-snap
	    clus-snap-south-west
	    clus-snap-south-east
	    clus-snap-to-source))

(define (clus-make-align-constraint source axis factor)
  (make <clutter-align-constraint>
    ;; #:name (symbol->string axis)
    #:source source
    #:align-axis axis
    #:factor factor))

(define (clus-make-bind-constraint source coordinate offset)
  (make <clutter-bind-constraint>
    #:source source
    #:coordinate coordinate
    #:offset offset))

(define (clus-bind source actor coordinate offset)
  (add-constraint actor
		  (clus-make-bind-constraint source coordinate offset)))


;; (add-constraint header (clus-make-bind-constraint stage 'x 0.0))
;; (add-constraint header (clus-make-bind-constraint stage 'y 0.0))
;; (add-constraint header (clus-make-bind-constraint stage 'width 0.0))
;; (add-child stage header)
;; (receive (fw fh)
;;     (get-size footer)
;;   (set! (!footer self) footer)
;;   (add-constraint footer (clus-make-bind-constraint stage 'width 0.0))
;;   (add-constraint footer (clus-make-snap-constraint stage 'bottom 'bottom 0.0))
;;   (add-constraint footer (clus-make-snap-constraint stage 'top 'bottom (- fh)))


(define (clus-make-snap-constraint source from to offset)
  (make <clutter-snap-constraint>
    #:source source
    #:from-edge from
    #:to-edge to
    #:offset offset))

(define (clus-snap source actor from to padding)
  (add-constraint actor
		  (clus-make-snap-constraint source from to padding)))

(define (clus-snap-south-west source actor ns-padding we-padding)
  (receive (aw ah)
      (get-size actor)
    (clus-snap source actor 'bottom 'bottom (- ns-padding))
    (clus-snap source actor 'top 'bottom (- (+ ah ns-padding)))
    (clus-snap source actor 'left 'left we-padding)
    (clus-snap source actor 'right 'left (+ aw we-padding))))

(define (clus-snap-south-east source actor ns-padding we-padding)
  (receive (aw ah)
      (get-size actor)
    (clus-snap source actor 'bottom 'bottom (- ns-padding))
    (clus-snap source actor 'top 'bottom (- (+ ah ns-padding)))
    (clus-snap source actor 'right 'right (- we-padding))
    (clus-snap source actor 'left 'right (- (+ aw we-padding)))))

(define* (clus-snap-to-source source actor
			      #:key (left-margin 0)
			      (top-margin 0)
			      (bottom-margin 0)
			      (right-margin 0))
  (clus-snap source actor 'top 'bottom top-margin)
  (clus-snap source actor 'bottom 'top (- bottom-margin))
  (clus-snap source actor 'left 'left left-margin)
  (clus-snap source actor 'right 'right (- right-margin)))
