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


(define-module (grip clutter animation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)

  #:export (%clus-easing-modes
	    clus-current-easing-mode
	    clus-next-easing-mode))


(define %clus-easing-modes #f)
(define clus-current-easing-mode #f)
(define clus-next-easing-mode #f)

(eval-when (expand load eval)
  (let* ((i 0)
	 (e-modes (vector->list (genum-class->value-table <clutter-animation-mode>)))
	 (a-modes (filter-map (lambda (em)
				(and (case (car em)
				       ((custom-mode animation-last) #f)
				       (else em))
				     em))
		      e-modes))
	 (how-many (length a-modes)))
    (set! %clus-easing-modes
	  a-modes)
    (set! clus-current-easing-mode
	  (lambda ()
	    (car (list-ref a-modes i))))
    (set! clus-next-easing-mode
	  (lambda ()
	    (set! i (modulo (1+ i) how-many))
	    (car (list-ref a-modes i))))))
