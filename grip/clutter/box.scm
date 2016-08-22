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


(define-module (grip clutter box)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome clutter)
  #:use-module (grip utils)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip keyword)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter apple)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<clus-box>))


(g-export get-spacing
	  set-spacing
	  get-homogeneous
	  set-homogeneous

	  !spacing
	  !homogeneous)


(define-class <clus-box> (<clus-apple>)
  (spacing #:accessor !spacing
	   #:init-keyword #:spacing
	   #:allocation #:virtual
	   #:slot-ref (lambda (self)
			(get-spacing (get-layout-manager self)))
	   #:slot-set! (lambda (self value)
			 (set-spacing (get-layout-manager self) value)))
  (homogeneous #:accessor !homogeneous
	       #:init-keyword #:homogeneous
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (get-homogeneous (get-layout-manager self)))
	       #:slot-set! (lambda (self value)
			     (set-homogeneous (get-layout-manager self) value))))

(define-method (initialize (self <clus-box>) initargs)
  #;(dimfi "initialize <clus-box>" initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-box>)))
    (let ((box-layout (make <clutter-box-layout>)))
      (if (null? virtual-kw)
	  (begin
	    (next-method)
	    (set-layout-manager self box-layout))
	  (begin
	    (next-method self kw)
	    (set-layout-manager self box-layout)
	    (let-keywords virtual-kw #t
			  ((spacing #f)
			   (homogeneous #f))
			  (when spacing
			    (set! (!spacing self) spacing))
			  (when homogeneous
			    (set! (!homogeneous self) homogeneous))))))))

(define-method (get-spacing (self <clus-box>))
  (!spacing self))

(define-method (set-spacing (self <clus-box>) value)
  (set! (!spacing self) value))

(define-method (get-homogeneous (self <clus-box>))
  (!homogeneous self))

(define-method (set-homogeneous (self <clus-box>) value)
  (set! (!homogeneous self) value))


#!

(define box (make <clus-box>))
(set-background-color box (get-colour "Gainsboro"))

(define a1 (make <clutter-actor>))
(set-background-color a1 (get-colour "Green"))
(set-size a1 32 32)
(add-child box a1)

(define a2 (make <clutter-actor>))
(set-background-color a2 (get-colour "Red"))
(set-size a2 32 32)
(add-child box a2)

(add-child *stage* box)
(set-position box 150 150)
(!orientation box)
(set! (!orientation box) 'vertical)

!#
