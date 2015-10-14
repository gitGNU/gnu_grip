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

;; This needs review

;;; Code:


(define-module (grip clutter grid)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip utils)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip keyword)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter apple)

  #:export (<clus-grid>))


(g-export add-child
	  get-row-spacing
	  set-row-spacing
	  get-column-spacing
	  set-column-spacing
	  get-row-homogeneous
	  set-row-homogeneous
	  get-column-homogeneous
	  set-column-homogeneous

	  !grid-layout
	  !row-spacing
	  !column-spacing
	  !row-homogeneous
	  !column-homogeneous

	  get-child-meta-object
	  get-child-meta-values
	  get-children-meta-objects
	  get-children-meta-values
	  get-dimensions
	  get-row
	  get-col
	  #;get-min-width
	  #;get-min-height
	  #;get-min-size)


;;;
;;; Grid
;;;

(define-class <clus-grid> (<clus-apple>)
  (grid-layout #:getter !grid-layout
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (get-layout-manager self))
	       #:slot-set! (lambda (self value)
			     'nothing))
  (row-spacing #:accessor !row-spacing
	       #:init-keyword #:row-spacing
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (get-row-spacing (get-layout-manager self)))
	       #:slot-set! (lambda (self value)
			     (set-row-spacing (get-layout-manager self) value)))
  (column-spacing #:accessor !column-spacing
		  #:init-keyword #:column-spacing
		  #:allocation #:virtual
		  #:slot-ref (lambda (self)
			       (get-column-spacing (get-layout-manager self)))
		  #:slot-set! (lambda (self value)
				(set-column-spacing (get-layout-manager self) value)))
  (row-homogeneous #:accessor !row-homogeneous
		   #:init-keyword #:row-homogeneous
		   #:allocation #:virtual
		   #:slot-ref (lambda (self)
				(get-row-homogeneous (get-layout-manager self)))
		   #:slot-set! (lambda (self value)
				 (set-row-homogeneous (get-layout-manager self) value)))
  (column-homogeneous #:accessor !column-homogeneous
		      #:init-keyword #:column-homogeneous
		      #:allocation #:virtual
		      #:slot-ref (lambda (self)
				   (get-column-homogeneous (get-layout-manager self)))
		      #:slot-set! (lambda (self value)
				    (set-column-homogeneous (get-layout-manager self) value))))

(define-method (initialize (self <clus-grid>) initargs)
  #;(dimfi "initialize <clus-grid>" initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-grid>)))
    (let ((grid-layout (make <clutter-grid-layout>)))
      (if (null? virtual-kw)
	  (next-method)
	  (begin
	    (next-method self kw)
	    (let-keywords virtual-kw #t
			  ((colour #f)
			   (orientation #f)
			   (row-spacing #f)
			   (column-spacing #f)
			   (row-homogeneous #f)
			   (column-homogeneous #f))
			  (when colour
			    (set! (!colour self) colour))
			  (when orientation
			    (set-orientation grid-layout orientation))
			  (when row-spacing
			    (set-row-spacing grid-layout row-spacing))
			  (when column-spacing
			    (set-column-spacing grid-layout column-spacing))
			  (when row-homogeneous
			    (set-row-homogeneous grid-layout row-homogeneous))
			  (when column-homogeneous
			    (set-column-homogeneous grid-layout column-homogeneous)))))
      (set-layout-manager self grid-layout))))

(define-method (add-child (grid <clus-grid>) child left top width height)
  (attach (!grid-layout grid) child left top width height))

(define-method (get-row-spacing (self <clus-grid>))
  (!row-spacing self))

(define-method (set-row-spacing (self <clus-grid>) value)
  (set! (!row-spacing self) value))

(define-method (get-column-spacing (self <clus-grid>))
  (!column-spacing self))

(define-method (set-column-spacing (self <clus-grid>) value)
  (set! (!column-spacing self) value))

(define-method (get-row-homogeneous (self <clus-grid>))
  (!row-homogeneous self))

(define-method (set-row-homogeneous (self <clus-grid>) value)
  (set! (!row-homogeneous self) value))

(define-method (get-column-homogeneous (self <clus-grid>))
  (!column-homogeneous self))

(define-method (set-column-homogeneous (self <clus-grid>) value)
  (set! (!column-homogeneous self) value))


;;;
;;; meta and other child properties...
;;;

(define-method (get-child-meta-object (self <clutter-actor>))
  (let* ((parent (get-parent self))
	 (grid-layout (and parent
			   (is-a? parent <clus-grid>)
			   (get-layout-manager parent))))
    (and grid-layout
	 (get-child-meta grid-layout parent self))))

(define-method (get-child-meta-values (self <clutter-actor>))
  (let ((child-meta (get-child-meta-object self)))
    (and child-meta
	 (list (get child-meta 'left-attach)
	       (get child-meta 'top-attach)
	       (get child-meta 'width)
	       (get child-meta 'height)
	       (get self 'x-expand)
	       (get self 'y-expand)
	       (genum->symbol (get self 'x-align))
	       (genum->symbol (get self 'y-align))))))

(define-method (get-children-meta-objects (self <clus-grid>))
  (map get-child-meta-object (get-children self)))

(define-method (get-children-meta-values (self <clus-grid>))
  (map get-child-meta-values
    (get-children self)))

(define-method (get-dimensions (self <clus-grid>))
  ;; dimensions:
  ;;   rows, cols, max row span, max col span
  (let ((children-meta-values (get-children-meta-values self)))
    (if (pair? children-meta-values)
	(list (1+ (match children-meta-values (((_ r _ _ . rest) ...) (reduce max 0 r))))
	      (1+ (match children-meta-values (((c _ _ _) . rest) (reduce max 0 c))))
	      (match children-meta-values (((_ _ w _) . rest) (reduce max 0 w)))
	      (match children-meta-values (((_ _ _ h) . rest) (reduce max 0 h))))
	(list 0 0 0 0))))

(define-method (get-row (self <clus-grid>) id)
  (let* ((children (get-children self))
	 (how-many (length children)))
    (if (and children
	     (< id how-many))
	(filter-map (lambda (child)
		      (match (get-child-meta-values child)
			((c r w h . rest) (and (= r id) child))))
	    children)
	(scm-error 'out-of-range
		   "get-row"
		   "~A: row id ~A is bounds"
		   (list self id) #f))))

(define-method (get-col (self <clus-grid>) id)
  (let* ((children (get-children self))
	 (how-many (length children)))
    (if (and children
	     (< id how-many))
	(filter-map (lambda (child)
		      (match (get-child-meta-values child)
			((c r w h . rest) (and (= c id) child))))
	    children)
	(scm-error 'out-of-range
		   "get-col"
		   "~A: col id ~A is bounds"
		   (list self id) #f))))

;; the following definitions should not be needed anymore, after the
;; clutter has been patched [see clutter master, commit dea5057].

#;(define-method (get-min-width (self <clus-grid>))
  (let ((row0 (get-row self 0)))
    (and row0
	 (+ (reduce + 0 (map get-width row0))
	    (* (- (length row0) 1) (!column-spacing self))))))

#;(define-method (get-min-height (self <clus-grid>))
  (let ((col0 (get-col self 0)))
    (and col0
	 (+ (reduce + 0 (map get-height col0))
	    (* (- (length col0) 1) (!row-spacing self))))))

#;(define-method (get-min-size (self <clus-grid>))
  (values (get-min-width self)
	  (get-min-height self)))


#!

;; I can't get 

(define g (make <clus-grid>))
(set-background-color g (get-colour "Gainsboro"))
(define a1 (make <clutter-actor>))
(define a2 (make <clutter-actor>))
(set-background-color a1 (get-colour "Green"))
(set-background-color a2 (get-colour "Red"))
(set-size a1 32 32)
(set-size a2 32 32)
(add-child g a1 0 0 1 1)
(add-child g a2 1 0 1 1)
(add-child *stage* g)
(set-position g 150 150)
(!orientation g)
(set! (!orientation g) 'vertical)
(get-children-meta-values g)

!#
