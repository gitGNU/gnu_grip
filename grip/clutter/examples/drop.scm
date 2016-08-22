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


(define-module (grip clutter examples drop)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip utils)
  #:use-module (grip strings)
  #:use-module (grip clutter)
  #:use-module (grip clutter examples clue)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (clue-drop-user-help
	    clue-drop-drag-begin-action
	    clue-drop-drag-end-action))


(g-export add-child)


(define %drop-help-specs
  `(;; line 1
    (0 0 1 1				;; left top width height
     #f					;; actor's name or #f
     ,(str/span "Left" "foreground" "#98fb98") ;; text
     #f					;; font-name or #f
     #f					;; background or #f
     #f					;; foreground or #f
     #f				 	;; span (prop value) or #f
     #f					;; line alignment of #f
     fill				;; x-align
     center				;; y-align
     #t					;; x-expand
     #f)				;; y-expand
    (1 0 1 1 #f "[" #f #f "Chocolate" #f #f start center #t #f)
    (2 0 1 1 #f "hold down" #f #f #f #f #f start center #t #f)
    (3 0 1 1 #f "]" #f #f "Chocolate" #f #f fill center #t #f)
    (4 0 1 1 #f " to drag" #f #f #f #f #f fill center #t #f)
    ;; line 2
    (0 1 1 1 #f
       ,(str/span "Release" "foreground" "LightSkyBlue")
       #f #f #f #f #f start center #t #f)
    (1 1 4 1 #f " to drop" #f #f #f #f #f fill center #f #f)))

(define* (clue-drop-user-help stage drag
			      #:key (bg #f)
			      (cbg #f)
			      (cfg (get-colour "Gainsboro")))
  (let* ((grid (make <clus-grid>
		 ;; #:background-color (get-darker-colour *clue-stage-colour*)
		 #:orientation 'horizontal
		 #:row-spacing 2
		 #:column-spacing 4
		 #:row-homogeneous #t
		 #:column-homogeneous #f))
	 (children (map (lambda (spec)
			  (clue-help-grid-add-child grid spec))
		     %drop-help-specs)))
    (and bg (set-background-color grid (get-colour bg)))
    grid))

(define (clue-drop-drag-begin-action drag-action drag event-x event-y modifiers)
  (set-drop #f))

(define (transpose target actor)
  ;; x, y are relative to the actor's parent;  tx, ty are the target pos in
  ;; the stage.  in this drop example case, y remains unchanged.
  (let* ((stage (get-stage target))
	 (target1 (get-child-named (get-stage target) "Target1"))
	 (constraints (map (lambda (item)
			     (cons (genum->symbol (get item 'align-axis)) (get item 'factor)))
			(get-constraints target1)))
	 (spacing (* (assq-ref constraints 'x-axis)
		     (get-width stage))))
  (receive (x y)
      (get-position actor)
    (receive (px py)
	(get-position (get-parent actor))
	(if (< x 0)
	    (values (+ px x (- spacing)) y)
	    (receive (tx ty)
		(get-position target)
	      (values (- (+ x spacing) tx) y)))))))

(define (clue-drop-drag-end-action drag-action drag event-x event-y modifiers)
  (let* ((x (get-x drag))
	 (y (get-y drag))
	 (w (get-width drag))
	 (h (get-height drag))
	 (parent (get-parent drag)))
    (match (get-drop)
      ((drop-t drop-x drop-y)
       (receive (tw th)
	   (get-size drop-t)
	 (if (eq? drop-t parent)
	     (begin
	       (save-easing-state drag)
	       (center-on-target parent drag)
	       (restore-easing-state drag)
	       (save-easing-state parent)
	       (set-opacity parent 64)
	       (restore-easing-state parent))
	     (receive (tx ty)
		 (get-position drop-t)
	       (receive (trans-x trans-y)
		   (transpose drop-t drag)
		 (remove-child parent drag)
		 (add-child drop-t drag)
		 #;(dimfi "x" x "y" y "trans-x" trans-x "trans-y" trans-y)
		 (set-position drag trans-x trans-y))
	       (save-easing-state drag)
	       (center-on-target drop-t drag)
	       (restore-easing-state drag)
	       (save-easing-state drop-t)
	       (set-opacity drop-t 64)
	       (restore-easing-state drop-t)))))
      (#f
        (save-easing-state drag)
	(center-on-target parent drag)
	(restore-easing-state drag)
	(save-easing-state parent)
	(set-opacity parent 64)
	(restore-easing-state parent)))))

#;(define-method (add-child (parent <clus-drop>) child)
  (next-method)
  (center-on-target parent child))
