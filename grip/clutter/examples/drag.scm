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


(define-module (grip clutter examples drag)
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
  #:use-module (grip clutter colour)
  #:use-module (grip clutter label)
  #:use-module (grip clutter grid)
  #:use-module (grip clutter utils)
  #:use-module (grip clutter examples clue)

  #:export (clue-drag-user-help
	    clue-drag-drag-begin-action
	    clue-drag-drag-end-action))


(define %drag-help-specs
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
    (2 0 1 2 #f "hold down" #f #f #f #f #f start center #t #f)
    (3 0 1 1 #f "]" #f #f "Chocolate" #f #f fill center #t #f)
    (4 0 1 1 #f " to drag" #f #f #f #f #f fill center #t #f)
    ;; line 2
    (0 1 1 1 #f
       ,(str/span "Shift-Left" "foreground" "LightSkyBlue")
       #f #f #f #f #f start center #t #f)
    (1 1 1 1 #f "[" #f #f "Chocolate" #f #f start center #t #f)
    (3 1 1 1 #f "]" #f #f "Chocolate" #f #f fill center #t #f)
    (4 1 1 1 #f " to drag a copy" #f #f #f #f #f fill center #f #f)))

(define* (clue-drag-user-help stage drag
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
		     %drag-help-specs)))
    (and bg (set-background-color grid (get-colour bg)))
    grid))

(define clue-drag-drag-begin-action #f)
(define clue-drag-drag-end-action #f)

(let ((drag-copy (make <clutter-actor>
		   #:background-color %tango-chameleon-dark)))
  (set-opacity drag-copy 64)
  (set! clue-drag-drag-begin-action
	(lambda (drag-action drag event-x event-y modifiers)
	  (if (memq 'shift-mask (gflags->symbol-list modifiers))
	      (receive (x y)
		  (get-position drag)
		(unless (get-stage drag-copy)
		  (add-child (get-stage drag) drag-copy)
		  (set-size drag-copy (get-width drag) (get-height drag)))
		(set-position drag-copy x y)
		(show drag-copy)
		(set-drag-handle drag-action drag-copy))
	      (set-drag-handle drag-action drag))))
  (set! clue-drag-drag-end-action
	(lambda (drag-action drag event-x event-y modifiers)
	  (if (eq? (get-drag-handle drag-action) drag-copy)
	      (receive (x y)
		  (get-position drag-copy)
		(save-easing-state drag)
		(set-position drag x y)
		(restore-easing-state drag)
		(hide drag-copy))))))
