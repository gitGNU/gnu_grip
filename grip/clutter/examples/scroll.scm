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


(define-module (grip clutter examples scroll)
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

  #:export (%clue-scroll-menu-items
	    clue-scroll-make-menu
	    clue-scroll-user-help))


(g-export unchoose
	  choose)


(define-class <clue-menu> (<clutter-actor>)
  (fg #:accessor !fg #:init-keyword #:fg)
  (sfg #:accessor !sfg #:init-keyword #:sfg)
  (s-idx #:accessor !s-idx #:init-value #f))

(define-method (unchoose (self <clue-menu>))
  (let ((s-idx? (!s-idx self)))
    (and s-idx?
	 (set-color (get-child-at-index self s-idx?)
		    (get-colour (!fg self))))))

(define-method (choose (self <clue-menu>) index)
  (and (integer? index)
       (>= index 0)
       (< index (length (get-children self)))
       (let ((item (get-child-at-index self index)))
	 (unchoose self)
	 (set-color item (get-colour (!sfg self)))
	 (set! (!s-idx self) index))))

(define %clue-scroll-menu-items
  (map (lambda (i)
	 (string-append "Option "
			(number->string (+ i 1))))
    (iota 11)))

(define (clue-scroll-make-menu-item i-text font colour margin)
  (let ((item (clus-make-label i-text font (get-colour colour))))
    (set-margin-left item margin)
    (set-margin-right item margin)
    item))

(define (clue-scroll-make-menu items font bg fg sfg spacing margin)
  (let ((menu (make <clue-menu>
		#:background-color (get-colour bg)
		#:fg fg
		#:sfg sfg))
	(layout (make <clutter-box-layout>
		  #:orientation 'vertical
		  #:spacing spacing)))
    (set-layout-manager menu layout)
    (for-each (lambda (item)
		(add-child menu
			   (clue-scroll-make-menu-item item font fg margin)))
	items)
    menu))

(define (clue-scroll-up scroll)
  (let* ((menu (get-first-child scroll))
	 (new-idx (modulo (- (!s-idx menu) 1)
			  (length (get-children menu))))
	 (new-item (get-child-at-index menu new-idx)))
    (receive (x y)
	(get-position new-item)
      (scroll-to-point scroll x y)
      (choose menu new-idx))))

(define (clue-scroll-down scroll)
  (let* ((menu (get-first-child scroll))
	 (new-idx (modulo (+ (!s-idx menu) 1)
			  (length (get-children menu))))
	 (new-item (get-child-at-index menu new-idx)))
    (receive (x y)
	(get-position new-item)
      (scroll-to-point scroll x y)
      (choose menu new-idx))))

(define %scroll-help-specs
  `((0 0 1 1				;; left top width height
     #f					;; actor's name or #f
     "Use "				;; text
     #f					;; font-name or #f
     #f					;; background or #f
     #f					;; foreground or #f
     #f				 	;; span (prop value) or #f
     #f					;; line alignment of #f
     start				;; x-align
     center				;; y-align
     #f					;; x-expand
     #f)				;; y-expand
    (1 0 1 1 #f
       ,(string-append (str/span "\u2191" "foreground" "#98fb98")
		       (str/span ", " "foreground" "Gainsboro")
		       (str/span "\u2193" "foreground" "LightSkyBlue"))
       #f #f #f #f #f start center #f #f)
    (2 0 1 1 #f " to change the selection" #f #f #f #f #f start center #f #f)))

(define (install-stage-key-press-signals stage scroll)
  (connect stage
	   'key-press-event #;'key-release-event
	   (lambda (s e)
	     (case (get-key-symbol e)
	       ((65362) ;; up arrow
		(clue-scroll-up scroll))
	       ((65364) ;; down arrow
		(clue-scroll-down scroll)))
	     #t))) ;; yes, please stop this even propagation

(define* (clue-scroll-user-help stage scroll
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
		     %scroll-help-specs)))
    (install-stage-key-press-signals stage scroll)
    (and bg (set-background-color grid (get-colour bg)))
    grid))
