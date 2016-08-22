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


(define-module (grip clutter drop)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip keyword)
  #:use-module (grip utils)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (<clus-drop>
	    get-drop
	    set-drop))


(g-export !colour
	  !d-action)


(define get-drop #f)
(define set-drop #f)


(let ((drop-value #f))
  (set! get-drop
	(lambda () drop-value))
  (set! set-drop
	(lambda (value) (set! drop-value value))))


;;;
;;; Drop
;;;

(define-class <clus-drop> (<clutter-actor>)
  (colour #:accessor !colour
	  #:init-keyword #:colour
	  #:allocation #:virtual
	  #:slot-ref (lambda (self)
		       (get-background-color self))
	  #:slot-set! (lambda (self value)
			(set-background-color self value)))
  (d-action #:accessor !d-action #:init-keyword #:d-action #:init-value identity))

(define-method (initialize (self <clus-drop>) initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-drop>)))
    (if (null? virtual-kw)
	(next-method)
	(begin
	  (next-method self kw)
	  (let-keywords virtual-kw #t
			((colour #f))
			(when colour (set! (!colour self) colour)))))
    (let ((drop-action (clutter-drop-action-new)))
      (set-reactive self #t)
      (set-opacity self 96)
      (add-action self drop-action)
      (connect drop-action
	       'over-in
	       (lambda (action actor)
		 (save-easing-state actor)
		 (set-opacity actor 196)
		 (restore-easing-state actor)))
      (connect drop-action
	       'over-out
	       (lambda (action actor)
		 (save-easing-state actor)
		 (set-opacity actor 96)
		 (restore-easing-state actor)))
      (connect drop-action
	       'drop
	       (lambda (action actor x y)
		 (set-drop `(,actor ,x ,y))
		 ((!d-action self) self))))))

