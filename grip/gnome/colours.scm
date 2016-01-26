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

;;; Code:


(define-module (grip gnome colours)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:export (%palette
	    colour-set
	    colour-set-bg
	    colour-set-border
	    colour-set-fg
	    colour-set-name
	    colour-set-ids
	    colour-set-names))


(define %palette #f)

(eval-when (expand load eval)
  (let ((whereami (dirname (search-path %load-path
					"grip/gnome/colours.scm"))))
    (set! %palette
	 ;; (name . (background border foreground))
	  (call-with-input-file
	      (string-append whereami "/colour-sets")
	    read))))


(define (colour-set id)
  (assq-ref %palette id))

(define (colour-set-bg id)
  (list-ref (colour-set id) 0))

(define (colour-set-border id)
  (list-ref (colour-set id) 1))

(define (colour-set-fg id)
  (list-ref (colour-set id) 2))

(define (colour-set-name id)
  (list-ref (colour-set id) 3))

(define (colour-set-ids)
  (filter-map (lambda (set) (car set))
      %palette))

(define (colour-set-names)
  (filter-map (lambda (entry) (colour-set-name (car entry)))
      %palette))


;;;
;;; 
;;;


#!

(use-modules (grip gnome colours))
(reload-module (resolve-module '(grip gnome colours)))

%palette

(colour-set 1)
(colour-set-bg 1)
(colour-set-border 1)
(colour-set-fg 1)
(colour-set-name 1)

(colour-set-ids)
(colour-set-names)


;; Gtk widget states

'((normal GTK_STATE_NORMAL 0)
  (active GTK_STATE_ACTIVE 1)
  (prelight GTK_STATE_PRELIGHT 2) 
  (selected GTK_STATE_SELECTED 3)
  (insensitive GTK_STATE_INSENSITIVE 4))

!#
