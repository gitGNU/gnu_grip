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


(define-module (grip clutter text)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip keyword)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter gravity)
  #:use-module (grip clutter orientation)
  #:use-module (grip clutter actor)
  #:use-module (grip clutter label)
  #:use-module (grip clutter apple)
  #:use-module (grip clutter grid)

  #:export (<clus-text>))


(g-export !text
	  !font
	  !bg
	  !fg

	  ;get-min-width
	  ;get-min-height
	  ;get-min-size
	  ;get-size
	  ;set-size
	  
	  get-text
	  set-text
	  get-font-name
	  set-font-name
	  get-use-markup
	  set-use-markup
	  set-markup
	  get-color
	  set-color

	  ;!colour
	  ;!border
	  ;!orientation
	  ;!gravity
	  ;!gravity-pole
	  ;!gravity-force
	  ;!gravity-resistance-north
	  ;!gravity-resistance-west
	  #;!gravity-resistance-mode)


;;;
;;; Text
;;;

(define-class <clus-text> (<clus-apple>)
  (text #:accessor !text
	#:init-keyword #:text
	#:allocation #:virtual
	#:slot-ref (lambda (self)
		     (get-text (get-first-child self)))
	#:slot-set! (lambda (self value)
		      (set-markup (get-first-child self) value)))
  (font #:accessor !font
	#:init-keyword #:font
	#:allocation #:virtual
	#:slot-ref (lambda (self)
		     (get-font-name (get-first-child self)))
	#:slot-set! (lambda (self value)
		      (set-font-name (get-first-child self) value)))
  ;; <clus-apple> has colour but because of fg [foreground], added bg as
  ;; well, all these are virtual anyway so no big deal.
  (bg #:accessor !bg
      #:init-keyword #:bg
      #:allocation #:virtual
      #:slot-ref (lambda (self)
		   (get-background-color self))
      #:slot-set! (lambda (self value)
		    (set-background-color self (get-colour value))))
  (fg #:accessor !fg
      #:init-keyword #:fg
      #:allocation #:virtual
      #:slot-ref (lambda (self)
		   (get-color (get-first-child self)))
      #:slot-set! (lambda (self value)
		    (set-color (get-first-child self) (get-colour value)))))

(define-method (initialize (self <clus-text>) initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-text>)))
    (let ((clutter-text (make <clutter-text>)))
      (if (null? virtual-kw)
	  (next-method)
	  (begin
	    (next-method self kw)
	    (let-keywords virtual-kw #t
			  ((text #f)
			   (font #f)
			   (bg #f)
			   (fg #f))
			  (when bg (set! (!bg self) bg))
			  (if fg
			      (set-color clutter-text (get-colour fg))
			      (set-color clutter-text (get-colour *clus-label-fg*)))
			  (if font
			      (set-font-name clutter-text font)
			      (set-font-name clutter-text *clus-label-font*))
			  (when text (set-markup clutter-text text)))))
      ;; if I uncomment the following, clutter gets lost about the instance
      ;; size, backgroung-color.  let's keep the line to remember that!
      ;; (set-easing-duration self 0) ;; <clus-apple> instances default is 1000
      #;(connect clutter-text
	       'text-changed
	       (lambda (actor)
		 (set-size self
			   (get-min-width self)
			   (get-min-height self))
      #f))
      (add-child self clutter-text)
      #;(add-child self clutter-text 0 0 1 1)
      #;(emit clutter-text 'text-changed)))) ;; we must trigger set-size

(define-method (get-text (self <clus-text>))
  (get-text (get-first-child self)))

(define-method (set-text (self <clus-text>) value)
  (set-text (get-first-child self) value))

(define-method (get-font-name (self <clus-text>))
  (get-font-name (get-first-child self)))

(define-method (set-font-name (self <clus-text>) font)
  (set-font-name (get-first-child self) font))

(define-method (get-use-markup (self <clus-text>))
  (get-use-markup (get-first-child self)))

(define-method (set-use-markup (self <clus-text>) value)
  (set-use-markup (get-first-child self) value))

(define-method (set-markup (self <clus-text>) value)
  ;; don't know why, it needs 2 calls otherwise pango renders sort of
  ;; with delay (part of the text seems still using the previous span
  ;; foreground setting, weird, but it probably is solved in recent
  ;; clutter version.
  (set-markup (get-first-child self) value)
  (set-markup (get-first-child self) value))

(define-method (get-color (self <clus-text>))
  (get-color (get-first-child self)))

(define-method (set-color (self <clus-text>) value)
  (set-color (get-first-child self) value))

#;(define-method (get-layout (self <clus-text>))
  (get-layout (get-first-child self)))

;; not defined [yet] in guile-clutter
#;(define-method (get-layout-offset (self <clus-text>))
  (get-layout-offset (get-first-child self)))
