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

;; this needs review, and a better integration with clutter.  it's not
;; even using <clutter-color>, but i needed something quick.

;;; Code:


(define-module (grip clutter colour)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)

  #:export (get-colour
	    get-darker-colour
	    get-lighter-colour
	    get-rgba-r get-rgba-g get-rgba-b get-rgba-a
	    get-rgba-values))


(define* (get-colour colour #:key (alpha 255) (fallback "Chocolate"))
  (if (string? colour)
      (let ((colour (or (clutter-color-from-string colour)
			(begin
			  (display (string-append "WARNING: Undefined colour " colour
						  ", using " fallback " instead\n"))
			  (clutter-color-from-string fallback)))))
	(if (= alpha 255)
	    colour
	    (append (drop-right colour 1) (list alpha))))
      colour))

(define* (get-darker-colour colour #:key (ratio 0.4))
  (let ((channels (if (string? colour) (get-colour colour) colour)))
    ;; nothing on the alpha channel
    (append (map (lambda (channel)
		   (- channel (inexact->exact (round (* channel ratio)))))
	      (drop-right channels 1))
	    (last-pair channels))))

(define* (get-lighter-colour colour #:key (ratio 0.4))
  (let ((channels (if (string? colour) (get-colour colour) colour)))
    ;; nothing on the alpha channel
    (append (map (lambda (channel)
		   (+ channel (inexact->exact (round (* channel ratio)))))
	      (drop-right channels 1))
	    (last-pair channels))))

(define (get-channel-pos channel)
  (case channel
    ((red) 0)
    ((green) 1)
    ((blue) 2)
    ((alpha) 3)))

(define* (get-rgba-r colour #:key (float #f))
  (let ((r (list-ref colour (get-channel-pos 'red))))
    (if float (/ r 255) r)))

(define* (get-rgba-g colour #:key (float #f))
  (let ((g (list-ref colour (get-channel-pos 'green))))
    (if float (/ g 255) g)))

(define* (get-rgba-b colour #:key (float #f))
  (let ((b (list-ref colour (get-channel-pos 'blue))))
    (if float (/ b 255) b)))

(define* (get-rgba-a colour #:key (float #f))
  (let ((a (list-ref colour (get-channel-pos 'alpha))))
    (if float (/ a 255) a)))

(define* (get-rgba-values colour #:key (float #f))
  (values (get-rgba-r colour #:float float)
	  (get-rgba-g colour #:float float)
	  (get-rgba-b colour #:float float)
	  (get-rgba-a colour #:float float)))
