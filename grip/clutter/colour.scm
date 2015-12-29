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
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)
  #:use-module (grip utils)
  #:use-module (grip strings)
  #:use-module (grip lists)

  #:export (get-colour
	    get-darker-colour
	    get-lighter-colour
	    get-rgba-r get-rgba-g get-rgba-b get-rgba-a
	    get-rgba-values

	    xcolour?
	    xcolour->int
	    xcolour->rgb
	    xcolour->rgba

	    %tango-butter
	    %tango-butter-light
	    %tango-butter-dark

	    %tango-orange
	    %tango-orange-light
	    %tango-orange-dark

	    %tango-chocolate
	    %tango-chocolate-light
	    %tango-chocolate-dark

	    %tango-chameleon
	    %tango-chameleon-light
	    %tango-chameleon-dark

	    %tango-sky-blue
	    %tango-sky-blue-light
	    %tango-sky-blue-dark

	    %tango-plum
	    %tango-plum-light
	    %tango-plum-dark

	    %tango-scarlet-red
	    %tango-scarlet-red-light
	    %tango-scarlet-red-dark

	    %tango-aluminium-1
	    %tango-aluminium-2
	    %tango-aluminium-3
	    %tango-aluminium-4
	    %tango-aluminium-5
	    %tango-aluminium-6))


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


;;;
;;; Xcolour [hex code] procs
;;;

;; Let's keep these for the record, but I finally discovered that
;; clutter-color-from-string does the same job [and more]! Oh well ...

(define %xcolour-regexp-1
  (make-regexp "^#[0-9a-fA-F]{3}$"))
(define %xcolour-regexp-2
  (make-regexp "^[0-9a-fA-F]{3}$"))

(define %xcolour-regexp-3
  (make-regexp "^#[0-9a-fA-F]{6}$"))
(define %xcolour-regexp-4
  (make-regexp "^[0-9a-fA-F]{6}$"))

(define (xcolour? xcolour)
  ;; ex: "#fff", "#ffffff"
  (cond ((integer? xcolour)
	 xcolour)
	((string? xcolour)
	 (let* ((xlist (if (char=? (string-ref xcolour 0) #\#)
			   (drop (string->list xcolour) 1)
			   (string->list xcolour)))
		(xlist-6 (cond ((or (regexp-exec %xcolour-regexp-1 xcolour)
				    (regexp-exec %xcolour-regexp-2 xcolour))
				(interleave xlist xlist))
			       ((or (regexp-exec %xcolour-regexp-3 xcolour)
				    (regexp-exec %xcolour-regexp-4 xcolour))
				xlist)
			       (else
				#f))))
	   (and xlist-6
		(str/read (string-append "#x"
					 (list->string xlist-6))))))
	(else
	 #f)))

(define (xcolour->int xcolour channel)
  "Return the integer value of an 8-bit color channel for XCOLOUR."
  (let* ((xcolour (xcolour? xcolour))
	 (offset (case channel
		   ((alpha) 24)
		   ((red) 16)
		   ((green) 8)
		   ((blue) 0)))
	 (mask (ash #xff offset)))
    (and xcolour
	 (ash (logand mask xcolour)
	      (- offset)))))

(define (xcolour->rgb xcolour)
  "Returns a list of red, green and blue integer values for XCOLOUR."
  (let ((xcolour (if (integer? xcolour)
		     xcolour
		     (xcolour? xcolour))))
    (and xcolour
	 (list (xcolour->int xcolour 'red)
	       (xcolour->int xcolour 'green)
	       (xcolour->int xcolour 'blue)))))

(define* (xcolour->rgba xcolour #:key (alpha 255))
  "Returns a list of red, green, blue and alpha integer values for
XCOLOUR. The default value for the optional alpha channel keyword is 255."
  (append (xcolour->rgb xcolour)
	  (list alpha)))


;;;
;;; Tango color pallete
;;;   http://tango.freedesktop.org
;;;

(define %tango-butter (xcolour->rgba "#edd400"))
(define %tango-butter-light (xcolour->rgba "#fce94f"))
(define %tango-butter-dark (xcolour->rgba "#c4a000"))

(define %tango-orange (xcolour->rgba "#f57900"))
(define %tango-orange-light (xcolour->rgba "#fcaf3e"))
(define %tango-orange-dark (xcolour->rgba "#ce5c00"))

(define %tango-chocolate (xcolour->rgba "#c17d11"))
(define %tango-chocolate-light (xcolour->rgba "#e9b96e"))
(define %tango-chocolate-dark (xcolour->rgba "#8f5902"))

(define %tango-chameleon (xcolour->rgba "#73d216"))
(define %tango-chameleon-light (xcolour->rgba "#8ae234"))
(define %tango-chameleon-dark (xcolour->rgba "#4e9a06"))

(define %tango-sky-blue (xcolour->rgba "#3465a4"))
(define %tango-sky-blue-light (xcolour->rgba "#729fcf"))
(define %tango-sky-blue-dark (xcolour->rgba "#204a87"))

(define %tango-plum (xcolour->rgba "#75507b"))
(define %tango-plum-light (xcolour->rgba "#ad7fa8"))
(define %tango-plum-dark (xcolour->rgba "#5c3566"))

(define %tango-scarlet-red (xcolour->rgba "#cc0000"))
(define %tango-scarlet-red-light (xcolour->rgba "#ef2929"))
(define %tango-scarlet-red-dark (xcolour->rgba "#a40000"))

(define %tango-aluminium-1 (xcolour->rgba "#eeeeec"))
(define %tango-aluminium-2 (xcolour->rgba "#d3d7cf"))
(define %tango-aluminium-3 (xcolour->rgba "#babdb6"))
(define %tango-aluminium-4 (xcolour->rgba "#888a85"))
(define %tango-aluminium-5 (xcolour->rgba "#555753"))
(define %tango-aluminium-6 (xcolour->rgba "#2e3436"))
