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


(define-module (grip clutter utils)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome clutter)
  #:use-module (grip utils)

  #:export (mset
	    dimfi-key-press-event
	    get-icon-filename
	    center-on-target
	    clus-get-children-named
	    clus-get-child-named))


(define (mset actor propvals)
  (for-each (lambda (propval)
	      (match propval
		((property value)
		 (set actor property value))))
      propvals))

(define (dimfi-key-press-event stage event)
  (let ((key-sym (get-key-symbol event))
	(key-code (get-key-code event))
	(key-unicode (get-key-unicode event)))
    (format #t "Key press event:
     Stage: ~S
     Event: ~S
     Key ->
    symbol: ~S
      code: ~S
   unicode: 0x~x~%"
          stage
          event
	  key-sym
	  key-code
	  key-unicode #;(utf8->string key-unicode))))

(define (get-icon-filename basename)
  (string-append (dirname (search-path %load-path "grip/clutter/utils.scm"))
		 "/icons/gnome/32x32/actions/"
		 basename))

(define (center-on-target target actor)
  (receive (target-w target-h)
      (get-size target)
    (receive (actor-w actor-h)
	(get-size actor)
      (set-position actor
		    (/ (- target-w actor-w) 2)
		    (/ (- target-h actor-h) 2)))))

(define (clus-get-children-named parent name)
  (filter-map (lambda (child)
		(let ((its-name (get-name child)))
		  (and its-name
		       (string=? its-name name)
		       child)))
      (get-children parent)))

(define (clus-get-child-named parent name)
  (let ((kids (clus-get-children-named parent name)))
    (match kids
      (() #f)
      ((kid) kid)
      ((kid1 ... kidn)
       (dimfi "WARNING: I found several kids named" name)
       kid1))))
