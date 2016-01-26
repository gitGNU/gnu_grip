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

;; Needs review.  the tooltip should know its parent and grab to it,
;; let's see.

;;; Code:


(define-module (grip clutter tooltip)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome gw gdk) ;; gdk-pixbuf
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  ;; #:use-module (grip clutter colour)
  ;; #:use-module (grip clutter globals)
  ;; #:use-module (grip clutter constraint)
  ;; #:use-module (grip clutter gravity)
  ;; #:use-module (grip clutter actor)
  ;; #:use-module (grip clutter image)
  ;; #:use-module (grip clutter label)
  ;; #:use-module (grip clutter stage)
  #:use-module (grip clutter text)
  ;; #:use-module (grip clutter utils)

  #:export (<clus-tooltip>))


#;(g-export !text
	  !font
	  !bg
	  !fg
	  !border)


;;;
;;; Tooltip
;;;

(define-class <clus-tooltip> (<clus-text>))

(define-method (initialize (self <clus-tooltip>) initargs)
  (next-method))

#!

;; this is because of a goops [known] bug which we hope will be solved
;; soon, but in the mean time, accessors, getters and setters are not
;; proprerly being (re)computed for subclasses.  to circumvent this
;; problem, just redine them [all] and call (next-method).

(define-method ((setter !text) (self <clus-tooltip>) text)
  (next-method))

(define-method ((setter !font) (self <clus-tooltip>) font)
  (next-method))

(define-method ((setter !bg) (self <clus-tooltip>) bg)
  (next-method))

(define-method ((setter !fg) (self <clus-tooltip>) fg)
  (next-method))

(define-method ((setter !border) (self <clus-tooltip>) border)
  (next-method))

!#
