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


(define-module (grip clutter examples utils)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter align)
  #:use-module (grip clutter examples clue)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (test-gparam-spec-color))


(define %text-labels
  "attach:
     span:
   expand:
    align:")

(define %text-values
  "red
green
blue
alpha")


(define (test-gparam-spec-color)
  (let* ((bin (make <clutter-actor>))
	 (bin-l (make <clutter-box-layout> #:orientation 'horizontal #:spacing 4))
	 (text-labels (make <clutter-text> #:text %text-labels))
	 (text-values (make <clutter-text> #:text %text-values))
	 #;(x-align-property (find-property <clutter-actor> 'x-align)))
    (set-layout-manager bin bin-l)
    (add-child bin text-labels)
    (set-line-alignment text-labels 'right)
    (add-child bin text-values)
    bin))
