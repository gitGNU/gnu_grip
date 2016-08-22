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


(define-module (grip clutter bin)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome clutter)
  #:use-module (grip utils)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip keyword)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter apple)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<clus-bin>))


(define-class <clus-bin> (<clus-apple>))

(define-method (initialize (self <clus-bin>) initargs)
  (next-method)
  (set-layout-manager self
		      (make <clutter-bin-layout>)))
