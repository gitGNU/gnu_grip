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


(define-module (grip clutter)
  #:use-module (gnome-2)
  #:use-module (grip reexport)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip utils)
  #:use-module (grip clutter utils)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter animation)
  #:use-module (grip clutter align)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter gravity)
  #:use-module (grip clutter orientation)
  #:use-module (grip clutter stage)  
  #:use-module (grip clutter actor)
  #:use-module (grip clutter image)
  #:use-module (grip clutter label)
  #:use-module (grip clutter apple)
  #:use-module (grip clutter bin)
  #:use-module (grip clutter box)
  #:use-module (grip clutter grid)
  #:use-module (grip clutter text)
  #:use-module (grip clutter tooltip)
  #:use-module (grip clutter toolbar-item)
  #:use-module (grip clutter toolbar)
  #:use-module (grip clutter bouncer)
  #:use-module (grip clutter clock)
  #:use-module (grip clutter drag)
  #:use-module (grip clutter drop))

  
(eval-when (expand load eval)
  (re-export-public-interface (grip goops)
			      (grip utils)
			      (grip clutter utils)
			      (grip clutter colour)
			      (grip clutter globals)
			      (grip clutter animation)
			      (grip clutter align)
			      (grip clutter constraint)
			      (grip clutter gravity)
			      (grip clutter orientation)
			      (grip clutter stage)  
			      (grip clutter actor)
			      (grip clutter image)
			      (grip clutter label)
			      (grip clutter apple)
			      (grip clutter bin)
			      (grip clutter box)
			      (grip clutter grid)
			      (grip clutter text)
			      (grip clutter tooltip)
			      (grip clutter toolbar-item)
			      (grip clutter toolbar)
			      (grip clutter bouncer)
			      (grip clutter clock)
			      (grip clutter drag)
			      (grip clutter drop)))
