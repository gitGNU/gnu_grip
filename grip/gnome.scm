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


(define-module (grip gnome)
  #:use-module (grip reexport)
  #:use-module (grip gnome colours)
  #:use-module (grip gnome key-press)
  #:use-module (grip gnome gtk2)
  #:use-module (grip gnome filechooser)
  #:use-module (grip gnome mdialogs))


(eval-when (expand load eval)
  (re-export-public-interface (grip gnome colours)
			      (grip gnome key-press)
			      (grip gnome gtk2)
			      (grip gnome filechooser)
			      (grip gnome mdialogs)))
