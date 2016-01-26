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


(define-module (grip clutter image)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gw gdk)  ;; gdk-pixbuf
  #:use-module (gnome clutter)
  #:use-module (grip g-export)

  #:export (<clus-image>))


(g-export !filename
	  !pixbuf

	  get-width
	  get-height
	  get-size)


(define-class <clus-image> (<clutter-image>)
  (filename #:accessor !filename #:init-keyword #:filename #:init-value #f)
  (pixbuf #:accessor !pixbuf #:init-keyword #:pixbuf #:init-value #f))

(define-method (initialize (self <clus-image>) initargs)
  (next-method)
  (let ((pixbuf (gdk-pixbuf-new-from-file (!filename self))))
    (set! (!pixbuf self) pixbuf)
    (set-data self
	      (get-pixels pixbuf)
	      (if (get-has-alpha pixbuf) 'rgba-8888 'rgb-888)
	      (get-width pixbuf)
	      (get-height pixbuf)
	      (get-rowstride pixbuf))
    self))

(define-method (get-width (self <clus-image>))
  (get-width (!pixbuf self)))

(define-method (get-height (self <clus-image>))
  (get-height (!pixbuf self)))

(define-method (get-size (self <clus-image>))
  (let ((pixbuf (!pixbuf self)))
    (values (get-width pixbuf) (get-height pixbuf))))
