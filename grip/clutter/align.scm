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


(define-module (grip clutter align)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)
  #:use-module (grip lists)

  #:export (%clus-align-modes
	    clus-current-align-mode
	    clus-next-align-mode))


(define %clus-align-modes #f)
(define clus-current-align-mode #f)
(define clus-next-align-mode #f)

(eval-when (expand load eval)
  (let* ((i 0)
	 (a-modes (vector->list (genum-class->value-table <clutter-actor-align>)))
	 (a-mode-symbols (map car a-modes))
	 (nb-a-modes (length a-modes)))
    (set! %clus-align-modes
	  a-modes)
    (set! clus-current-align-mode
	  (lambda ()
	    (car (list-ref a-modes i))))
    (set! clus-next-align-mode
	  (lambda* (#:key (mode #f))
	    (if mode
		(list-ref a-mode-symbols
			  (modulo (1+
				   (list-pos mode a-mode-symbols eq?))
				  nb-a-modes))
		(begin
		  (set! i (modulo (1+ i) nb-a-modes))
		  (car (list-ref a-modes i))))))))
