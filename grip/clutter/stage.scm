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


(define-module (grip clutter stage)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip keyword)
  #:use-module (grip utils)
  #:use-module (grip clutter utils)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  
  #:export (<clus-stage>
	    
	    clus-stage-get-item-on-hold
	    clus-stage-set-item-on-hold))


(g-export !colour
	  get-children-named
	  get-child-named)


(define clus-stage-get-item-on-hold #f)
(define clus-stage-set-item-on-hold #f)


(eval-when (expand load eval)
  (let ((toolbar-item-on-hold #f))
    (set! clus-stage-get-item-on-hold
	  (lambda () toolbar-item-on-hold))
    (set! clus-stage-set-item-on-hold
	  (lambda (val) (set! toolbar-item-on-hold val)))))


;;;
;;; Stage
;;;

(define-class <clus-stage> (<clutter-stage>)
  (colour #:accessor !colour
	  #:init-keyword #:colour
	  #:allocation #:virtual
	  #:slot-ref (lambda (obj)
		       (get-background-color obj))
	  #:slot-set! (lambda (obj value)
			(set-background-color obj value))))

(define-method (initialize (self <clus-stage>) initargs)
  ;; (dimfi "initialize <clus-stage>" initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-stage>)))
    (if (null? virtual-kw)
	(next-method)
	(begin
	  (next-method self kw)
	  (let-keywords virtual-kw #t
			((colour #f))
			(when colour (set! (!colour self) colour)))))
    (connect self
	     'delete-event
	     (lambda (. args)
	       (clutter-main-quit)
	       #t)) ;; stops the event to be propagated
    #;(connect self
	     'button-press-event
	     (lambda (i e)
	       (dimfi "press on stage")
	       #f)) ;; do not stop the event to be propagated
    (connect self
	     'button-release-event
	     (lambda (actor event)
	       (when (clus-stage-get-item-on-hold)
		 (clus-stage-set-item-on-hold #f))
	       #f)))) ;; do not stop the event from being propagated

(define-method (get-children-named (self <clus-stage>) name)
  (clus-get-children-named self name))

(define-method (get-child-named (self <clus-stage>) name)
  (clus-get-child-named self name))
