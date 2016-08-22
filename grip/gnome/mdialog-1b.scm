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

;; This code needs review and/or be rewritten: it is imported almost 'as
;; is' from 'common', grip's ancestor, started while using guile-1.6, a
;; past where I could not trust the module system, goops, and myself [:)
;; as a schemer].  But I want grip-0.1.0 and kise-0.9.5 ready asap to
;; talk to svannah, gitorious has been murdered.

;;; Code:


(define-module (grip gnome mdialog-1b)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glade)
  #:use-module (gnome gtk)
  #:use-module (grip reexport)
  #:use-module (grip utils)
  #:use-module (grip gnome gtk2)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (md1b/select-gui))


(eval-when (expand load eval)
  (re-export-public-interface (ice-9 format)
			      (oop goops)
			      (gnome gtk)
			      (gnome glade)
			      (gnome gobject)
			      (grip utils)
			      (grip gnome gtk2))
  (unless (storage-get 'mdgladefile)
    (let ((gripgnomedir (dirname (search-path %load-path "grip/gnome/mdialog-1b.scm"))))
      (storage-set 'mdgladefile (string-append gripgnomedir "/glade/mdialogs.glade")))))


;;;
;;; Globals
;;;

(define *md1b-widget* #f)


;;;
;;; Dialog
;;;

(define-class <md1b-widget> ()
  (xml-code #:accessor xml-code #:init-keyword #:xml-code #:init-value #f)
  (dialog #:accessor dialog #:init-keyword #:dialog #:init-value #f)
  (image #:accessor image #:init-keyword #:image #:init-value #f)
  (label #:accessor label #:init-keyword #:label #:init-value #f)
  (ok-bt #:accessor ok-bt #:init-keyword #:ok-bt #:init-value #f)
  (ok-callback #:accessor ok-callback #:init-keyword #:ok-callback #:init-value #'identity))

(define (md1b/make-dialog)
  (let* ((xml (glade-xml-new (storage-get 'mdgladefile) #f "md1b"))
	 (md1b-dialog (get-widget xml "md1b"))
	 (img (get-widget xml "md1b/image"))
	 (lb (get-widget xml "md1b/label"))
	 (ok (get-widget xml "md1b/ok_bt"))
	 (md1b-widget (make <md1b-widget>
			#:xml-code xml
			#:dialog md1b-dialog
			#:image img
			#:label lb
			#:ok-bt ok)))
    ;; (md1b/translate-dialog md1b-widget)
    (connect md1b-dialog
	     'destroy-event
	     (lambda (widget event)
	       (dimfi "!!! destroy with parent ... is not called !!!")
	       (set! *md1b-widget* #f)
	       #f))
    (connect md1b-dialog
	     'delete-event
	     (lambda (widget event)
	       (set! *md1b-widget* #f)
	       #f))
    (connect ok
	     'clicked
	     (lambda (button)
	       ((ok-callback md1b-widget))
	       (set-modal md1b-dialog #f)
	       (hide md1b-dialog)))
    md1b-widget))


(define (md1b/select-gui-1 md1b-widget parent title icon p-text s-text ok-callb)
  (let ((md1b-dialog (dialog md1b-widget)))
    (if parent
	(set-transient-for md1b-dialog parent))
    (set-destroy-with-parent md1b-dialog #f)
    (set-title md1b-dialog title)
    (if icon (set-from-stock (image md1b-widget) (gtk-stock-id icon) 6))
    (set-markup (label md1b-widget) (gtk2/span-message p-text s-text))
    (set! (ok-callback md1b-widget) ok-callb)
    (set-modal md1b-dialog #t)
    (set-keep-above md1b-dialog #t)
    (show-all md1b-dialog)))

(define* (md1b/select-gui parent title p-text s-text ok-callb #:optional (icon #f))
  (let ((md1b-widget (or *md1b-widget*
			 (let ((md1b-widget (md1b/make-dialog)))
			   (set! *md1b-widget* md1b-widget)
			   md1b-widget))))
    (md1b/select-gui-1 md1b-widget parent title icon p-text s-text ok-callb)))


;;;
;;; Translation
;;;

(define (md1b/translate-dialog md1b-widget)
  (let ((xml (xml-code md1b-widget)))
    ;; (set-title (dialog md1b-widget) (_ "Warning"))
    #f))


#!

(md1b/select-gui #f
		 "Information"
		 "Information"
		 "The weather is crap, don't forget your umbrella!"
		 (lambda () (display "Ok"))
		 'dialog-info)

!#
