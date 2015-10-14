;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2011 - 2015
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


(define-module (grip gnome mdialog-2b)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome gtk)
  #:use-module (gnome glade)
  #:use-module (gnome gobject)
  #:use-module (grip reexport)
  #:use-module (grip utils)
  #:use-module (grip gnome gtk2)

  #:export (md2b/select-gui))


(eval-when (expand load eval)
  (re-export-public-interface (ice-9 format)
			      (oop goops)
			      (gnome gtk)
			      (gnome glade)
			      (gnome gobject)
			      (grip utils)
			      (grip gnome gtk2))
  (unless (storage-get 'mdgladefile)
    (let ((gripgnomedir (dirname (search-path %load-path "grip/gnome/mdialog-2b.scm"))))
      (storage-set 'mdgladefile (string-append gripgnomedir "/glade/mdialogs.glade")))))


;;;
;;; Globals
;;;

(define *md2b-widget* #f)


;;;
;;; Dialog
;;;

(define-class <md2b-widget> ()
  (xml-code #:accessor xml-code #:init-keyword #:xml-code #:init-value #f)
  (dialog #:accessor dialog #:init-keyword #:dialog #:init-value #f)
  (image #:accessor image #:init-keyword #:image #:init-value #f)
  (label #:accessor label #:init-keyword #:label #:init-value #f)
  (ok-bt #:accessor ok-bt #:init-keyword #:ok-bt #:init-value #f)
  (ok-callback #:accessor ok-callback #:init-keyword #:ok-callback #:init-value #'identity)
  (cancel-bt #:accessor cancel-bt #:init-keyword #:cancel-bt #:init-value #f)
  (cancel-callback #:accessor cancel-callback #:init-keyword #:cancel-callback #:init-value #'identity))

(define (md2b/make-dialog)
  (let* ((xml (glade-xml-new (storage-get 'mdgladefile) #f "md2b"))
	 (md2b-dialog (get-widget xml "md2b"))
	 (img (get-widget xml "md2b/image"))
	 (lb (get-widget xml "md2b/label"))
	 (ok (get-widget xml "md2b/ok_bt"))
	 (cancel (get-widget xml "md2b/cancel_bt"))
	 (md2b-widget (make <md2b-widget>
			#:xml-code xml
			#:dialog md2b-dialog
			#:image img
			#:label lb
			#:ok-bt ok
			#:cancel-bt cancel)))
    ;; (md2b/translate-dialog md2b-widget)
    (connect md2b-dialog
	     'delete-event
	     (lambda (widget event)
	       ;; (format #t "Delete event triggered~%")
	       ((cancel-callback md2b-widget))
	       (set! *md2b-widget* #f)
	       #f))
    (connect ok
	     'clicked
	     (lambda (button)
	       ((ok-callback md2b-widget))
	       (set-modal md2b-dialog #f)
	       (hide md2b-dialog)))
    (connect cancel
	     'clicked
	     (lambda (button)
	       ((cancel-callback md2b-widget))
	       (set-modal md2b-dialog #f)
	       (hide md2b-dialog)))
    md2b-widget))


;;;
;;; API
;;;

(define (md2b/select-gui-1 md2b-widget
			   parent title icon p-text s-text ok-callb cancel-callb)
  (let ((md2b-dialog (dialog md2b-widget)))
    (if parent (set-transient-for md2b-dialog parent))
    (set-destroy-with-parent md2b-dialog #f)
    (set-title md2b-dialog title)
    ;; (genum->value (make <gtk-icon-size> #:value 'dialog)) = 6
    (if icon (set-from-stock (image md2b-widget) (gtk-stock-id icon) 6))
    (set-markup (label md2b-widget) (gtk2/span-message p-text s-text))
    (set! (ok-callback md2b-widget) ok-callb)
    (set! (cancel-callback md2b-widget) cancel-callb)
    (set-modal md2b-dialog #t)
    (set-keep-above md2b-dialog #t)
    (show-all md2b-dialog)))

(define* (md2b/select-gui parent title p-text s-text ok-callb cancel-callb
			  #:optional (icon #f))
  (let ((md2b-widget (or *md2b-widget*
			 (let ((md2b-widget (md2b/make-dialog)))
			   (set! *md2b-widget* md2b-widget)
			   md2b-widget))))
    (md2b/select-gui-1 md2b-widget
		       parent title icon p-text s-text ok-callb cancel-callb)))


;;;
;;; Translation
;;;

(define (md2b/translate-dialog md2b-widget)
  (let ((xml (xml-code md2b-widget)))
    ;; (set-title (dialog md2b-widget) (_ "Warning"))
    #f))


#!

(md2b/select-gui #f
		 "My title"
		 "Incorrect code"
		 "Incorrect code: unkown variable bibi"
		 (lambda () (display "Ok"))
		 (lambda () (display "Cancel")))

!#
