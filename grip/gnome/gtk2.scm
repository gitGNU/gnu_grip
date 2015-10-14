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


(define-module (grip gnome gtk2)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (grip reexport)
  #:use-module (grip do)

  #:export (gtk2/status-push
	    gtk2/status-pop
	    gtk2/get-text 
	    gtk2/set-text 
	    gtk2/span-message 
	    gtk2/pack-tv-cols
	    gtk2/clear-combo
	    gtk2/fill-combo
	    ;; gtk2/combo-set-tree-store
	    ;; gtk2/fill-combo-tree-store
	    gtk2/fixed-toggled
	    gtk2/hide
	    gtk2/set-visible
	    gtk2/set-sensitive
	    gtk2/set-active
	    gtk2/check-nav-tb-sensitive-needs
	    gtk2/create-completion-model
	    gtk2/combo-find-row))


(eval-when (expand load eval)
  (re-export-public-interface (ice-9 format)
			      (oop goops)
			      (gnome gobject)
			      (gnome gtk)
			      (grip do)))


;;;
;;; Status bar
;;;

(define (gtk2/status-push status-bar message source)
  (let ((context-id (gtk-statusbar-get-context-id status-bar source)))
    (gtk-statusbar-push status-bar context-id message)))

(define (gtk2/status-pop status-bar source)
  (let ((context-id (gtk-statusbar-get-context-id status-bar source)))
    (gtk-statusbar-pop status-bar context-id)))


;;;
;;; Entry and Textview
;;;

(define-method (gtk2/get-text (widget <gtk-entry>) . trim?)
  (if (null? trim?)
      (get-text widget)
      (string-trim-both (get-text widget))))

(define-method (gtk2/get-text (widget <gtk-text-view>) . trim?)
  (let* ((buffer (get-buffer widget))
	 (text (get-text buffer
			 (get-iter-at-offset buffer 0)
			 (get-iter-at-offset buffer -1)
			 #f)))
    (if (null? trim?)
	text
	(string-trim-both text))))

(define-method (gtk2/set-text (widget <gtk-entry>) text)
  (set-text widget text))

(define-method (gtk2/set-text (widget <gtk-text-view>) text)
  ;; (format #t "gtk2/set-text: ~S~%" text)
  (let ((buffer (get-buffer widget)))
    (gtk-text-buffer-set-text buffer text -1)))

(define (gtk2/span-message p-text s-text)
  (format #f "<span weight=\"bold\" size=\"larger\">~A</span>

~A"
	  p-text
	  s-text))


;;;
;;; Treeview stuff
;;;

(define (gtk2/pack-tv-cols treeview items)
  (dotimes (i (length items))
    (let* ((item (list-ref items i))
	   (acc-symb (car item))
	   (column (cadr item))
	   (renderer (caddr item))
	   (type (cadddr item)))
      (pack-start column renderer #t)
      (add-attribute column renderer type i)
      (append-column treeview column))))

(define (gtk2/clear-combo combo)
  (gtk-list-store-clear (get-model combo)))

(define (gtk2/fill-combo combo items)
  (gtk2/clear-combo combo)
  (and items
       (for-each (lambda (item)
		   (let ((item-str (cond ((string? item) item)
					 ((symbol? item) (symbol->string item))
					 ((vector? item) (vector-ref item 0)))))
		     (append-text combo item-str)))
	   items))
  combo)


;;;
;;; Combo with treestore - not working yet
;;;

#!
(define (gtk2/combo-set-tree-store combo)
  (let* ((model (gtk-tree-store-new (list <gchararray>)))
	 (renderer1 (make <gtk-cell-renderer-text>))
	 (column1 (make <gtk-tree-view-column>
		    #:title       "Activity tree" ;; (_ "Code")
		    #:clickable   #f
		    #:resizable   #f
		    #:reorderable #f
		    #:alignment   .5))
	 (to-pack `((activity ,column1 ,renderer1 "text"))))
    (gtk2/pack-tv-cols treeview to-pack)
    (set-model combo model)))

(define (gtk2/fill-combo-tree-store combo tree-items)
  (set! tree-items
	'(("admin" . ("email" "invoice"))
	  ("asys" . ("install" "upgrade"))))
  (let ((model (get-model combo)))
    (gtk-tree-store-clear model)
    (and tree-items
	 (for-each (lambda (activity)
		     (let ((a-iter (gtk-tree-store-append model #f)))
		       (set-value model a-iter 0 (car activity))
		       (for-each (lambda (subtype)
				   (let ((s-iter (gtk-tree-store-append model a-iter)))
				     (set-value model s-iter 0 subtype)))
			   (cdr activity))))
	     tree-items))
    ))
!#

(define (gtk2/fixed-toggled model iter iter-get iter-set . value)
  ;; (format #t "gtk2/fixed-toggled: ~S, ~S, ~S, ~S, ~S~%"
  ;;   model iter iter-get iter-set value)
  (let* ((old-value (iter-get model iter))
	 (new-value (if (null? value) (not old-value) (car value))))
    (iter-set model iter new-value)
    new-value))


;;;
;;; Multiple widgets same action
;;;

(define (gtk2/hide widgets)
  (for-each (lambda (widget)
	      (hide widget))
      widgets))

(define (gtk2/set-visible widgets value)
  (for-each (lambda (widget)
	      (set-visible widget value))
      widgets))

(define (gtk2/set-sensitive widgets value)
  (for-each (lambda (widget)
	      (set-sensitive widget value))
      widgets))

(define (gtk2/set-active widgets value)
  (for-each (lambda (widget)
	      (set-active widget value))
      widgets))

(define (gtk2/check-nav-tb-sensitive-needs val max
					   first-bt prev-bt next-bt last-bt
					   other-widgets)
  ;; (format #t "min: ~A max ~A~%" val max)
  (let ((nav-widgets (list first-bt prev-bt next-bt last-bt)))
    (if (or (= max 0)
	    (and (= val 1)
		 (= val max)))
	(begin
	  (if (= max 0)
	      (gtk2/set-sensitive other-widgets #f))
	  (gtk2/set-sensitive nav-widgets #f))
	(begin
	  (gtk2/set-sensitive other-widgets #t)
	  (cond ((= val max)
		 (set-sensitive first-bt #t)
		 (set-sensitive prev-bt  #t)
		 (set-sensitive next-bt  #f)
		 (set-sensitive last-bt  #f))
		((= val 1)
		 (set-sensitive first-bt #f)
		 (set-sensitive prev-bt  #f)
		 (set-sensitive next-bt  #t)
		 (set-sensitive last-bt  #t))
		(else
		 (gtk2/set-sensitive nav-widgets #t)))))))


;;;
;;; Completion
;;;

(define (gtk2/create-completion-model completion-values)
  (let ((model (gtk-list-store-new (list <gchararray>))))
    (for-each (lambda (value)
		;; Note: value is a vector since sqlite returns a list of vectors
		;;       what ever the quewry is ...
		(let ((iter (gtk-list-store-append model)))
		  ;; (format #t "~S~%" `(set-value ,model ,iter 0 ,value))
		  (set-value model iter 0 (vector-ref value 0))))
	completion-values)
    model))


;;;
;;;
;;;

(define (gtk2/combo-find-row combo value)
  (let* ((model (get-model combo))
	 (nb-rows (gtk-tree-model-iter-n-children model #f)))
    ;; (format #t "Nb of rows: ~A~%" tv-nb-rows)
    (if (and nb-rows
	     (> nb-rows 0))
	(catch 'exit
	       (lambda ()
		 (do ((row  0
			    (+ row 1)))
		     ((= row nb-rows)
		      #f)
		   (let ((iter (get-iter model row)))
		     (if (string=? (get-value model iter 0) value)
		       (throw 'exit row)))))
	       (lambda (key index)
		 index))
	#f)))
