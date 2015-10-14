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

;;       <- stands for ->
;; clue                   clutter example
;; clues                  clutter examples set
;; clus                   clutter support

;;; Code:


(define-module (grip clutter examples grid)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome clutter)
  #:use-module (grip strings)
  #:use-module (grip utils)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter align)
  #:use-module (grip clutter bin)
  #:use-module (grip clutter grid)
  #:use-module (grip clutter utils)
  #:use-module (grip clutter examples clue)

  #:export (clue-grid-fill-grid
	    clue-grid-user-help))


(define %clue-grid-labels
  ;; left, top [attach], width, height [span]
  "Attach\nSpan\nExpand\nAlign")

(define %clue-grid-values-fstr
  ": ~A, ~A\n: ~A, ~A\n: ~A, ~A\n: ~A, ~A")

(define (clue-grid-child-button-release-cb child event t-values)
  (let ((button (get-button event))
	(modifiers (gflags->symbol-list (get-state event))))
    (match (get-child-meta-values child)
      ((left top width height x-expand y-expand x-align y-align)
       (case button
	 ((1) (if (memq 'shift-mask modifiers)
		  (set child 'x-expand (not x-expand))
		  (set child 'x-align (clus-next-align-mode #:mode x-align))))
	 ((3) (if (memq 'shift-mask modifiers)
		  (set child 'y-expand (not y-expand))
		  (set child 'y-align (clus-next-align-mode #:mode y-align)))))))))

(define (clue-grid-child-text-changed-cb child t-values)
  (match (get-child-meta-values child)
    ((left top width height x-expand y-expand x-align y-align)
     (set-markup t-values
		 (format #f "~?" %clue-grid-values-fstr
			 (list left
			       top
			       width
			       height
			       (str/span (if x-expand "1" "0") "foreground" "#98fb98")
			       (str/span (if y-expand "1" "0") "foreground" "LightSkyBlue")
			       (str/span (symbol->string x-align) "foreground" "LightSkyBlue")
			       (str/span (symbol->string y-align) "foreground" "#98fb98")))))))

(define (clue-grid-make-child bg fg)
  (let* ((bg (get-colour bg))
	 (fg (get-colour fg))
	 (child (make <clus-bin>
		  #:colour bg
		  #:x-align 'fill
		  #:y-align 'fill
		  #:x-expand #t
		  #:y-expand #t))
	 (t-grid (make <clus-grid>
		   #:row-spacing 2
		   #:column-spacing 4
		   #:row-homogeneous #t
		   #:column-homogeneous #f))
	 (t-labels (make <clutter-text>
		     #:text %clue-grid-labels
		     #:color fg))
	 (t-values (make <clutter-text>
		     #:text ""
		     #:color fg)))
    (add-child child t-grid)
    #;(set-line-alignment t-labels 'right)
    (add-child t-grid t-labels 0 0 1 1)
    (add-child t-grid t-values 1 0 1 1)
    (set-reactive child #t)
    (connect child
	     'enter-event
	     (lambda (a e)
	       (set-opacity child 196)
	       #f)) ;; do not stop the event propagation
    (connect child
	     'leave-event
	     (lambda (a e)
	       (set-opacity child 255)
	       #f)) ;; do not stop the event propagation
    (connect child
	     'button-release-event
	     (lambda (a e)
	       (clue-grid-child-button-release-cb child e t-values)
	       (clue-grid-child-text-changed-cb child t-values)
	       #t)) ;; stop the event propagation
    (list child t-values)))

(define %grid-children-pos
  ;; left top width height
  '((0 0 1 1) ;; 3 on the first line
    (1 0 1 1)
    (2 0 1 1)
    (0 1 1 1) ;; 2 on the second
    (1 1 2 1)
    (0 2 3 1) ;; 1 on the third
    (0 3 2 2) ;; a verticle span
    (2 3 1 1) ;;   its 2 friends aside
    (2 4 1 1)))

(define (clue-grid-fill-grid grid child-bg child-fg)
  (let loop
      ((in %grid-children-pos))
    (match in
      (((left top width height) . rest)
       (match (clue-grid-make-child child-bg child-fg)
	 ((child t-values)
	  (add-child grid child left top width height)
	  (clue-grid-child-text-changed-cb child t-values)))
       (loop (cdr in)))
      (() 'done))))


;;;
;;; User help stuff
;;;

(define %help-col1
  '("Attach"
    "Span"
    "Expand"
    "Align"))

(define %help-col2
  `(": left, top"
    ": width, height"
    ;; once we use span, we have to span 'back' to the fg,
    ;; which is a bug in cluter 1.12.2 [or in pango].
    (,(string-append (str/span ": " "foreground" "Gainsboro")
		     (str/span "Shift-Left" "foreground" "#98fb98")
		     (str/span ", " "foreground" "Gainsboro")
		     (str/span "Shift-Right" "foreground" "LightSkyBlue")))
    (,(string-append (str/span ": " "foreground" "Gainsboro")
		     (str/span "Left" "foreground" "LightSkyBlue")
		     (str/span ", " "foreground" "Gainsboro")
		     (str/span "Right" "foreground" "#98fb98")))))

(define %help-col3
  '(""
    ""
    ("[")
    ("[")))

(define %help-col4
  '(" to tween "))

(define %help-col5
  '(""
    ""
    ("]")
    ("]")))

(define (clue-grid-user-help-get-col grid items bg fg)
  (map (lambda (item)
	 (let ((markup (if (pair? item)
			   (str/span (car item) "foreground" "Chocolate")
			   item))
	       (text (make <clutter-text> #:color fg)))
	   (if (pair? item)
	       (set-markup text markup)
	       (set-text text item))
	   (set text 'x-align 'start)
	   (if bg
	       (let ((actor (make <clutter-actor>
			      #:background-color bg)))
		 (set actor 'x-align 'start)
		 (add-child actor text)
		 actor)
	       text)))
    items))

(define (clue-grid-user-help-add-children grid bg fg)
  (let* ((grid-layout (!grid-layout grid))
	 (fg (if fg (get-colour fg) (get-colour "Gainsboro")))
	 (col1 (clue-grid-user-help-get-col grid %help-col1 bg fg))
	 (col2 (clue-grid-user-help-get-col grid %help-col2 bg fg))
	 (col3 (clue-grid-user-help-get-col grid %help-col3 bg fg))
	 (col4 (clue-grid-user-help-get-col grid %help-col4 bg fg))
	 (col5 (clue-grid-user-help-get-col grid %help-col5 bg fg)))
    (do ((i 0 (+ i 1))
	 (i-col1 col1 (cdr i-col1))
	 (i-col2 col2 (cdr i-col2))
	 (i-col3 col3 (cdr i-col3))
	 (i-col5 col5 (cdr i-col5)))
	((null? i-col1)
	 (let ((tween (car col4)))
	   (set tween 'y-align 'center)
	   (add-child grid tween 3 2 1 2)))
      (add-child grid (car i-col1) 0 i 1 1)
      (add-child grid (car i-col2) 1 i 1 1)
      (add-child grid (car i-col3) 2 i 1 1)
      (add-child grid (car i-col5) 4 i 1 1))))

(define* (clue-grid-user-help #:key (bg #f)
			      (cbg #f)
			      (cfg (get-colour "Gainsboro")))
  (let ((grid (make <clus-grid>
		     #:orientation 'horizontal
		     #:row-spacing 2
		     #:column-spacing 4
		     #:row-homogeneous #t
		     #:column-homogeneous #f)))
    (when bg (set-background-color grid (get-colour bg)))
    (clue-grid-user-help-add-children grid cbg cfg)
    grid))


#!

;;;
;;; Left as an introspection example
;;;

(describe (find-property <clutter-actor> 'x-align))
->
<<gparam-enum> x-align 3edcb40> - instance of <gparam-enum>
  slots and values are:
    gtype-instance = 64247328
    name = x-align
    nick = "X Alignment"
    blurb = "The alignment of the actor on the X axis within its allocation"
    flags = 3
    enum-type = #<<genum-class> <clutter-actor-align>>
    default-value = 0

;;;
;;; Could be usefull, to debug...
;;;

(connect child
	 'notify
	 (lambda (a e)
	   (and (is-a? e <gparam-enum>)
		(case (slot-ref e 'name)
		  ((x-align y-align x-expand y-expand)
		   (clue-grid-change-cb parent child t-values e))))
	   #f)) ;; stop the event propagation


;;;
;;; Attach bug 'trace'
;;;

(define (attach-resize-test)
  (let ((a (make <clutter-actor>
	     #:background-color (get-colour "Gainsboro")))
	(g (make <clutter-grid-layout>))
	(t (make <clutter-text>
	     #:text "Attach: left, top.  Span: width, height")))
    (set-layout-manager a g)
    (attach g t 0 0 1 1)
    a))


;;;
;;; When I debug, it's easier...
;;;

(define clue *clue*)
(define stage *stage*)
(define grid *grid*)
(define help *help*)
(define actor help)
(define header (!header clue))
(define footer (!footer clue))

!#
