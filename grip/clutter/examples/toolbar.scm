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

;; This code needs review.

;;; Code:


(define-module (grip clutter examples toolbar)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome glib)
  #:use-module (gnome gw gdk) ;; gdk-pixbuf
  #:use-module (gnome clutter)
  #:use-module (grip nbs)
  #:use-module (grip strings)
  #:use-module (grip utils)
  #:use-module (grip clutter)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter align)
  #:use-module (grip clutter actor)
  #:use-module (grip clutter bin)
  #:use-module (grip clutter grid)
  #:use-module (grip clutter utils)
  #:use-module (grip clutter examples clue)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (clue-toolbar-user-help))


(define get-current-gravity #f)
(define get-next-gravity #f)


(let* ((gravities %clus-gravities)
       (nb-gravities (length gravities))
       (i 0))
  (set! get-current-gravity (lambda () (list-ref gravities i)))
  (set! get-next-gravity
	(lambda ()
	  (set! i (modulo (1+ i) nb-gravities))
	  (list-ref gravities i))))


(define (get-current-gravity-string)
  (match (map string-capitalize
	   (string-split (symbol->string (get-current-gravity)) #\-))
    ((a b) (string-append a " " b))
    ((a) a)))

(define (get-current-resistance-mode-string mode)
  (case mode
    ((asym) "Asymmetric")
    ((min) "Minimum")
    ((max) "Maximum")))

(define (clue-toolbar-user-help-get-col grid items bg fg)
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

(define %toolbar-help-children-spec
  `((0 0 6 1				;; left top width height
     "title-1"				;; actor's name or #f
     "Gravity"				;; text
     "Dejavu Sans 11"			;; font-name or #f
     (48 48 48 255)			;; background or #f
     #f					;; foreground or #f
     ("foreground" "#ffffbb")	 	;; span (prop value) or #f
     center				;; line alignment of #f
     fill				;; x-align
     center				;; y-align
     #t					;; x-expand
     #f)				;; y-expand
    (0 1 3 1 "gravity" "North West" #f #f #f #f #;("foreground" "#ffffbb") #f fill center #t #f)
    (3 1 1 1 #f "[" #f #f "Chocolate" #f #f fill center #t #f)
    (4 1 1 1 #f
       ,(string-append (str/span "Right" "foreground" "LightSkyBlue")
		       (str/span ", " "foreground" "Gainsboro")
		       (str/span "Left" "foreground" "#98fb98"))
       #f #f #f #f #f fill center #t #f)
    (5 1 1 1 #f "]" #f #f "Chocolate" #f #f fill center #t #f)
    (6 1 3 1 "r-mode" "Asymmetric" #f #f #f #f #;("foreground" "#ffffbb") #f fill center #t #f)
    (9 1 1 1 #f "[" #f #f "Chocolate" #f #f fill center #t #f)
    (10 1 1 1 #f "m" #f #f #f ("foreground" "LightSkyBlue") #f fill center #t #f)
    (11 1 1 1 #f "]" #f #f "Chocolate" #f #f fill center #t #f)

    (0 2 1 1 #f "Pole" #f #f #f #f #f start center #t #f)
    (0 3 1 1 #f "Force" #f #f #f #f #f start center #t #f)
    #;(1 1 1 1 #f ":" #f #f #f #f #f start center #t #f)
    (1 2 1 1 #f ":" #f #f #f #f #f start center #t #f)
    (1 3 1 1 #f ":" #f #f #f #f #f start center #t #f)

    (2 2 1 1 "pole" "North" #f #f #f ("foreground" "LightSkyBlue") #f start center #t #f)
    (2 3 1 1 "force" "Strong" #f #f #f ("foreground" "#98fb98") #f start center #t #f)

    (3 2 1 1 #f "[" #f #f "Chocolate" #f #f start center #t #f)
    (3 3 1 1 #f "[" #f #f "Chocolate" #f #f start center #t #f)
    (4 2 1 1 #f "p" #f #f #f ("foreground" "#98fb98") #f start center #t #f)
    (4 3 1 1 #f "f" #f #f #f ("foreground" "LightSkyBlue") #f start center #t #f)

    (5 2 1 1 #f "]" #f #f "Chocolate" #f #f start center #t #f)
    (5 3 1 1 #f "]" #f #f "Chocolate" #f #f start center #t #f)
    (6 0 6 1				;; left top width height
     "title-2"				;; actor's name or #f
     "Resistance"			;; text
     "Dejavu Sans 11"			;; text font or #f
     (48 48 48 255)			;; background or #f
     #f					;; foreground or #f
     ("foreground" "#ffffbb")	 	;; span (prop value) or #f
     center				;; line alignment of #f
     fill				;; x-align
     center				;; y-align
     #t					;; x-expand
     #f)				;; y-expand
    (6 2 1 1 #f "North" #f #f #f #f #f start center #t #f)
    (6 3 1 1 #f "West" #f #f #f #f #f start center #t #f)
    (7 2 1 1 #f ":" #f #f #f #f #f start center #t #f)
    (7 3 1 1 #f ":" #f #f #f #f #f start center #t #f)

    (8 2 1 1 "r-north" "0.00" #f #f #f ("foreground" "LightSkyBlue") #f start center #t #f)
    (8 3 1 1 "r-west" "0.00" #f #f #f ("foreground" "#98fb98") #f start center #t #f)

    (9 2 1 1 #f "[" #f #f "Chocolate" #f #f start center #t #f)
    (9 3 1 1 #f "[" #f #f "Chocolate" #f #f start center #t #f)

    (10 2 1 1 #f
	,(string-append (str/span "\u2191" "foreground" "#98fb98")
			(str/span ", " "foreground" "Gainsboro")
			(str/span "\u2193" "foreground" "LightSkyBlue"))
	#f #f #f #f #f start center #t #f)
    (10 3 1 1 #f
	,(string-append (str/span "\u2190" "foreground" "LightSkyBlue")
			(str/span ", " "foreground" "Gainsboro")
			(str/span "\u2192" "foreground" "#98fb98"))
	#f #f #f #f #f start center #t #f)

    (11 2 1 1 #f "]" #f #f "Chocolate" #f #f start center #t #f)
    (11 3 1 1 #f "]" #f #f "Chocolate" #f #f start center #t #f)))

(define (install-stage-button-press-signals stage toolbar gravity)
  (connect stage
	   'button-press-event
	   (lambda (s e)
	     (let ((button (get-button e))
		   (modifiers (gflags->symbol-list (get-state e))))
	       (case button
		 ((1)
		  (set-gravity toolbar (get-current-gravity)))
		 ((3)
		  ;; (memq 'shift-mask modifiers) ...
		  (get-next-gravity)
		  (set-markup gravity (get-current-gravity-string)))))
	     #t))) ;; stops the event to be propagation

(define (install-stage-key-press-signals stage toolbar 
					 gravity pole force r-mode r-north r-west)
  (connect stage
	   'key-press-event #;'key-release-event
	   (lambda (s e)
	     (case (get-key-symbol e)
	       ((112) ;; #\p
		(let* ((next-pole (get-next toolbar 'pole))
		       (next-pole-string (string-capitalize (symbol->string next-pole))))
		  (set-gravity-pole toolbar next-pole)
		  (set-markup pole (str/span next-pole-string "foreground" "LightSkyBlue"))))
	       ((102) ;; #\f
		(let* ((next-force (get-next toolbar 'force))
		       (next-force-string (string-capitalize (symbol->string next-force))))
		  (set-gravity-force toolbar next-force)
		  (set-markup force (str/span next-force-string "foreground" "#98fb98"))))
	       ((109) ;; #\m
		(let* ((next-r-mode (get-next toolbar 'r-mode))
		       (next-r-mode-string (get-current-resistance-mode-string next-r-mode)))
		  (set-gravity-resistance-mode toolbar next-r-mode)
		  (set-markup r-mode next-r-mode-string)))
	       ((65362) ;; up arrow
		(let* ((next-r-north (fp/round (+ (!gravity-resistance-north toolbar) 0.01)))
		       (next-r-north-string (format #f "~,2F" next-r-north)))
		  (and (fp/<? next-r-north 1.01)
		       (set-gravity-resistance-north toolbar next-r-north)
		       (set-markup r-north
				   (str/span next-r-north-string "foreground" "LightSkyBlue")))))
	       ((65364) ;; down arrow
		(let* ((next-r-north (fp/round (- (!gravity-resistance-north toolbar) 0.01)))
		       (next-r-north-string (format #f "~,2F" next-r-north)))
		  (and (fp/>? next-r-north -0.01)
		       (set-gravity-resistance-north toolbar next-r-north)
		       (set-markup r-north
				   (str/span next-r-north-string "foreground" "LightSkyBlue")))))
	       ((65361) ;; left arrow
		(let* ((next-r-west (fp/round (- (!gravity-resistance-west toolbar) 0.01)))
		       (next-r-west-string (format #f "~,2F" next-r-west)))
		  (and (fp/>? next-r-west -0.01)
		       (set-gravity-resistance-west toolbar next-r-west)
		       (set-markup r-west
				   (str/span next-r-west-string "foreground" "#98fb98")))))
	       ((65363) ;; right arrow
		(let* ((next-r-west (fp/round (+ (!gravity-resistance-west toolbar) 0.01)))
		       (next-r-west-string (format #f "~,2F" next-r-west)))
		  (and (fp/<? next-r-west 1.01)
		       (set-gravity-resistance-west toolbar next-r-west)
		       (set-markup r-west
				   (str/span next-r-west-string "foreground" "#98fb98"))))))
	     #t))) ;; yes, please stop this even propagation

(define* (clue-toolbar-user-help stage toolbar
				 #:key (bg #f)
				 (cbg #f)
				 (cfg (get-colour "Gainsboro")))
  (let* ((grid (make <clus-grid>
		 ;; #:background-color (get-darker-colour *clue-stage-colour*)
		 #:orientation 'horizontal
		 #:row-spacing 2
		 #:column-spacing 4
		 #:row-homogeneous #t
		 #:column-homogeneous #f))
	 (children (map (lambda (spec)
			  (clue-help-grid-add-child grid spec))
		     %toolbar-help-children-spec))
	 (gravity (get-child-named grid "gravity"))
	 (pole (get-child-named grid "pole"))
	 (force (get-child-named grid "force"))
	 (r-mode (get-child-named grid "r-mode"))
	 (r-north (get-child-named grid "r-north"))
	 (r-west (get-child-named grid "r-west")))
    (install-stage-button-press-signals stage toolbar gravity)
    (install-stage-key-press-signals stage toolbar
				     gravity pole force r-mode r-north r-west)
    (and bg (set-background-color grid (get-colour bg)))
    grid))
