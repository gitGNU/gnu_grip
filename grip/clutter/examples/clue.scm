;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2014 - 2016
;;;; David Pirotte <david at altosw dot be>

;;;; This file is part of the Grip examples set.

;;;; Grip examples set is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.

;;;; Grip examples set is distributed in the hope that it will be useful
;;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;;; License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with the Grip examples set.  If not, see
;;;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;       <- stands for ->
;; clue                   clutter example
;; clues                  clutter examples set
;; clus                   clutter support

;;; Code:


(define-module (grip clutter examples clue)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)
  #:use-module (grip reexport)
  #:use-module (grip g-export)
  #:use-module (grip utils)
  #:use-module (grip strings)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter label)
  #:use-module (grip clutter actor)

  #:export (*clue-synopsis*
	    *clue-copyright*
	    *clue-help*
	    *clue-welcome*
	    *clue-debug-help-1*
	    *clue-debug-help-2*
	    *clue-debug-help-3*
	    
	    *clue-stage-colour*

	    *clue-header-opacity*
	    *clue-header-label*
	    *clue-header-font*
	    *clue-header-fg*
	    *clue-header-bg*
	    *clue-header-ulcolour*

	    *clue-footer-opacity*
	    *clue-footer-label*
	    *clue-footer-font*
	    *clue-footer-fg*
	    *clue-footer-bg*

	    clue-get-title
	    clue-get-synopsis
	    clue-display-help
	    clue-display-version
	    clue-display-debug-help
	    clue-help-grid-add-child

	    <clue-header>
	    <clue-footer>
	    <clue>
	    clue-snap-to-stage
	    clue-snap-to-bottom))


(g-export !opacity
	  !label
	  !font
	  !bg
	  !fg
	  !ulcolour

	  !name
	  !stage
	  !toolbar
	  !header
	  !footer)

(eval-when (expand load eval)
  (re-export-public-interface (grip clutter colour)
			      (grip clutter globals)))


;;;
;;; Clue globals
;;;


(define *clue-synopsis*
  '((debug (value #f))
    (version (value #f))
    (help (value #f))))

(define *clue-copyright*
  "Copyright (C) 2015 David Pirotte <david at altosw dot be>

Grip-Clutter examples come with ABSOLUTELY NO WARRANTY.  This
program is free software, and you are welcome to redistribute it under
certain conditions.  See <http://www.gnu.org/licenses/gpl.html>, for
more details.")

(define *clue-help*
  "Usage: ~A [OPTION]...
      --debug    spawns a server that you may connect to in order to
                 interact with the clutter example that is being run
      --help     display this usage information
      --version  display version information

Mandatory arguments to long options are mandatory for short options
too.")

(define *clue-welcome* "Grip-Clutter ~A:  ~A example.")

(define *clue-debug-help-1*
  "\nYou requested to run this grip-clutter example in debug mode, here is
what I did for you:

  [-] a server has been spawned, in a new thread, port ~A, which you may
      connect to [if you use emacs and geiser, which we recommend, use M-x
      connect-to-guile];

  [-] the following global variables have been defined:

        *stage*      a <clus-stage> instance
        *clue*       a <clue> instance\n")

(define *clue-debug-help-2*
  "        ~A~21ta ~A instance\n")

(define *clue-debug-help-3*
  "\nHappy hacking!\n")

(define *clue-stage-colour* #;"OldLace" '(60 60 60 255))

(define *clue-header-opacity* 220)
(define *clue-header-label* "Untitled")
(define *clue-header-font* "FreeSans Regular 22")
(define *clue-header-fg* #;"Gainsboro" #;"BurlyWood" "#ffffbb")
(define *clue-header-bg* (get-darker-colour *clue-stage-colour* #:ratio 0.4))
(define *clue-header-ulcolour* "Chocolate" #;"Green3")

(define *clue-footer-opacity* 196)
#;(define *clue-footer-label*
  "this uses guile-clutter, a direct binding of the clutter library for the guile language")
(define *clue-footer-label*
  "powered by <span foreground=\"Chocolate\">guile-clutter</span>, a clutter library binding for the guile language")
(define *clue-footer-font* "Mono 9")
(define *clue-footer-fg* #;"Green3" "#32ff32" #;"Gainsboro")
(define *clue-footer-bg* "Black")


;;;
;;; User help related stuff
;;;

(define (clue-get-title clue-name)
  (string-append "Grip-Clutter:  " clue-name " example"))

(define* (clue-get-synopsis #:key (extra #f))
  (if extra
      (append extra *clue-synopsis*)
      *clue-synopsis*))

(define (clue-display-help cmd port)
  (display (format #f "~?" *clue-help* (list cmd)) port)
  (display "\n" port))

(define (clue-display-version clue-name port)
  (let ((version *clutter-version*))
    (display (format #f "~?\n\n" *clue-welcome* (list version clue-name)) port)
    (display *clue-copyright* port)
    (display "\n" port)))

(define* (clue-display-debug-help clue-name server-port port #:key (ovars #f))
  (display (format #f "~?" *clue-debug-help-1* (list server-port)) port)
  (when ovars
    (for-each (lambda (ovar)
		(display (format #f "~?" *clue-debug-help-2*
				 (list (car ovar) (cdr ovar)) port)))
	ovars))
  (display *clue-debug-help-3* port))

(define (clue-help-grid-add-child grid spec)
  (match spec
    ((l t w h name text font bg fg span l-align x-align y-align x-expand y-expand)
     (let ((child (make <clutter-text>
		    ;; below only for grid size debug
		    ;; #:background-color (get-lighter-colour *clue-stage-colour*)
		    #:text text
		    #:x-align x-align
		    #:y-align y-align
		    #:x-expand x-expand
		    #:y-expand y-expand
		    #:use-markup #t)))
       (set-color child
		  (if fg (get-colour fg) (get-colour "Gainsboro")))
       (and name (set-name child name))
       (and font (set-font-name child font))
       (and bg (set-background-color child bg))
       (and span
	    (match span
	      ((property value)
	       (set-markup child
			   (str/span text property value)))))
       (and l-align (set-line-alignment child l-align))
       (add-child grid child l t w h)
       child))))


;;;
;;; Header, Footer
;;;

(define-class <clue-header> (<clutter-actor>)
  (opacity #:accessor !opacity
	   #:init-keyword #:opacity
	   #:init-value *clue-header-opacity*)
  (label #:accessor !label
	 #:init-keyword #:label
	 #:init-value (clue-get-title *clue-header-label*))
  (font #:accessor !font
	#:init-keyword #:font
	#:init-value *clue-header-font*)
  (fg #:accessor !fg
      #:init-keyword #:fg
      #:init-value *clue-header-fg*)
  (bg #:accessor !bg
      #:init-keyword #:bg
      #:init-value *clue-header-bg*)
  (ulcolour #:accessor !ulcolour
	    #:init-keyword #:ulcolour
	    #:init-value *clue-header-ulcolour*))

(define-method (initialize (self <clue-header>) initargs)
  (next-method)
  (receive (label lw lh)
      (clus-make-label (!label self) (!font self) (get-colour (!fg self)))
    (let ((layout (clutter-bin-layout-new 'center 'center)))
      (set-layout-manager self layout)
      (set-height self (* 2 lh))
      (set-background-color self (get-colour (!bg self)))
      (set-opacity self (!opacity self))
      (add-child self label)
      (let ((ul (clus-make-actor (+ lw 20) 2 #:colour (!ulcolour self))))
	(add-constraint ul (clus-make-bind-constraint label 'y (+ lh 4)))
	(add-child self ul)))))

(define-class <clue-footer> (<clutter-actor>)
  (opacity #:accessor !opacity #:init-keyword #:opacity #:init-value *clue-footer-opacity*)
  (label #:accessor !label #:init-keyword #:label #:init-value *clue-footer-label*)
  (font #:accessor !font #:init-keyword #:font #:init-value *clue-footer-font*)
  (fg #:accessor !fg #:init-keyword #:fg #:init-value *clue-footer-fg*)
  (bg #:accessor !bg #:init-keyword #:bg #:init-value *clue-footer-bg*))

(define (b-footer label)
  (string-append "<b>" label "</b>"))

(define-method (initialize (self <clue-footer>) initargs)
  (next-method)
  (receive (label lw lh)
      (clus-make-label (!label self) #;(b-footer (!label self))
		       (!font self) (get-colour (!fg self)) 'use-markup)
    (let ((footer-height (* 1.5 lh))
	  (layout (clutter-bin-layout-new 'center 'center)))
      (set-layout-manager self layout)
      (set-background-color self (get-colour (!bg self)))
      (set-opacity self (!opacity self))
      (set-height self footer-height)
      (add-child self label))))


;;;
;;; Clue
;;;

(define-class <clue> ()
  (name #:accessor !name #:init-keyword #:name #:init-value #f)
  (stage #:accessor !stage #:init-keyword #:stage #:init-value #f)
  (toolbar #:accessor !toolbar #:init-keyword #:toolbar #:init-value #f)
  (header #:accessor !header #:init-keyword #:header #:init-value #f)
  (footer #:accessor !footer #:init-keyword #:footer #:init-value #f))

(define-method (initialize (self <clue>) initargs)
  (next-method)
  (let ((stage (!stage self)))
    (receive (sw sh)
	(get-size stage)
      (let ((header (make <clue-header> #:label (get-title stage)))
	    (footer (make <clue-footer> #:label *clue-footer-label*)))
	(set! (!header self) header)
	(clus-bind stage header 'x 0.0)
	(clus-bind stage header 'y 0.0)
	(clus-bind stage header 'width 0.0)
	(add-child stage header)
	(set! (!footer self) footer)
	(clus-bind stage footer 'width 0.0)
	(clus-snap stage footer 'bottom 'bottom 0.0)
	(clus-snap stage footer 'top 'bottom (- (get-height footer)))
	(add-child stage footer)))))


;;;
;;; Clue specific utils
;;;

(define* (clue-snap-to-stage clue actor
			     #:key (left-margin 0)
			     (top-margin 0)
			     (bottom-margin 0)
			     (right-margin 0))
  (let ((stage (!stage clue))
	(header (!header clue))
	(footer (!footer clue)))
    (clus-snap header actor 'top 'bottom top-margin)
    (clus-snap footer actor 'bottom 'top (- bottom-margin))
    (clus-snap stage actor 'left 'left left-margin)
    (clus-snap stage actor 'right 'right (- right-margin))))

(define* (clue-snap-to-bottom clue grid actor
			      #:key (left-margin 0)
			      (top-margin 0)
			      (bottom-margin 0)
			      (right-margin 0))
  (let ((stage (!stage clue))
	(header (!header clue))
	(footer (!footer clue)))
    (clus-snap grid actor 'top 'bottom top-margin)
    (clus-snap footer actor 'bottom 'top (- bottom-margin))
    (clus-snap stage actor 'left 'left left-margin)
    (clus-snap stage actor 'right 'right (- right-margin))))
