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

;; This code needs review, but it won't be as good as it could until
;; goops is fixed.

;;; Code:


(define-module (grip clutter apple)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome clutter)
  #:use-module (grip g-export)
  #:use-module (grip goops)
  #:use-module (grip utils)
  #:use-module (grip keyword)
  #:use-module (grip lists)
  #:use-module (grip nbs)
  #:use-module (grip strings)
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter gravity)
  #:use-module (grip clutter orientation)
  #:use-module (grip clutter actor)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<clus-apple>))


(g-export get-orientation
	  set-orientation
	  
	  !colour
	  !layout
	  !orientation
	  !x-align
	  !y-align
	  !x-expand
	  !y-expand
	  !gravity
	  !gravity-pole
	  !gravity-force
	  !gravity-resistance-north
	  !gravity-resistance-west
	  !gravity-resistance-mode
	  !x-constraint
	  !y-constraint

	  show-gravity
	  get-gravity
	  set-gravity
	  get-gravity-pole
	  set-gravity-pole
	  get-gravity-force
	  set-gravity-force
	  get-gravity-resistance-margin
	  get-gravity-resistance
	  get-gravity-resistance-north
	  set-gravity-resistance-north
	  get-gravity-resistance-west
	  set-gravity-resistance-west
	  get-gravity-resistance-mode
	  set-gravity-resistance-mode

	  get-next
	  #;get-min-width
	  #;get-min-height
	  #;get-min-size
	  #;auto-resize)


;;;
;;; Apple
;;;

(define-class <clus-apple> (<clutter-actor>)
  (colour #:accessor !colour
	  #:init-keyword #:colour
	  #:allocation #:virtual
	  #:slot-ref (lambda (self)
		       (get-background-color self))
	  #:slot-set! (lambda (self value)
			(set-background-color self value)))
  (layout #:accessor !layout
	  #:allocation #:virtual
	  #:slot-ref (lambda (self)
		       (get-layout-manager self))
	  #:slot-set! (lambda (self value)
			(set-layout-manager self value)))
  (orientation #:accessor !orientation
	       #:init-keyword #:orientation
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (let* ((layout (get-layout-manager self))
				   (c-name (class-name (class-of layout))))
			      (case c-name
				((<clutter-box-layout>
				  <clutter-grid-layout>)
				 (get-orientation layout))
				(else
				 (dimfi "WARNING:" self "is not sensitive to orientation")))))
	       #:slot-set! (lambda (self value)
			     ;; (dimfi "<clus-apple> slot-set! orientation" self value)
			     (let* ((layout (get-layout-manager self))
				   (c-name (class-name (class-of layout))))
			      (case c-name
				((<clutter-box-layout>
				  <clutter-grid-layout>)
				 (set-orientation layout value))
				(else
				 (dimfi "WARNING:" self "is not sensitive to orientation"))))))
  (x-align #:accessor !x-align
	   #:init-keyword #:x-align
	   #:allocation #:virtual
	   #:slot-ref (lambda (self)
			(get self 'x-align))
	   #:slot-set! (lambda (self value)
			 (set self 'x-align value)))
  (y-align #:accessor !y-align
	   #:init-keyword #:y-align
	   #:allocation #:virtual
	   #:slot-ref (lambda (self)
			(get self 'y-align))
	   #:slot-set! (lambda (self value)
			 (set self 'y-align value)))
  (x-expand #:accessor !x-expand
	   #:init-keyword #:x-expand
	   #:allocation #:virtual
	   #:slot-ref (lambda (self)
			(get self 'x-expand))
	   #:slot-set! (lambda (self value)
			 (set self 'x-expand value)))
  (y-expand #:accessor !y-expand
	   #:init-keyword #:y-expand
	   #:allocation #:virtual
	   #:slot-ref (lambda (self)
			(get self 'y-expand))
	   #:slot-set! (lambda (self value)
			 (set self 'y-expand value)))
  ;; gravity can't be set before the actor has a parent
  (g #:init-value 'north-west)
  (gravity #:accessor !gravity
	   #:init-keyword #:gravity
	   #:allocation #:virtual
	   #:slot-ref (lambda (self)
			(get-gravity self))
	   #:slot-set! (lambda (self value)
			 (set-gravity self value)))
  (g-pole #:init-value 'north)
  (gravity-pole #:accessor !gravity-pole
		#:init-keyword #:gravity-pole
		#:allocation #:virtual
		#:slot-ref (lambda (self)
			     (get-gravity-pole self))
		#:slot-set! (lambda (self value)
			      (set-gravity-pole self value)))
  (g-force #:init-value 'strong)
  (gravity-force #:accessor !gravity-force
		 #:init-keyword #:gravity-force
		 #:allocation #:virtual
		 #:slot-ref (lambda (self)
			      (get-gravity-force self))
		 #:slot-set! (lambda (self value)
			       (set-gravity-force self value)))
  ;; the gravity resistance is a ratio [0.0 1.0]
  (g-r-north #:init-value 0.0)
  (gravity-resistance-north #:accessor !gravity-resistance-north
			    #:init-keyword #:gravity-resistance-north
			    #:allocation #:virtual
			    #:slot-ref (lambda (self)
					 (get-gravity-resistance-north self))
			    #:slot-set! (lambda (self value)
					  (set-gravity-resistance-north self value)))
  (g-r-west #:init-value 0.0)
  (gravity-resistance-west #:accessor !gravity-resistance-west
			   #:init-keyword #:gravity-resistance-west
			   #:allocation #:virtual
			   #:slot-ref (lambda (self)
					(get-gravity-resistance-west self))
			   #:slot-set! (lambda (self value)
					 (set-gravity-resistance-west self value)))
  (g-r-mode #:init-value 'asym)
  (gravity-resistance-mode #:accessor !gravity-resistance-mode
			   #:init-keyword #:gravity-resistance-mode
			   #:allocation #:virtual
			   #:slot-ref (lambda (self)
					(get-gravity-resistance-mode self))
			   #:slot-set! (lambda (self value)
					 (set-gravity-resistance-mode self value)))
  (x-constraint #:accessor !x-constraint #:init-keyword #:x-constraint #:init-value #f)
  (y-constraint #:accessor !y-constraint #:init-keyword #:y-constraint #:init-value #f))


(define-method (initialize (self <clus-apple>) initargs)
  #;(dimfi "initialize <clus-apple>" initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-apple>)))
    (if (null? virtual-kw)
	(next-method)
	(begin
	  (next-method self kw)
	  (let-keywords virtual-kw #t
			((colour #f)
			 (x-align #f)
			 (y-align #f)
			 (x-expand #f)
			 (y-expand #f))
			(when colour
			  (set! (!colour self) colour))
			(when x-align
			  (set! (!x-align self) x-align))
			(when y-align
			  (set! (!y-align self) y-align))
			(when x-expand
			  (set! (!x-expand self) x-expand))
			(when y-expand
			  (set! (!y-expand self) y-expand)))))))

(define-method (show-gravity (self <clus-apple>))
  (format #t "~S - instance of ~A~%"
	  self
	  (class-name (class-of self)))
  (format #t "  gravity slots and values are:~%")
  (for-each (lambda (slot)
	      (let ((name (slot-definition-name slot)))
		(format #t "    ~S = ~A~%"
			name
			(if (slot-bound? self name) 
			    (format #f "~S" (slot-ref self name))
			    "#<unbound>"))))
      (filter-map (lambda (slot)
		    (and (str/contains? (symbol->string (slot-definition-name slot))
					"gravity")
			 slot))
	  (class-direct-slots <clus-apple>)))
  (format #t "  margins are:~%")
  (format #t "    left = ~A~%" (get-margin-left self))
  (format #t "     top = ~A~%" (get-margin-top self))
  (format #t "   right = ~A~%" (get-margin-right self))
  (format #t "  bottom = ~A~%" (get-margin-bottom self))
  (values))

#;(define-method (get-min-width (self <clus-apple>))
  (receive (grid-min-width)
      (get-min-width (!grid self))
    (and grid-min-width
	 (+ grid-min-width (* 2 (!border self))))))

#;(define-method (get-min-height (self <clus-apple>))
  (receive (grid-min-height)
      (get-min-height (!grid self))
    (and grid-min-height
	 (+ grid-min-height (* 2 (!border self))))))

#;(define-method (get-min-size (self <clus-apple>))
  (values (get-min-width self)
	  (get-min-height self)))

(define-method (auto-resize (self <clus-apple>))
  #;(let ((grid (!grid self)))
    (set-size grid
	      (get-min-width grid)
	      (get-min-height grid))
    (set-size self
	      (get-min-width self)
	      (get-min-height self))
    (dimfi grid
	   "r-spacing:" (!row-spacing grid)
	   "c-spacing:" (!column-spacing grid)
	   "min-width:" (get-min-width grid)
	   "min-height:" (get-min-height grid))
    (dimfi self
	   "min-width:" (get-min-width self)
	   "min-height:" (get-min-height self)))
  'nothing-for-now)

(define-method (get-orientation (self <clus-apple>))
  (!orientation self))

(define-method (set-orientation (self <clus-apple>) value)
  (set! (!orientation self) value))

#;(define-method ((setter !border) (self <clus-apple>) value)
  (slot-set! self 'border value)
  (auto-resize self))

(define-method (get-gravity (self <clus-apple>))
  (slot-ref self 'g))

(define-method (set-gravity (self <clus-apple>) gravity)
  (let ((parent (get-parent self)))
    (if parent
	(if (clus-gravity? gravity)
	    (unless (eq? gravity (!gravity self))
	      (let ((north-pole? (eq? (!gravity-pole self) 'north))
		    (strong-g? (eq? (!gravity-force self) 'strong)))
		(unless (!x-constraint self)
		  (let ((x-constraint (clus-make-align-constraint parent 'x-axis 0.0))
			(y-constraint (clus-make-align-constraint parent 'y-axis 0.0)))
		    (set! (!x-constraint self) x-constraint)
		    (set! (!y-constraint self) y-constraint)
		    (add-constraint self x-constraint)
		    (add-constraint self y-constraint)))
		(case gravity
		  ((north-west)
		   (when strong-g?
		     (if north-pole?
			 (set! (!orientation self) 'horizontal)
			 (set! (!orientation self) 'vertical)))
		   (set-factor (!x-constraint self) 0.0)
		   (set-factor (!y-constraint self) 0.0))
		  ((west)
		   (when strong-g?
		     (set! (!orientation self) 'vertical))
		   (set-factor (!x-constraint self) 0.0)
		   (set-factor (!y-constraint self) 0.5))
		  ((south-west)
		   (when strong-g?
		     (if north-pole?
			 (set! (!orientation self) 'horizontal)
			 (set! (!orientation self) 'vertical)))
		   (set-factor (!x-constraint self) 0.0)
		   (set-factor (!y-constraint self) 1.0))
		  ((north)
		   (when strong-g?
		     (set! (!orientation self) 'horizontal))
		   (set-factor (!x-constraint self) 0.5)
		   (set-factor (!y-constraint self) 0.0))
		  ((north-east)
		   (when strong-g?
		     (if north-pole?
			 (set! (!orientation self) 'horizontal)
			 (set! (!orientation self) 'vertical)))
		   (set-factor (!x-constraint self) 1.0)
		   (set-factor (!y-constraint self) 0.0))
		  ((east)
		   (when strong-g?
		     (set! (!orientation self) 'vertical))
		   (set-factor (!x-constraint self) 1.0)
		   (set-factor (!y-constraint self) 0.5))
		  ((south-east)
		   (when strong-g?
		     (if north-pole?
			 (set! (!orientation self) 'horizontal)
			 (set! (!orientation self) 'vertical)))
		   (set-factor (!x-constraint self) 1.0)
		   (set-factor (!y-constraint self) 1.0))
		  ((south)
		   (when strong-g?
		     (set! (!orientation self) 'horizontal))
		   (set-factor (!x-constraint self) 0.5)
		   (set-factor (!y-constraint self) 1.0))
		  ((center)
		   (when strong-g?
		     (if north-pole?
			 (set! (!orientation self) 'vertical)
			 (set! (!orientation self) 'horizontal)))
		   (set-factor (!x-constraint self) 0.5)
		   (set-factor (!y-constraint self) 0.5)))
		(slot-set! self 'g gravity)))
	    (format (current-output-port)
		    "no such gravity '~A for ~S~%" gravity self))
	(format (current-output-port)
		"~S has no parent, can't set its gravity yet~%" self))))

(define-method (get-gravity-pole (self <clus-apple>))
  (slot-ref self 'g-pole))

(define-method (set-gravity-pole (self <clus-apple>) gravity-pole)
  #;(dimfi "set-gravity-pole" self gravity-pole)
  (if (clus-gravity-pole? gravity-pole)
      (begin
	(when (eq? (!gravity-force self) 'strong)
	  (case gravity-pole
	    ((north)
	     (case (!gravity self)
	       ((north-west north north-east south-west south south-east)
		(set! (!orientation self) 'horizontal))
	       (else
		(set! (!orientation self) 'vertical))))
	    ((west)
	     (case (!gravity self)
	       ((north center south)
		(set! (!orientation self) 'horizontal))
	       (else
		(set! (!orientation self) 'vertical))))))
	(slot-set! self 'g-pole gravity-pole))
      (format (current-output-port)
	      "no such gravity pole '~A for ~S~%" gravity-pole self)))

(define-method (get-gravity-force (self <clus-apple>))
  (slot-ref self 'g-force))

(define-method (set-gravity-force (self <clus-apple>) gravity-force)
  (if (clus-gravity-force? gravity-force)
      (slot-set! self 'g-force gravity-force)
      (dimfi "WARNING:" gravity-force "is not a valid gravity force")))

;; note that the gravity resistance factor is normalized, so that west
;; [always] remains west of east, north remains north of south... and
;; the actor always remains entirely in the quarter of the scene
;; determined by its gravity.

(define* (clus-warning-invalid-resistance-factor factor #:optional (port #f))
  (let ((port (if port port (current-output-port))))
    (dimfi port "WARNING:" factor "is not a valid gravity resistance factor, which
must ba a value between " *clus-gravity-resistance-min* "and" *clus-gravity-resistance-max*)
    #f))

(define* (clus-warning-no-parent self #:optional (port #f))
  (let ((port (if port port (current-output-port))))
    (dimfi port "WARNING:" self " has no parent, gravity resistance can't be set\n")
    #f))

(define-method (get-gravity-resistance-margin (self <clus-apple>) which factor)
  (let ((parent (get-parent self)))
    (if parent
	(if (clus-gravity-valid-resistance-factor? factor)
	    (receive (pw ph)
		(get-size parent)
	      (let ((n-factor (/ factor
				 *clus-gravity-resistance-normalization-quotient*)))
		(case which
		  ((north)
		   (fp/round (* n-factor ph) 3))
		  ((west)
		   (fp/round (* n-factor pw) 3)))))
	    (clus-warning-invalid-resistance-factor factor))
	(clus-warning-no-parent self))))

(define-method (set-gravity-resistance (self <clus-apple>) which factor)
  (let* ((gravity (!gravity self))
	 (r-mode (!gravity-resistance-mode self))
	 (new (get-gravity-resistance-margin self which factor)))
    (when new
      (let* ((prnf (!gravity-resistance-north self))
	     (prwf (!gravity-resistance-west self))
	     (prev-north (get-gravity-resistance-margin self 'north prnf))
	     (prev-west (get-gravity-resistance-margin self 'west prwf))
	     (min-margin (min new prev-north prev-west))
	     (max-margin (max new prev-north prev-west))
	     (margin (case r-mode
		       ((asym) new)
		       ((min) min-margin)
		       ((max) max-margin))))
	#;(dimfi gravity r-mode)
	#;(dimfi "  " (list-insert
		     '(new prev-north prev-west min max set-to)
		     ;; clutter sometimes returns far too many decimals
		     (map fp/round
		       (list new prev-north prev-west min-margin max-margin margin))))
	(case which
	  ((north)
	   (slot-set! self 'g-r-north factor)
	   (set-margin-top self margin)
	   (set-margin-bottom self margin))
	  ((west)
	   (slot-set! self 'g-r-west factor)
	   (set-margin-left self margin)
	   (set-margin-right self margin)))))))

(define-method (get-gravity-resistance-north (self <clus-apple>))
  (slot-ref self 'g-r-north))

(define-method (set-gravity-resistance-north (self <clus-apple>) factor)
  (set-gravity-resistance self 'north factor))

(define-method (get-gravity-resistance-west (self <clus-apple>))
  (slot-ref self 'g-r-west))

(define-method (set-gravity-resistance-west (self <clus-apple>) factor)
  (set-gravity-resistance self 'west factor))

(define-method (get-gravity-resistance-mode (self <clus-apple>))
  (slot-ref self 'g-r-mode))

(define-method (set-gravity-resistance-mode (self <clus-apple>) mode)
  (let ((parent (get-parent self)))
    (if parent
	(if (clus-gravity-resistance-mode? mode)
	    (begin
	      (slot-set! self 'g-r-mode mode)
	      ;; need to trigger re-positioning
	      (set! (!gravity-resistance-north self) (!gravity-resistance-north self))
	      (set! (!gravity-resistance-west self) (!gravity-resistance-west self)))
	    (format (current-output-port)
		    "~A is not a valid gravity resistance mode.  It must be one of ~S~%"
		    mode %clus-gravity-resistance-modes))
	(format (current-output-port)
		"~S has no parent, can't set its gravity resistance mode yet~%" self))))


(define-method (get-next (self <clus-apple>) what)
  (let ((current (case what
		   ((gravity) (!gravity self))
		   ((pole) (!gravity-pole self))
		   ((force) (!gravity-force self))
		   ((r-mode) (!gravity-resistance-mode self)))))
    (clus-get-next what current)))


#!

;; got this bug once

Failed to open BO for returned DRI2 buffer (1600x900, dri2 back buffer, named 18).
This is likely a bug in the X Server that will lead to a crash soon.
Segmentation fault

!#
