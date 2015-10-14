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
  #:use-module (grip clutter colour)
  #:use-module (grip clutter globals)
  #:use-module (grip clutter constraint)
  #:use-module (grip clutter gravity)
  #:use-module (grip clutter orientation)
  #:use-module (grip clutter actor)
  #:use-module (grip clutter grid)  

  #:export (<clus-apple>))


(g-export !colour
	  !border
	  !layout
	  !grid
	  !grid-layout
	  !orientation
	  !gravity
	  !gravity-pole
	  !gravity-force
	  !gravity-resistance-north
	  !gravity-resistance-west
	  !gravity-resistance-mode
	  !x-constraint
	  !y-constraint

	  get-grid-children
	  #;add-child
	  get-min-width
	  get-min-height
	  get-min-size
	  auto-resize

	  get-orientation
	  set-orientation)


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
			(set-background-color self (get-colour value))))
  (border #:accessor !border
	  #:init-keyword #:border
	  #:init-value 0)
  (layout #:getter !layout
	  #:allocation #:virtual
	  #:slot-ref (lambda (self)
		       (get-layout-manager self))
	  #:slot-set! (lambda (self value)
			'nothing))
  (grid #:getter !grid
	#:allocation #:virtual
	#:slot-ref (lambda (self)
		     (get-first-child self))
	#:slot-set! (lambda (self value)
		      'nothing))
  (grid-layout #:getter !grid-layout
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (get-layout-manager (!grid self)))
	       #:slot-set! (lambda (self value)
			     'nothing))
  (orientation #:accessor !orientation
	       #:allocation #:virtual
	       #:slot-ref (lambda (self)
			    (get-orientation (!grid-layout self)))
	       #:slot-set! (lambda (self value)
			     (set! (!orientation self) value)))
  ;; gravity can't be set before the actor has a parent
  (gravity #:accessor !gravity
	   #:init-keyword #:gravity
	   #:init-value #f)
  (gravity-pole #:accessor !gravity-pole
		#:init-keyword #:gravity-pole
		#:init-value *clus-apple-gravity-pole*)
  (gravity-force #:accessor !gravity-force
		 #:init-keyword #:gravity-force
		 #:init-value *clus-apple-gravity-force*)
  ;; the gravity resistance is a ratio [0.0 1.0]
  (gravity-resistance-north #:accessor !gravity-resistance-north
			    #:init-keyword #:gravity-resistance-north
			    #:init-value 0.0)
  (gravity-resistance-west #:accessor !gravity-resistance-west
			   #:init-keyword #:gravity-resistance-west
			   #:init-value 0.0)
  (gravity-resistance-mode #:accessor !gravity-resistance-mode
			   #:init-keyword #:gravity-resistance-mode
			   #:init-value 'asym)
  (x-constraint #:accessor !x-constraint #:init-keyword #:x-constraint #:init-value #f)
  (y-constraint #:accessor !y-constraint #:init-keyword #:y-constraint #:init-value #f))


(define-method (initialize (self <clus-apple>) initargs)
  (dimfi "initialize <clus-apple>" initargs)
  (receive (kw virtual-kw)
      (split-keyword-args initargs
			  (map slot-definition-init-keyword
			    (class-direct-virtual-slots <clus-apple>)))
      (if (null? virtual-kw)
	  (next-method)
	  (begin
	    (next-method self kw)
	    (let-keywords virtual-kw #t
			  ((colour #f))
			  (when colour (set! (!colour self) colour)))))
      (let* ((name (get-name self))
	     (grid (make <clus-grid>
		    #:name (and name
				(string-append name "'s grid"))))
	    (self-layout (make <clutter-bin-layout>
			   #:x-align 'center
			   #:y-align 'center)))
      (set-easing-duration self 1000)
      (set-layout-manager self self-layout)
      (add-child self grid))))

(define-method (get-grid-children (self <clus-apple>))
  (get-children (!grid self)))

#;(define-method (add-child (self <clus-apple>) item)
  (let* ((children (get-children (self)))
	 (how-many (length children)))
    (attach (!grid-layout self)
	    item
	    how-many 0 1 1)))

(define-method (get-min-width (self <clus-apple>))
  (receive (grid-min-width)
      (get-min-width (!grid self))
    (and grid-min-width
	 (+ grid-min-width (* 2 (!border self))))))

(define-method (get-min-height (self <clus-apple>))
  (receive (grid-min-height)
      (get-min-height (!grid self))
    (and grid-min-height
	 (+ grid-min-height (* 2 (!border self))))))

(define-method (get-min-size (self <clus-apple>))
  (values (get-min-width self)
	  (get-min-height self)))

(define-method (auto-resize (self <clus-apple>))
  (let ((grid (!grid self)))
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
	   "min-height:" (get-min-height self))))

(define-method ((setter !border) (self <clus-apple>) value)
  (slot-set! self 'border value)
  (auto-resize self))

(define-method ((setter !orientation) (self <clus-apple>) orientation)
  (if (clus-orientation? orientation)
      (unless (eq? orientation (!orientation self))
	(set-orientation (!grid-layout self) orientation)
	(auto-resize self))
      (format (current-output-port)
	      "WARNING: no such orientation '~A for ~S~%" orientation self)))

(define-method (get-orientation (self <clus-apple>))
  (get-orientation (!grid-layout self)))

(define-method (set-orientation (self <clus-apple>) orientation)
  (set! (!orientation self) orientation))

(define-method ((setter !gravity) (self <clus-apple>) gravity)
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
		(slot-set! self 'gravity gravity)))
	    (format (current-output-port)
		    "no such gravity '~A for ~S~%" gravity self))
	(format (current-output-port)
		"~S has no parent, can't set its gravity yet~%" self))))

(define-method ((setter !gravity-pole) (self <clus-apple>) gravity-pole)
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
	(slot-set! self 'gravity-pole gravity-pole))
      (format (current-output-port)
	      "no such gravity pole '~A for ~S~%" gravity-pole self)))

;; note that the gravity resistance factor is normalized, so that west
;; [always] remains west of east, north remains north of south... and
;; the actor always remains entirely in the quarter of the scene
;; determined by its gravity.

(define (clus-display-gravity-invalid-resistance-factor factor port)
  (format port "~A is not a valid gravity resistance factor value.  It must be a number
within the range of ~A to ~A~%"
	  factor
	  *clus-gravity-resistance-min*
	  *clus-gravity-resistance-max*))

(define-method ((setter !gravity-resistance-north) (self <clus-apple>) factor)
  (let ((parent (get-parent self))
	(west-factor (!gravity-resistance-west self)))
    (if parent
	(if (clus-gravity-valid-resistance? factor)
	    (receive (pw ph)
		(get-size parent)
	      (let* ((n-factor (/ factor
				  *clus-gravity-resistance-normalization-quotient*))
		     (w-factor (/ west-factor
				  *clus-gravity-resistance-normalization-quotient*))
		     (lr-margin (* w-factor pw))
		     (tb-margin (* n-factor ph))
		     (min-margin (min lr-margin tb-margin))
		     (max-margin (max lr-margin tb-margin)))
		(case (!gravity-resistance-mode self)
		  ((asym)
		   (set-margin-top self tb-margin)
		   (set-margin-bottom self tb-margin))
		  ((min)
		   (set-margin self (list min-margin min-margin min-margin min-margin)))
		  ((max)
		   (set-margin self (list max-margin max-margin max-margin max-margin))))
		(slot-set! self 'gravity-resistance-north factor)))
	    (clus-display-gravity-invalid-resistance-factor factor (current-output-port)))
	(format (current-output-port)
		"~S has no parent, can't set its gravity resistance(s) yet~%" self))))

(define-method ((setter !gravity-resistance-west) (self <clus-apple>) factor)
  (let ((parent (get-parent self))
	(north-factor (!gravity-resistance-north self)))
    (if parent
	(if (clus-gravity-valid-resistance? factor)
	    (receive (pw ph)
		(get-size parent)
	      (let* ((w-factor (/ factor
				  *clus-gravity-resistance-normalization-quotient*))
		     (n-factor (/ north-factor
				  *clus-gravity-resistance-normalization-quotient*))
		     (lr-margin (* w-factor pw))
		     (tb-margin (* n-factor ph))
		     (min-margin (min lr-margin tb-margin))
		     (max-margin (max lr-margin tb-margin)))
		(case (!gravity-resistance-mode self)
		  ((asym)
		   (set-margin-left self lr-margin)
		   (set-margin-right self lr-margin))
		  ((min)
		   (set-margin self (list min-margin min-margin min-margin min-margin)))
		  ((max)
		   (set-margin self (list max-margin max-margin max-margin max-margin))))
		(slot-set! self 'gravity-resistance-west factor)))
	    (clus-display-gravity-invalid-resistance-factor factor (current-output-port)))
	(format (current-output-port)
		"~S has no parent, can't set its gravity resistance(s) yet~%" self))))

(define-method ((setter !gravity-resistance-mode) (self <clus-apple>) mode)
  (let ((parent (get-parent self)))
    (if parent
	(if (clus-gravity-resistance-mode? mode)
	    (begin
	      #;(show-gravity self (current-output-port))
	      (slot-set! self 'gravity-resistance-mode mode)
	      (set! (!gravity-resistance-north self) (!gravity-resistance-north self))
	      (set! (!gravity-resistance-west self) (!gravity-resistance-west self)))
	    (format (current-output-port)
		    "~A is not a valid gravity resistance mode.  It must be one of ~S~%"
		    mode %clus-gravity-resistance-modes))
	(format (current-output-port)
		"~S has no parent, can't set its gravity resistance mode yet~%" self))))


#!
Failed to open BO for returned DRI2 buffer (1600x900, dri2 back buffer, named 18).
This is likely a bug in the X Server that will lead to a crash soon.
Segmentation fault
!#

#;(define-method ((setter !gravity-resistance) (self <clus-apple>) factor)
  (let ((parent (get-parent self)))
    (if parent
	(if (clus-gravity-valid-resistance? factor)
	    (receive (pw ph)
		(get-size parent)
	      (let* ((n-factor (/ factor *clus-gravity-resistance-normalization-quotient*))
		     (lr-margin (* n-factor pw))
		     (tb-margin (* n-factor ph))
		     (min-margin (min lr-margin tb-margin))
		     (max-margin (max lr-margin tb-margin)))
		(case (!gravity-resistance-mode self)
		  ((min)
		   (set-margin self (list min-margin min-margin min-margin min-margin))
		   (slot-set! self 'gravity-resistance-north factor)
		   (slot-set! self 'gravity-resistance-west factor))
		  ((max)
		   (set-margin self (list max-margin max-margin max-margin max-margin))
		   (slot-set! self 'gravity-resistance-north factor)
		   (slot-set! self 'gravity-resistance-west factor))
		  (else
		   (format (current-output-port)
			   "~S gravity resistance is asymmetrical, use specific north west methods~%" self)))))
	    (clus-display-gravity-invalid-resistance-factor factor (current-output-port)))
	(format (current-output-port)
		"~S has no parent, can't set its gravity resistance(s) yet~%" self))))
