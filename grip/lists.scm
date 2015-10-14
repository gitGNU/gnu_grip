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


(define-module (grip lists)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)

  #:export (list-pos
	    list-add
	    list-replace
	    first-level-pos-second-level-lookup
	    list-pair
	    list-insert
	    interleave))


#!
;; srfi-1 has last
(define (list-last ll)
  (if (null? ll) 
      #f
      (list-ref ll (1- (length ll)))))
!#

(define (list-pos item llist pred)
  (list-index (lambda (x) (pred x item))
	      llist))

(define (list-add item llist pos)
  (receive (split-a split-b)
      (split-at llist pos)
    (append split-a
	    (list item)
	    split-b)))

(define (list-replace item new llist pred . index)
  (let ((pos (if (null? index)
		 (list-pos item llist pred)
		 (car index))))
    (if pos
	(receive (split-a split-b)
	    (split-at llist pos)
	  (append split-a
		  (list new)
		  (cdr split-b)))
	llist)))

(define (first-level-pos-second-level-lookup item llist pred)
  (let ((its-length (length llist)))
    (catch 'exit
	   (lambda ()
	     (do ((i 0 
		     (+ 1 i)))
		 ((>= i its-length) 
		  #f)
	       (if (list-pos item (list-ref llist i) pred)
		   (throw 'exit i))))
	   (lambda (key index)
	     index))))

(define (list-pair l1 l2)
  (and (pair? l1)
       (pair? l2)
       (= (length l1) (length l2))
       (do ((result (list (cons (car l1) (car l2)))
		    (cons (cons (car ll1) (car ll2))
			  result))
	    (ll1 (cdr l1) (cdr ll1))
	    (ll2 (cdr l2) (cdr ll2)))
	   ((null? ll1) (reverse! result)))))

(define (list-insert l1 l2)
  (and (pair? l1)
       (pair? l2)
       (= (length l2) (length l1))
       (do ((result (list (car l2) (car l1))
		    (cons (car ll2)
			  (cons (car ll1) result)))
	    (ll1 (cdr l1) (cdr ll1))
	    (ll2 (cdr l2) (cdr ll2)))
	   ((null? ll1) (reverse! result)))))

(define (interleave . lls)
  (concatenate (apply map list lls)))


#!

;; this is used to find a specific renderer in a 'to-pack list [of
;; (rendereri columni)] the order of which could change upon
;; time... the renderer could hold the background/foreground colour of
;; the row ... Here is a simpler example to test

(first-level-pos-second-level-lookup 5 '((1 2 3) (4 5 6) (7 8 9)) eq?)


;;;
;;; Old code
;;;

(define (list-pos item llist pred)
  (let ((its-length (length llist)))
    (catch 'exit
	   (lambda ()
	     (do ((i 0 
		     (+ 1 i)))
		 ((>= i its-length) 
		  #f)
	       (if (pred (list-ref llist i) item)
		   (throw 'exit i))))
	   (lambda (key index)
	     index))))

!#
