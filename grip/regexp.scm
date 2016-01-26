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


(define-module (grip regexp)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  ;; #:use-module (ice-9 popen)
  ;; #:use-module (ice-9 rdelim)
  #:use-module (grip reexport)
  #:use-module (grip do)

  #:export (regexp/show-match
	    regexp/get-regexp-str
	    regexp/get-regexp
	    regexp/get-match
	    regexp/process-match))


(eval-when (expand load eval)
  (re-export-public-interface (ice-9 format)
			      (ice-9 receive)
			      (ice-9 regex)
			      (grip do)))


(define (regexp/show-match m)
  (and m
       (format #t "~S~%" m)
       (if (pair? m)
	   (dotimes (i (match:count (cdr m)))
	     (format #t "~A:  ~S~%" i (match:substring (cdr m) i)))
	   (dotimes (i (match:count m))
	     (format #t "~A:  ~S~%" i (match:substring m i))))
       m))

;; Notes:
;;   filters: ((id . (regexp-str . regexp))+)

(define (regexp/get-regexp-str what filters)
  (let ((pair (assoc what filters)))
    (and pair (car (cdr pair)))))

(define (regexp/get-regexp what filters)
  (let ((pair (assoc what filters)))
    (and pair (cddr pair))))

(define (regexp/get-match entry filters)
  ;; entry:   string [an end-user entry]
  (catch 'exit
    (lambda ()
      (dolist (filter filters #f)
	(let* ((what (car filter))
	       (regexp (regexp/get-regexp what filters))
	       (matched? (regexp-exec regexp entry)))
	  (if matched? (throw 'exit (cons what matched?))))))
    (lambda (key match)
      match)))

(define (regexp/process-match match hows)
  ;; (regexp/show-match match)
  (let ((result (list)))
    (for-each (lambda (how)
		;; how: (converter (match-offsets+))
		(let ((convert (car how))
		      (i-result (list)))
		  (for-each (lambda (offset)
			      (set! i-result
				    (cons (convert (match:substring match offset))
					  i-result)))
		      (cdr how))
		  (set! result
			(cons (reverse! i-result) result))))
	hows)
    (reverse! result)))


#!

;; missing good example/mini tests

!#
