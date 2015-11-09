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


(define-module (grip locale)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (grip reexport)
  #:use-module (grip utils)

  #:export (set-locale
	    sys/get-locales
	    sys/get-utf8-locales
	    sys/get-idioms
	    sys/get-utf8-idioms
	    sys/get-utf8-idioms-radicals
	    sys/get-utf8-fallback))


(define (set-locale)
  ;; (setlocale LC_ALL "") raises an exception when the user's environment
  ;; varibable $LANG is badly defined or defined using a non installed
  ;; locale.  If this is the case, we catch it and display a warning
  ;; message instead.
  (catch #t
    (lambda () (setlocale LC_ALL ""))
    (lambda (key . parameters)
      (let ((lang (getenv "LANG")))
	(format (current-error-port)
		"~%WARNING: could not set Guile's ports locale to: ~A.~%~%"
		lang)))))


(define sys/get-locales #f)
(define sys/get-utf8-locales #f)
(define sys/get-idioms #f)
(define sys/get-utf8-idioms #f)
(define sys/get-utf8-idioms-radicals #f)
(define sys/get-utf8-fallback #f)


(eval-when (expand load eval)
  (re-export-public-interface (grip utils))

  (define (get-locales)
    (let ((results (list))
	  (s (open-input-pipe "locale -a")))
      (do ((line (read-line s) (read-line s)))
	  ((eof-object? line))
	(set! results (cons line results)))
      (close s)
      (reverse! results)))

  (define (get-utf8-locales locales)
    (let ((valid-utf8-encoding '("utf8" "UTF-8")))
      (filter (lambda (locale)
		(let ((splitted (string-split locale #\.)))
		  (and (not (null? (cdr splitted)))
		       (let ((encoding (cadr splitted)))
			 (or (string=? encoding (car valid-utf8-encoding))
			     (string=? encoding (cadr valid-utf8-encoding)))))))
	      locales)))

  (define (get-idioms locales)
    (let ((idioms (list)))
      (for-each (lambda (locale)
		  (set! idioms (cons (car (string-split locale #\.)) idioms)))
	  locales)
      (reverse! idioms)))

  (define (get-utf8-idioms utf8-locales)
    (let ((results (list)))
      (for-each (lambda (locale)
		  (set! results (cons (car (string-split locale #\.)) results)))
	  utf8-locales)
      (reverse! results)))

  (define (get-utf8-idioms-radicals utf8-idioms)
    (let ((results (list)))
      (for-each (lambda (idiom)
		  (set! results (cons (car (string-split idiom #\_)) results)))
	  utf8-idioms)
      (reverse! results)))

  (define (get-utf8-fallback proposed utf8-idioms-radicals utf8-locales)
    ;; proposed is a list of idioms radicals, i.e., "C", "en"... when
    ;; we find one, by construction the fallback is at the same pos in
    ;; utf8-locales.
    (let* ((its-length (length proposed))
	   (offset (catch 'exit
		     (lambda ()
		       (do ((i 0 (1+ i)))
			   ((= i its-length) #f)
			 (let ((item (list-ref proposed i)))
			   (if (member item utf8-idioms-radicals)
			       (throw 'exit i)))))
		     (lambda (key index) index))))
      (and offset (list-ref utf8-locales offset))))

  (let* ((lang (getenv "LANG"))
	 (locales (get-locales))
	 (utf8-locales (get-utf8-locales locales))
	 (idioms (get-idioms locales))
	 (utf8-idioms (get-utf8-idioms utf8-locales))
	 (utf8-idioms-radicals (get-utf8-idioms-radicals utf8-idioms)))
    (storage-set 'lang lang)
    (set! sys/get-locales (lambda () locales))
    (set! sys/get-utf8-locales (lambda () utf8-locales))
    (set! sys/get-idioms (lambda () idioms))
    (set! sys/get-utf8-idioms (lambda () utf8-idioms))
    (set! sys/get-utf8-idioms-radicals (lambda () utf8-idioms-radicals))
    (set! sys/get-utf8-fallback 
	  (lambda (proposed)
	    (get-utf8-fallback proposed utf8-idioms-radicals utf8-locales)))))


#!

(storage-get 'lang)
(sys/get-locales)
(sys/get-utf8-locales)
(sys/get-idioms)
(sys/get-utf8-idioms)
(sys/get-utf8-idioms-radicals)
(sys/get-utf8-fallback '("C" "en"))

!#
