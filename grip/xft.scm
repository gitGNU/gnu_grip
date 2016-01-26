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


(define-module (grip xft)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (grip reexport)
  #:use-module (grip push)
  #:use-module (grip utils)
  #:use-module (grip strings)
  #:use-module (grip nbs fp)

  #:export (xft/get-settings
	    xft/get))


(define xft/get-settings #f)
(define xft/get #f)

(eval-when (expand load eval)
  (re-export-public-interface (grip push)
			      (grip utils)
			      (grip strings)
			      (grip nbs fp))

  (define (get-xft-settings)
    (let* ((results (list))
	   (cmd "xrdb -query | grep Xft")
	   (s (open-input-pipe cmd)))
      (do ((line (read-line s) (read-line s)))
	  ((eof-object? line))
	(set! results (cons line results)))
      (unless (zero? (status:exit-val (close-pipe s)))
	(error "subprocess returned non-zero result code" cmd))
      (reverse! results)))

  (let ((xft-settings (get-xft-settings))
	(xft-settings-2 (list))
	(xft-dpi-ratio -1))
    (for-each (lambda (xft-setting)
		(let* ((tokens (str/get-tokens xft-setting (char-set #\tab)))
		       (key (car tokens))
		       (value (cadr tokens)))
		  (let* ((name (substring key 0 (1- (string-length key))))
			 (tokens (str/get-tokens name (char-set #\.)))
			 (class (car tokens))
			 (param (cadr tokens))
			 (value-2 (or (string->number value) value)))
		    (storage-set (string->symbol name) value-2)
		    (push! (cons (string->symbol param) value-2) xft-settings-2))))
	(get-xft-settings))
    (if (storage-get 'Xft.dpi)
	(storage-set 'Xft.dpi.ratio (/ (storage-get 'Xft.dpi) 96))
	(storage-set 'Xft.dpi.ratio 1.0))
    (storage-set 'apply-dpi-ratio? (not (fp/=? (storage-get 'Xft.dpi.ratio) 1.0 1.0E-2)))
    (push! (cons 'dpi.ratio (storage-get 'Xft.dpi.ratio)) xft-settings-2)
    (set! xft-settings-2 (reverse! xft-settings-2))
    (set! xft/get-settings (lambda () xft-settings-2))))


#!

(xft/get-settings)
(storage-get 'Xft.antialias)
(storage-get 'Xft.autohint)
(storage-get 'Xft.dpi)
(storage-get 'Xft.dpi.ratio)
(storage-get 'Xft.hinting)
(storage-get 'Xft.hintstyle)
(storage-get 'Xft.rgba)

(storage-get 'apply-dpi-ratio?)

!#
