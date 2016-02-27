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

;;; Code:


(define-module (grip config)
  #:use-module (ice-9 format)
  #:use-module (grip passwd)

  #:export (get-config-filename
	    read-config
	    write-config))


(define (get-config-filename f-name)
  (string-append (sys/get 'udir) "/" ".config/" f-name ".conf"))

#;(define (sys/read filename)
  (if (access? filename R_OK)
      (let* ((istream (open-input-file filename))
	     (line (read istream)))
	(close istream)
	line)
      #f))

(define (read-config f-name)
  (catch #t
    (lambda ()
      (call-with-input-file
	  (get-config-filename f-name)
	read))
    (lambda (key . parameters)
      #f)))

(define (write-config f-name what)
  (call-with-output-file
      (get-config-filename f-name)
    (lambda (port)
      (format port "~S~%" what))))


#!

(read-config "kise")
(write-config '((db-file . "/home/david/alto/db/sqlite.alto.david.db") (open-at-startup . #t) (ulogo . "/usr/alto/logos/Logo-blue") (win-x . 350) (win-y . 134) (win-w . 446) (win-h . 631)))

!#
