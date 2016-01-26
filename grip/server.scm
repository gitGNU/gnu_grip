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


(define-module (grip server)
  #:use-module (system repl server)
  ;; #:use-module (grip utils)

  #:export (run-server-any-port
	    spawn-server-any-port))


(define (start-server-any-port port server proc)
  #;(dimfi port server proc)
  (if server
      (values port server)
      (let ((new-port (+ port 1)))
	(start-server-any-port new-port
			       (catch #t
				 (lambda ()
				   (proc (make-tcp-server-socket #:port new-port)))
				 (lambda (key . parameters)
				   #f))
			       proc))))

(define* (run-server-any-port #:key (start 1968))
  (start-server-any-port (1- start) #f run-server))

(define* (spawn-server-any-port #:key (start 1968))
  (start-server-any-port (1- start) #f spawn-server))


#!

;; missing good example/mini tests

!#
