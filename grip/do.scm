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


(define-module (grip do)
  #:export (dotimes
	    dolist))


(define-syntax dolist
  (syntax-rules ()
    ((dolist (?var ?ls) . ?body)
     (for-each (lambda (?var) . ?body) ?ls))
    ((dolist (?var ?ls ?result) . ?body)
     (begin
       (for-each (lambda (?var) . ?body) ?ls)
       ?result))))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (?var ?count . ?result) . ?body)
     (let ((limit ?count))
       (do ((?var 0 (+ ?var 1)))
           ((>= ?var limit) . ?result)
         . ?body)))))


#!

(dolist (i '(1 2 3 4) 24) (display i) (display " "))
(dotimes (i 10 34) (display i) (display " "))

!#
