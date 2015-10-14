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


(define-module (grip passwd)
  #:export (sys/get
	    sys/get-passwd))


(define sys/get #f)

(eval-when (expand load eval)
  (let* ((id (geteuid))
	 (urec (getpwuid id))
	 (gid (passwd:gid urec))
	 (grec (getgrgid gid)))
    (set! sys/get
	  (lambda (what)
	    (case what
	      ((urec) urec)
	      ((uid) (passwd:uid urec))
	      ((uname) (passwd:name urec))
	      ((udir) (passwd:dir urec))
	      ((gid) gid)
	      ((grec) grec)
	      ((gname) (group:name grec)))))))


(define (sys/get-passwd . uname)
  (if (null? uname)
      (getpwuid (geteuid))
      (getpw (car uname))))


#!

(sys/get 'urec)
(sys/get 'uid)
(sys/get 'uname)
(sys/get 'udir)
(sys/get 'gid)
(sys/get 'grec)
(sys/get 'gname)

!#
