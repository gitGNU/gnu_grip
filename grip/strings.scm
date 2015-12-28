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


(define-module (grip strings)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  
  #:export (str/replace-all
	    str/prep-str-for-sql
	    str/prep-str-for-fs
	    str/get-tokens
	    str/contains?
	    str/delete
	    str/span
	    str/read))


(define (str/replace-all str search-for replace-by)
  (regexp-substitute/global #f search-for str 'pre replace-by 'post))

(define (str/prep-str-for-sql str)
  ;; ' -> ''
  (if (string-index str #\')
      (let ((quote (make-string 1 #\'))
	    (2quotes (make-string 2 #\')))
	(str/replace-all str quote 2quotes))
      str))

(define (str/prep-str-for-fs str)
  ;; #\space -> #\\#\space
  (if (string-index str #\space)
      (str/replace-all str " " "\\ ")
      str))

(define (str/get-tokens s cs)
  (string-tokenize s (char-set-complement cs)))

(define (str/contains? s1 s2)
  (let* ((l-index (string-contains s1 s2))
	 (s2-length (string-length s2))
	 (r-index (and l-index (+ l-index s2-length))))
    (values l-index r-index s2-length)))

(define (str/delete s1 s2)
  (receive (l-index r-index s2-length)
      (str/contains? s1 s2)
    (if l-index
	(if (= l-index 0)
	    (substring s1 s2-length)
	    (let ((a (substring s1 0 l-index))
		  (b (substring s1 (+ l-index s2-length))))
	      (string-append a b)))
	s1)))


(define (str/span item property value)
  (string-append "<span " property "=\"" value "\">" item "</span>"))

(define (str/read str)
  (with-input-from-string str read))


#!

;; missing good example/mini tests

!#
