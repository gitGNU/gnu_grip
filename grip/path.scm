;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2015
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

;;; Commentary:

;; merge-paths, remove-dot-segments snarfed from guix, thank you Mark H
;; Weaver [https://savannah.gnu.org/projects/guix], but locally
;; modified: here we deal with GNU Linux pathnames only, no URL,
;; renaming base-path as prefix, components as items, and:

;; merge-paths:
;;   won't drop the last prefix-item unless ""
;;   checks that rel-path is a relative path

;; remove-dot-segments:
;;   won't remove leading ".."
;;   removes inner ".." only if 'possible'

;;; Code:


(define-module (grip path)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (split-path
	    merge-paths
	    remove-dot-segments))


(define (split-path path)
  (string-split path #\/))

(define (merge-paths prefix rel-path)
  (let* ((prefix-items (split-path prefix))
	 (filtered-prefix-items (match prefix-items
				  (() '())
				  ((items ... "") items)
				  (_ prefix-items)))
	 (prefix (string-join filtered-prefix-items "/"))
	 (rel-path (match (split-path rel-path) 
		     (("" . items)
		      (error "merge-paths: rel-path is not a relative path" rel-path))
		     (_ rel-path))))
    (string-append prefix "/" rel-path)))

(define (remove-dot-segments path)
  ;; absolute paths start with ""
  (let loop
      ((in (drop-while (match-lambda ("." #t) (_ #f))
		       (split-path path)))
       (out '()))
    (match in
      (("." . rest)
       (loop rest out))
      ((".." . rest)
       (match out
	 ((or () ("") (".." . _))
	  (loop rest (cons ".." out)))
	 (_ (loop rest (cdr out)))))
      ((item . rest)
       (loop rest (cons item out)))
      (()
       (string-join (reverse out) "/")))))
