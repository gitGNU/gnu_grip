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

;; Old comment: this code is [old and] badly written. it needs some
;; serious cleaning/reviewing.

;;; Code:


(define-module (grip tex-utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (grip do)
  #:use-module (grip nbs)

  #:export (tex/prep-str-for-tex
	    tex/prep-str-for-tbx-env
	    tex/prep-str-for-tbx-env-nl-sp
	    tex/prep-str-for-tbx-env-1
	    tex/sanitize
	    tex/sanitize-for-table
	    tex/xspace? 
	    tex/prep-str-compute-new-length
	    tex/prep-str-compute-new-length-tbx
	    tex/write-utf8-comment-line
	    tex/write-newpage
	    tex/write-clearpage
	    tex/write-end))


;;; 
;;; normal tex environment
;;; 

(define *tex/reserved-characters*
  ;; % \ { } ~ $ & # ^ _
  '(#\%		
    ;; #\\ 	;; can't remember why i don't process it here ?
    #\{		
    #\}		
    ;; #\~	;; can't remember why i don't process it here ?
    #\$		
    #\&		
    #\#		;; symbol of parameter zone
    ;; #\^	;; can't remember why i don't process it here ?
    #\_))	;; index

;; awful, i know
(define *tex/backslash-char-list*
  '(#\\ #\t #\e #\x #\t #\b #\a #\c #\k #\s #\l #\a #\s #\h #\space))

(define *tex/backslash-char-list-1*
  '(#\\ #\t #\e #\x #\t #\b #\a #\c #\k #\s #\l #\a #\s #\h #\\ #\x #\s #\p #\a #\c #\e))

(define *tex/tilde-char-list*
  '(#\\ #\t #\e #\x #\t #\a #\s #\c #\i #\i #\t #\i #\l #\d #\e #\space))

(define *tex/tilde-char-list-1*
  '(#\\ #\t #\e #\x #\t #\a #\s #\c #\i #\i #\t #\i #\l #\d #\e #\\ #\x #\s #\p #\a #\c #\e))

(define *tex/circonf-char-list*
  '(#\\ #\t #\e #\x #\t #\a #\s #\c #\i #\i #\c #\i #\r #\c #\u #\m #\space))

(define *tex/circonf-char-list-1*
  '(#\\ #\t #\e #\x #\t #\a #\s #\c #\i #\i #\c #\i #\r #\c #\u #\m #\\ #\x #\s #\p #\a #\c #\e))

(define *tex/reversed-backslash-char-list*
  (reverse *tex/backslash-char-list*))

(define (tex/write-\+space new-str j)
  (dotimes (i 15)
    (string-set! new-str (+ i j) (list-ref *tex/backslash-char-list* i)))
  (+ j 15))

(define (tex/write-\+space-1 new-str j)
  (dotimes (i 21)
    (string-set! new-str (+ i j) (list-ref *tex/backslash-char-list-1* i)))
  (+ j 21))

(define (tex/write-tilde+space new-str j)
  (dotimes (i 16)
    (string-set! new-str (+ i j) (list-ref *tex/tilde-char-list* i)))
  (+ j 16))

(define (tex/write-tilde+space-1 new-str j)
  (dotimes (i 22)
    (string-set! new-str (+ i j) (list-ref *tex/tilde-char-list-1* i)))
  (+ j 22))

(define (tex/write-circonf+space new-str j)
  (dotimes (i 17)
    (string-set! new-str (+ i j) (list-ref *tex/circonf-char-list* i)))
  (+ j 17))

(define (tex/write-circonf+space-1 new-str j)
  (dotimes (i 23)
    (string-set! new-str (+ i j) (list-ref *tex/circonf-char-list-1* i)))
  (+ j 23))

(define (tex/xspace? str length i)
  (or (and (< (+ i 1) length)
	   (char=? (string-ref str (+ i 1)) #\space))
      (= i (- length 1))))

(define (tex/prep-str-compute-new-length str)
  (let ((its-length (string-length str))
	(new-length 0)
	(res-chars 0))
    (dotimes (i its-length)
      (let ((a-char (string-ref str i)))
	(cond ((char=? a-char #\\)
	       (set! res-chars (+ res-chars 1))
	       (if (tex/xspace? str its-length i)
		   (set! new-length (+ new-length 21))
		   (set! new-length (+ new-length 15))))
	      ((char=? a-char #\~)
	       (set! res-chars (+ res-chars 1))
	       (if (tex/xspace? str its-length i)
		   (set! new-length (+ new-length 22))
		   (set! new-length (+ new-length 16))))
	      ((char=? a-char #\^)
	       (set! res-chars (+ res-chars 1))
	       (if (tex/xspace? str its-length i)
		   (set! new-length (+ new-length 23))
		   (set! new-length (+ new-length 17))))
	      ((member a-char *tex/reserved-characters*)
	       (set! res-chars (+ res-chars 1))
	       (set! new-length (+ new-length 2)))
	      (else
	       (set! new-length (+ new-length 1)))
	      )))
    (values new-length res-chars)))

(define (tex/prep-str-for-tex str)
  (receive (new-length res-char-nb) 
      (tex/prep-str-compute-new-length str)
    (if (= res-char-nb 0)
	str
	(let ((its-length (string-length str))
	      (new-str (make-string new-length))
	      (j 0))				;; new-string offset
	  (dotimes (i its-length)		;; original str offset
	    (let ((a-char (string-ref str i)))
	      (cond ((char=? a-char #\\)
		     (if (tex/xspace? str its-length i)
			 (receive (new-offset)
			     (tex/write-\+space-1 new-str j)
			   (set! j new-offset))
			 (receive (new-offset)
			     (tex/write-\+space new-str j)
			   (set! j new-offset))))
		    ((char=? a-char #\~)
		     (if (tex/xspace? str its-length i)
			 (receive (new-offset)
			     (tex/write-tilde+space-1 new-str j)
			   (set! j new-offset))
			 (receive (new-offset)
			     (tex/write-tilde+space new-str j)
			   (set! j new-offset))))
		    ((char=? a-char #\^)
		     (if (tex/xspace? str its-length i)
			 (receive (new-offset)
			     (tex/write-circonf+space-1 new-str j)
			   (set! j new-offset))
			 (receive (new-offset)
			     (tex/write-circonf+space new-str j)
			   (set! j new-offset))))
		    ((member a-char *tex/reserved-characters*)
		     (string-set! new-str j #\\)
		     (string-set! new-str (+ j 1) (string-ref str i))
		     (set! j (+ j 2)))
		    (else
		     (string-set! new-str j (string-ref str i))
		     (set! j (+ j 1)))
		    )))
	  new-str))))


;;;
;;; tabularx
;;;

(define (tex/newline-tbx-char-list k)
  (append '(#\\ #\\) (make-list k #\&)))

(define (tex/newline-tbx-char-vect new-str j k) 
  (dotimes (i (+ k 2))
    (string-set!  new-str (+ i j)
		  (list-ref (tex/newline-tbx-char-list k) i)))
  ;; (format #t "~S~%" new-str)
  )

(define (tex/tab-tbx-char-vect new-str j)
  (dotimes (i 3)
    (string-set! new-str (+ i j) #\~)))

(define (tex/prep-str-compute-new-length-tbx str k . nl->sp)
  (let ((its-length (string-length str))
	(new-length 0)
	(res-chars 0))
    (dotimes (i its-length)
      (let ((a-char (string-ref str i)))
	(cond ((char=? a-char #\\)
	       (set! res-chars (+ res-chars 1))
	       (if (tex/xspace? str its-length i)
		   (set! new-length (+ new-length 21))
		   (set! new-length (+ new-length 15))))
	      ((char=? a-char #\~)
	       (set! res-chars (+ res-chars 1))
	       (if (tex/xspace? str its-length i)
		   (set! new-length (+ new-length 22))
		   (set! new-length (+ new-length 16))))
	      ((char=? a-char #\^)
	       (set! res-chars (+ res-chars 1))
	       (if (tex/xspace? str its-length i)
		   (set! new-length (+ new-length 23))
		   (set! new-length (+ new-length 17))))
	      ((member a-char *tex/reserved-characters*)
	       (set! res-chars (+ res-chars 1))
	       (set! new-length (+ new-length 2)))
	      ((char=? a-char #\space)
	       (set! new-length (+ new-length 1)))
	      ((char=? a-char #\newline)
	       (set! res-chars (+ res-chars 1))       
	       (if (null? nl->sp)
		   (set! new-length (+ new-length k 2))
		   (set! new-length (+ new-length 1))))
	      ((char=? a-char #\tab)
	       (set! res-chars (+ res-chars 1))
	       (set! new-length (+ new-length 3)))
	      (else (set! new-length (+ new-length 1))))
	))
    (values new-length res-chars)))

(define (tex/prep-str-for-tbx-env str k . nl->sp)
  ;; k is the column 'id' [in the table], so it is also the number of
  ;; #\& to be added when str has #\newline(s) and nl->sp is #f
  (receive (new-length res-char-nb) 
      (if (null? nl->sp)
	  (tex/prep-str-compute-new-length-tbx str k)
	  (tex/prep-str-compute-new-length-tbx str k #t))
    (if (= res-char-nb 0)
	str
	(let* ((its-length (string-length str))
	       (prev-char #f)
	       (new-str (make-string new-length))
	       (j 0))
	  (dotimes (i its-length)
	    (let ((a-char (string-ref str i)))
	      (cond ((char=? a-char #\\) 
		     (if (tex/xspace? str its-length i)
			 (receive (new-offset)
			     (tex/write-\+space-1 new-str j)
			   (set! j new-offset))
			 (receive (new-offset)
			     (tex/write-\+space new-str j)
			   (set! j new-offset))))
		    ((char=? a-char #\~) 
		     (if (tex/xspace? str its-length i)
			 (receive (new-offset)
			     (tex/write-tilde+space-1 new-str j)
			   (set! j new-offset))
			 (receive (new-offset)
			     (tex/write-tilde+space new-str j)
			   (set! j new-offset))))
		    ((char=? a-char #\^) 
		     (if (tex/xspace? str its-length i)
			 (receive (new-offset)
			     (tex/write-circonf+space-1 new-str j)
			   (set! j new-offset))
			 (receive (new-offset)
			     (tex/write-circonf+space new-str j)
			   (set! j new-offset))))
		    ((member a-char *tex/reserved-characters*)
		     (string-set! new-str j #\\)
		     (string-set! new-str (+ j 1) (string-ref str i))
		     (set! j (+ j 2)))
		    ((char=? a-char #\newline)
		     (if (null? nl->sp)
			 (begin 
			   (tex/newline-tbx-char-vect new-str j k)
			   (set! j (+ j k 2)))
			 (begin 
			   (string-set! new-str j #\space)
			   (set! j (+ j 1)))))
		    ((char=? a-char #\tab)
		     (tex/tab-tbx-char-vect new-str j)
		     (set! j (+ j 3)))
		    (else
		     (string-set! new-str j (string-ref str i))
		     (set! j (+ j 1))))))
	  new-str))))


(define (tex/write-utf8-comment-line ostream)
  (format ostream "% -*- coding: utf-8; -*-~%"))

(define (tex/write-clearpage ostream)
  (format ostream "~%\\clearpage~%"))

(define (tex/write-newpage ostream)
  (format ostream "~%\\newpage~%"))

(define (tex/write-end ostream name)
  (format ostream "~%\\end{~A}~%" name))


;;;
;;; Code using regular expressions
;;;

;; not used yet: is it really better? lol

(define (tex/sanitize str)
  ;; 1: replace \ by \textbackslash
  ;; 2: escaping % { } ~ $ & # ^ _ with \
  (let* ((tra-1 (regexp-substitute/global #f "([\\])" str 'pre  "\\textbackslash" 'post))
	 (tra-2 (regexp-substitute/global #f "([%{}~$&#^_])" tra-1 'pre  "\\" 1 'post)))
    tra-2))

;; who wrote this ?
;;   1: replace "\" by "\textbackslash "
;;   2: replace "\textbackslash " by "\textbackslash~"
;;   3: replace newline by \\ followed by as many & as index
;;   4: replace tab by "~~~"
;;   5: escape all other reserved chars: % { } ~ $ & # ^ _

(define (tex/sanitize-for-table str index) 
  (let* ((tra-1 (regexp-substitute/global #f "([\\])" str 'pre  "\\textbackslash" 'post))
	 (tra-2 (regexp-substitute/global #f "\\textbackslash " tra-1 'pre  "\textbackslash~" 'post))
	 (tra-3 (regexp-substitute/global
		 #f "\\n" tra-2 'pre (format #f "\\\\~A" (make-string index #\&)) 'post))
	 (tra-4 (regexp-substitute/global #f "\\t" tra-3 'pre  "~~~" 'post))
	 (tra-5 (regexp-substitute/global #f "([%{}~$&#^_])" tra-4 'pre  "\\" 1 'post)))
    tra-5))


#!

(tex/prep-str-for-tex "david\\\\pirotte")
(tex/prep-str-for-tex "% { } ~ $ & # ^ _ \\")
(tex/prep-str-for-tbx-env "% { } ~ $ & # ^ _ \\" 0)

!#
