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

;; Old comment: date regexp keywords, yet to come other natural language
;; based expressions and months do not use i18n yet...

;;; Code:


(define-module (grip dates)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (grip reexport)
  #:use-module (grip do)
  #:use-module (grip regexp)

  #:export (date/get-filters
	    date/get-filter
	    date/system-date
	    date/system-time
	    date/leap-year?
	    date/get-match
	    date/valid-date?
	    date/iso-date
	    sys/date))


(eval-when (compile load eval)
  (re-export-public-interface (ice-9 format)
			      (ice-9 receive)
			      (ice-9 regex)
			      (grip do)
			      (grip regexp)))


(define date/build-date-regexp-str
  (let ((date-re-str
	 "([[:digit:]]+)([./-])([[:digit:]]+)\\~A([[:digit:]]+)"))
    (lambda (sep-group-pos)
      (format #f "~?" date-re-str
	      (list sep-group-pos)))))

(define date/get-filters
  (let* ((date-re-str (date/build-date-regexp-str 2))
	 (op-re-str (string-append "([=<>]|[<>]=)?" "([[:blank:]]*)"
				   (date/build-date-regexp-str 4)))
	 (range-re-str (string-append (date/build-date-regexp-str 2)
				      "(\\.\\.)" (date/build-date-regexp-str 7)))
	   (kword-re-str "today|this month|this year")
	   (month-re-str "(jan|feb|mar|apr|jun|jul|aug|sep|oct|nov|dec)([[:alpha:]]*)([- ]*)([[:digit:]]+)")
	   ;; filters is ordered in such a way that for-each / catch exit can be
	   ;; used when looking for the right filter to apply based on end-user
	   ;; entry
	   (filters `((kword . (,kword-re-str . ,(make-regexp kword-re-str)))
		      (month . (,month-re-str . ,(make-regexp month-re-str)))
		      (range . (,range-re-str . ,(make-regexp range-re-str)))
		      (operator . (,op-re-str . ,(make-regexp op-re-str)))
		      ;; this is for consistent interface purposes
		      ;; [getting a single match value], since in the
		      ;; case of looking for a match, operator will
		      ;; match in case of a single date entry
		      (date . (,date-re-str . ,(make-regexp date-re-str))))))
	  (lambda () filters)))


;;;
;;; Non API
;;;

(define (date/process-operator-match match)
  (let* ((filter (regexp/process-match match
				       `((,identity . (1)) (,string->number . (3 5 6)))))
	 (operator (caar filter))
	 (date (cadr filter)))
    (and (date/valid-day-month-year? (car date) (cadr date) (caddr date))
	 (list 'operator 
	       (cond ((not operator) "=")
		     ((string-null? operator) "=")
		     (else operator))
	       (date/iso-date date)))))

(define (date/process-range-match match)
  (let* ((filter (regexp/process-match match
				       `((,identity . (1))
					 (,string->number . (1 3 4))
					 (,string->number . (6 8 9)))))
	 (operator (caar filter))
	 (d-inf (cadr filter))
	 (d-sup (caddr filter)))
    (and (date/valid-day-month-year? (car d-inf) (cadr d-inf) (caddr d-inf))
	 (date/valid-day-month-year? (car d-sup) (cadr d-sup) (caddr d-sup))
	 (list 'range (date/iso-date d-inf) (date/iso-date d-sup)))))

(define (date/process-filter-match match)
  ;; (format #t "match: ~S~%" match)
  (case (car match)
    ((operator) (date/process-operator-match (cdr match)))
    ((range) (date/process-range-match (cdr match)))
    (else
     match)))


;;;
;;; API
;;;

(define (date/get-filter entry)
  (let ((match? (regexp/get-match entry (date/get-filters))))
    (and match?
	 ;; (regexp/show-match match?)
	 (date/process-filter-match match?))))

(define (date/leap-year? year)
  (or (and (zero? (modulo year 4))
	   (not (zero? (modulo year 100))))
      (zero? (modulo year 400))))

(define (date/get-match date)
  (let ((m (regexp-exec (regexp/get-regexp 'date (date/get-filters)) date)))
    (if m
	(values (match:substring m 1)
		(match:substring m 3)
		(match:substring m 4))
	(values #f #f #f))))

(define (date/valid-day-month-year? day month year)
  (unless (number? day)
    (set! day (string->number day))
    (set! month (string->number month))
    (set! year (string->number year)))
  (if (and (>= year 1970) ;; sqlite3 strftime _limitations_
	   (<= year 2037)
	   (<= 1 month)
	   (<= month 12)
	   (<= 1 day)
	   (<= day (case month
		     ((1 3 5 7 8 10 12) 31)
		     ((4 6 9 11) 30)
		     ((2) (if (date/leap-year? year) 29 28)))))
      (values day month year)
      (values #f #f #f)))

(define (date/valid-date? date)
  (cond ((string? date)
	 (receive (day month year)
	     (date/get-match date)
	   (if day
	       (date/valid-day-month-year? day month year)
	       (values #f #f #f))))
	((list? date)
	 (date/valid-day-month-year? (car date) (cadr date) (caddr date)))
	(else
	 (format #t "Error - date/valid-date?: unkown date type ~S~%" date)
	 (values #f #f #f))))

(define (date/system-date . fmt)
  (if (null? fmt)
      (strftime "%d.%m.%Y" 
		(localtime (current-time)))
      (strftime (car fmt)
		(localtime (current-time)))))

(define (date/system-time . fmt)
  (if (null? fmt)
      (strftime "%H:%M %Z"
		(localtime (current-time)))
      (strftime (car fmt)
		(localtime (current-time)))))

(define (date/iso-date date)
  ;; date is assumed to be or:
  ;;   a valid 'european' string date;
  ;;   a list of valid day month year
  (cond ((string? date)
	 (receive (day month year)
	     (date/get-match date)
	   (format #f "~A-~2,,,'0@A-~2,,,'0@A" year month day)))
	((list? date)
	 (format #f "~A-~2,,,'0@A-~2,,,'0@A" (caddr date) (cadr date) (car date)))
	(else
	 (format #t "Error - date/valid-date?: unkown date type ~S~%" date)
	 #f)))

(define (sys/date . rest)
  (let* ((cmd (if (pair? rest)
		  (format #f "date '+~A'" (car rest))
		  "date"))
	 (port (open-input-pipe cmd))
	 (line (read-line port)))
    (unless (zero? (status:exit-val (close-pipe port)))
      (error "subprocess returned non-zero result code" cmd))
    line))


#!

(date/system-date)
(date/system-date "%Y.%m.%d")

(sys/date)
(sys/date "%Y.%m.%d")

(date/valid-date? "26-05-2011")
(date/valid-date? (list 26 5 2011))

(date/valid-date? "29/2/2008")
(date/valid-date? "29/2/2011")

(date/iso-date "26-05-2011")
(date/iso-date "30/09/2010")
(date/iso-date "2-5-2011")

;; time integer from Sqlite
(define time 1270252800)
;; select strftime('%s','2010-04-03');

(strftime "%s" (strptime "%d/%m/%Y" "03/04/2010"))
(strptime "%d/%m/%Y" "03/04/2010")
(strftime "%d/%m/%Y" 1270252800)
(strftime "%d/%m/%Y"(localtime time))


;;;
;;; srfi-19
;;;

(use-modules (srfi srfi-19))

(current-date)
(string->date "01.01.2010" "~d.~m.~Y")
(string->date "01.13.2010" "~d.~m.~Y")


;;;
;;; Regexp check
;;;

(date/get-match "1.2.1999")
(date/get-match "1.2.1999")
(date/get-match "1.2.1999")
(date/get-match "1.2.1999")

(date/get-filter "=")
(date/get-filter "=2.2.2012")
(date/get-filter ">=2.2.2012")

(date/get-filter "1.1.2010..1.1.2011")

(date/get-filter "today")
(date/get-filter "jan")
(date/get-filter "jan 2010")
(date/get-filter "january 2010")

!#
