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


(define-module (grip db filters)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  ;; #:use-module (ice-9 popen)
  ;; #:use-module (ice-9 rdelim)
  #:use-module (grip reexport)
  #:use-module (grip do)
  #:use-module (grip dates)
  #:use-module (grip regexp)

  #:export (dbf/get-text-filters
	    dbf/get-date-filter
	    dbf/get-text-filter
	    dbf/get-filter))


(define dbf/get-text-filters #f)

(eval-when (expand load eval)
  (re-export-public-interface (ice-9 format)
			      (ice-9 receive)
			      (ice-9 regex)
			      (grip do)
			      (grip dates)
			      (grip regexp))
  (let* ((empty-re-str "^(=)$")			;; "="
	 (not-empty-re-str "^(\\*)$")		;; "*"
	 (equal-re-str "^(=)(.+)")		;; "=bla bla"
	 (contains-re-str "^(\")(.+)(\")$") 	;; "\"bli bli\""
	                                        ;; <- lacking 'surrounded by' [:punct:] class
	 (oan-re-str "[|&!]+")			;; "demu&demi|dema!demand"
	 (like1-re-str "(.+)")			;; "hello", "*hello*"
	 (like2-re-str "(\\*)(.+)")		;; "*blue"
	 (like3-re-str "(.+)(\\*)")		;; "blue*"
	 (like4-re-str "(\\*)(.+)(\\*)")	;; "*blue*"
	 ;; filters are ordered -> for-each/catch can be used...
	 (filters `((empty . (,empty-re-str . ,(make-regexp empty-re-str)))
		    (not-empty . (,not-empty-re-str . ,(make-regexp not-empty-re-str)))
		    (equal . (,equal-re-str . ,(make-regexp equal-re-str)))
		    (contains . (,contains-re-str . ,(make-regexp contains-re-str)))
		    (oan . (,oan-re-str . ,(make-regexp oan-re-str)))
		    (like4 . (,like4-re-str . ,(make-regexp like4-re-str)))
		    (like3 . (,like3-re-str . ,(make-regexp like3-re-str)))
		    (like2 . (,like2-re-str . ,(make-regexp like2-re-str)))
		    (like1 . (,like1-re-str . ,(make-regexp like1-re-str))))))
    (set! dbf/get-text-filters (lambda () filters))))

(define (dbf/process-oan-match-1 matches entry)
  (let ((result (list))
	(prev-match #f))
    (for-each (lambda (match)
		(let ((start (match:start match 0))
		      (end (match:end match 0))
		      (prev-end  (and prev-match (match:end prev-match 0))))
		  ;; (format #t "~S~%" match)
		  (set! result
			(cons (if (null? result)
				  (if (= start 0)
				      (list (match:substring match 0))
				      (list (string-ref entry 0) (match:substring match 0)))
				  (list (string-ref entry prev-end) (match:substring match 0)))
			      result))
		  (set! prev-match match)))
	matches)
    (reverse! result)))

(define (dbf/process-contains-match match entry)
  (list 'contains (match:substring match 2)))

(define (dbf/process-oan-match match entry)
  (let* ((filter (make-regexp "[^|&!]+"))
	 (matches (list-matches filter entry)))
    ;; we also need to know matches conjunction: |, & or !
    (list 'oan (dbf/process-oan-match-1 matches entry))))

(define (dbf/process-match match entry)
  (let ((type (car match))
	(match (cdr match)))
    (case type
      ;; this is a special case, we don't need to match:substring
      ((empty) (list 'empty))
      ((not-empty) (list 'not-empty))
      ((equal) (list 'equal (substring entry 1)))
      ((contains) (dbf/process-contains-match match entry))
      ((oan) (dbf/process-oan-match match entry))
      ((like1) (list 'like1 entry))
      ((like2) (list 'like2 (substring entry 1)))
      ((Like3) (list 'like3 (substring entry 0 (1- (string-length entry)))))
      ((like4) (list 'like4 (substring entry 1 (1- (string-length entry)))))
      (else
       (format #t "warning: no such match type: ~S~%" type)
       #f))))

(define (dbf/get-text-filter-1 entry)
  (let ((match? (regexp/get-match entry (dbf/get-text-filters))))
    (and match?
	 ;; (regexp/show-match match?)
	 (dbf/process-match match? entry))))


;;;
;;; SQL related code
;;;

(define (dbf/contains-regexp-str)
  "~A = '~A'
   or ~A REGEXP '([[:space:]|[:punct:]])+(~A)$'
   or ~A REGEXP '^(~A)([[:space:]|[:punct:]])+'
   or ~A REGEXP '([[:space:]|[:punct:]])+(~A)([[:space:]|[:punct:]])'")

(define (dbf/get-contains-sql-exp colname value)
  (format #f "~?" (dbf/contains-regexp-str)
	  (list colname value colname value colname value colname value)))

(define (dbf/oan-get-sql-sub-op colname user-op pos type)
  ;; user-op is a char: #\| #\& #\!
  ;; @ pos 0, it only accept #\! [not]
  ;; when type is 'regexp colname is omited.
  (case pos
    ((0) (if (char=? user-op #\!) 
	     (case type
	       ((normal) (format #f "~A not" colname))
	       ((regexp) "not"))
	     ""))
    (else
     (case type
       ((normal) (cond ((char=? user-op #\|)
			(format #f "or ~A" colname))
		       ((char=? user-op #\&)
			(format #f "and ~A" colname))
		       ((char=? user-op #\!)
			(format #f "and ~A not" colname))))
       ((regexp) (cond ((char=? user-op #\|) "or")
		       ((char=? user-op #\&) "and"
		       ((char=? user-op #\!) "and not"))))))))

(define (dbf/oan-build-sql-sub-expr colname oan-matches)
  ;; ex:
  ;; (("demu") (#\| "demi") (#\| "dema") (#\! "demand"))
  ;; -> "~A like '%~A%' or ~A like '%~A%'"
  ;; (dimfi "oan" oan-matches)
  (let ((result (list))
	(contains-regexp (regexp/get-regexp 'contains (dbf/get-text-filters))))
    (for-each (lambda (oan-match)
		(let* ((op1? (and (not (null? (cdr oan-match))) (car oan-match)))
		       (value (string-trim-both (if op1? (cadr oan-match) (car oan-match))))
		       (word? (regexp-exec contains-regexp value))
		       (op? (and op1?
				 (dbf/oan-get-sql-sub-op colname op1? (length result) (if word? 'regexp 'normal)))))
		  (if word?
		      (let* ((word (cadr (dbf/process-contains-match word? "")))
			     (contains-sub-exp (dbf/get-contains-sql-exp colname word)))
			;; (format #t "word?: ~S, value: ~A~%" word? word)
			;; (format #t "contains: ~S~%" contains-sub-exp)
			(set! result
			      (cons (if op?
					(format #f "~A (~A)" op? contains-sub-exp)
					(format #f "(~A)" contains-sub-exp))
				    result)))
		      (set! result
			    (cons (if (null? (cdr oan-match))
				      (format #f "~A like '%~A%'" colname (string-trim-both (car oan-match)))
				      (format #f "~A like '%~A%'" op? (string-trim-both  (cadr oan-match))))
				  result)))))
	oan-matches)
    (reverse! result)))

(define (dbf/oan-build-sql-expr colname oan-matches)
  (let ((sub-exprs (dbf/oan-build-sql-sub-expr colname oan-matches)))
    (let ((result "("))
      (for-each (lambda (sub-expr)
		  (set! result
			(string-append result
				       (if (string-null? result) "" " ")
				       sub-expr)))
	  sub-exprs)
      (string-append result ")"))))

(define (dbf/get-text-filter colname entry)
  ;; return or :
  ;;   an sql(ite) text filter condition;
  ;;   #f.
  (let* ((trimed (string-trim-both entry char-set:blank))
	 (filter (and (not (string-null? trimed))
		      (dbf/get-text-filter-1 trimed))))
    (and filter
	 (case (car filter)
	   ((empty) (format #f "(~A is null or ~A = '')" colname colname))		;; "="
	   ((not-empty) (format #f "not (~A is null or ~A = '')" colname colname))	;; "*"
	   ((equal) (format #f "~A = '~A'" colname (cadr filter)))			;; "=bla bla"
	   ((contains) (dbf/get-contains-sql-exp colname (cadr filter)))		;; "\"bli bli\""
	   ((oan) (dbf/oan-build-sql-expr colname (cadr filter)))			;; "demu&demi|dema!demand"
	   ((like1 like4) (format #f "~A like '%~A%'" colname (cadr filter)))		;; "hello", "*hello*"
	   ((like2) (format #f "~A like '%~A'" colname (cadr filter)))			;; "*blue"
	   ((like3) (format #f "~A like '~A%'" colname (cadr filter)))))))		;; "blue*"


;;;
;;; Date related
;;;

(define (dbf/get-date-filter colname entry)
  ;; returns or :
  ;;   an sqlite date filter condition;
  ;;   #f.
  (let* ((trimed (string-trim-both entry char-set:blank))
	 (filter (and (not (string-null? trimed))
		      (date/get-filter trimed))))
    (and filter
	 (case (car filter)
	   ;; (operator "=" "2010-01-01")
	   ((operator) (format #f "~A ~A strftime('%s','~A')" colname (cadr filter) (caddr filter)))
	   ((range) (format #f "(~A >= strftime('%s','~A') AND ~A <= strftime('%s','~A'))"
			    colname (cadr filter) colname (caddr filter)))
	   (else
	    "")))))


;;;
;;; Main API
;;;

(define (dbf/get-filter specs)
  ;; specs: (colname coltype u-filter)
  (let ((result ""))
    (for-each (lambda (spec)
		(let* ((colname (car spec))
		       (coltype (cadr spec))
		       (entry (caddr spec))
		       (filter (case coltype
				    ((date) (dbf/get-date-filter colname entry))
				    ((text) (dbf/get-text-filter colname entry))
				    (else
				     (format #t "Error - dbf/get-filter: unkown type ~S~%" coltype)
				     #f))))
		  (unless (or (not filter)
			      (string-null? filter))
		    (set! result
			  (if (string-null? result)
			      filter
			      (string-append result " and " filter))))))
	specs)
    (and (not (string-null? result))
	 result)))


#!

;; missing good example/mini tests

!#
