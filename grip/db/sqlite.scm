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


(define-module (grip db sqlite)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (grip reexport)
  #:use-module (grip db sqlite3)
  #:use-module (grip strings)
  #:use-module (grip lists)

  #:export (sqlite/get
	    sqlite/set
	    sqlite/sqlite-db-file?
	    sqlite/true?
	    sqlite/boolean
	    sqlite/query
	    sqlite/command
	    sqlite/begin-transaction
	    sqlite/commit
	    sqlite/tuple-pos
	    sqlite/table-names
	    sqlite/table-exists?
	    sqlite/table-info
	    sqlite/table-rename
	    sqlite/build-set-expression
	    sqlite/build-group-by-order-by-expression
	    sqlite/add-column))


(define sqlite/get #f)
(define sqlite/set #f)

(eval-when (expand load eval)
  (let ((cache (make-hash-table 13)))
    (set! sqlite/get
	  (lambda (db key)
	    (let ((db-assoc (hashq-get-handle cache db)))
	      (if db-assoc
		  (case key
		    ((all) (cdr db-assoc))
		    (else
		     (assq-ref db-assoc key)))
		  #f))))
    (set! sqlite/set
	(lambda (db key value)
	  (let ((db-assoc (hashq-get-handle cache db)))
	    (if db-assoc
		(hashq-set! cache db (assq-set! (cdr db-assoc) key value))
		(hashq-set! cache db (assq-set! (list) key value)))))))
  (re-export-public-interface (ice-9 format)
			      (ice-9 rdelim)
			      (ice-9 popen)
			      (grip db sqlite3)
			      (grip strings)
			      (grip lists)))


(define (sqlite/sqlite-db-file? filename)
  (let* ((cmd (format #f "file ~A" (str/prep-str-for-fs filename)))
	 (port (open-input-pipe cmd))
	 (str (read-line port)))
    (unless (zero? (status:exit-val (close-pipe port)))
      (error "subprocess returned non-zero result code" cmd))
    (string-contains-ci str "sqlite")))

(define (sqlite/true? value)
  (string=? value "t"))

(define (sqlite/boolean value)
  (if value "t" "f"))

(define (sqlite/query db query)
  (let* ((stmt (sqlite-prepare db query))
	 (results (sqlite-fold cons (list) stmt)))
    (sqlite-finalize stmt)
    (reverse! results)))

(define (sqlite/command db command)
  (let ((stmt (sqlite-prepare db command)))
    (sqlite-step stmt)
    (sqlite-finalize stmt)
    (if #f #f)))

(define (sqlite/begin-transaction db)
  (sqlite/command db "begin transaction"))

(define (sqlite/commit db)
  (sqlite/command db "commit"))

(define (sqlite/tuple-pos item tuples pred idx)
  (list-pos item tuples (lambda (a b) (pred (vector-ref a idx) b))))

(define %sqlite/table-names-str
  "select name from sqlite_master
    where type='table'
 order by name;")

(define* (sqlite/table-names db #:key (refresh #f))
  (let ((cached (sqlite/get db 'table-names)))
    (if (or refresh
	    (not cached))
	(let* ((tuples (sqlite/query db %sqlite/table-names-str))
	       (table-names (map (lambda (tuple) (vector-ref tuple 0)) tuples)))
	  (sqlite/set db 'table-names table-names)
	  table-names)
	cached)))

(define (sqlite/table-exists? db t-name)
  (member t-name (sqlite/table-names db)))

(define %sqlite/table-info-str
  "pragma table_info (\"~A\");")

(define (sqlite/table-info db t-name)
  (and (sqlite/table-exists? db t-name)
       (sqlite/query db (format #f "~?" %sqlite/table-info-str
				(list t-name)))))

(define %sqlite/table-rename-str
  "alter table ~A rename to ~A;")

(define (sqlite/table-rename db from to)
  (sqlite/command db (format #f "~?" %sqlite/table-rename-str
			     (list from to))))

(define (sqlite/build-set-expression items . accessor)
  (let ((result #f)
	(first? #t)
	(acc (if (null? accessor) identity (car accessor))))
    (for-each (lambda (item)
		(if first?
		    (begin
		      (set! result (format #f "(~A" (acc item)))
		      (set! first? #f))
		    (set! result (format #f "~A, ~A" result (acc item)))))
	items)
    (if result
	(format #f "~A)" result)
	#f)))

(define (sqlite/build-group-by-order-by-expression items . accessor)
  (let ((result #f)
	(first? #t)
	(acc (if (null? accessor) identity (car accessor))))
    (for-each (lambda (item)
		(if first?
		    (begin
		      (set! result (format #f "~A" (acc item)))
		      (set! first? #f))
		    (set! result (format #f "~A, ~A" result (acc item)))))
	items)
    (if result
	(format #f "~A" result)
	#f)))

(define %sqlite/add-column-str
  "alter table ~A add column ~A;")

(define (sqlite/add-column db table column-spec)
  ;; ex: imported_id integer default '-1' not null;
  (sqlite/command db (format #f "~?" %sqlite/add-column-str
			     (list table column-spec))))


#!

;; missing good example/mini tests

!#
