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
;; as a schemer].

;;; Code:


(define-module (grip fs-ops)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs io ports)
  #:use-module (grip reexport)
  #:use-module (grip utils)
  #:use-module (grip push)
  #:use-module (grip do)
  #:use-module (grip lists)
  #:use-module (grip strings)
  #:use-module (grip support slib)

  #:export (split-filename
	    get-dirnames
	    get-filenames
	    pair-files
	    mk-dir
	    rm-dir
	    cp-files
	    rm-files
	    cat-file
	    is-more-recent?
	    read-file-string-all))


(define* (split-filename filename #:key (dot-lindex #f))
  (let* ((d-name (dirname filename))
	 (b-name (basename filename))
	 (dot (if dot-lindex
		  (string-index b-name #\.)
		  (string-rindex b-name #\.)))
	 (f-name (if dot (substring b-name 0 dot) b-name))
	 (f-ext (if dot (substring b-name (1+ dot)) #f)))
    (values d-name b-name f-name f-ext)))

(define* (get-dirnames in #:key (recursively #f))
  (let ((dirs (list)))
    (nftw in
	  (lambda (filename statinfo flag base level)
	    (case flag
	      ((directory)
	       (when (or recursively (= level 1))
		 (push! filename dirs))))
	    #t) ;; do not stop
	  'physical)
    (reverse! dirs)))

(define* (get-filenames in #:key (like #f) (ci #f) (recursively #f) (dir-assoc #f))
  ;; (dimfi in)
  (let ((d-assoc (list))
	(files (list))
	(dir-nb 0)
	(file-nb 0)
	(matcher (and like
		      (if ci
			  (glob:make-matcher like char-ci=? char-ci<=?)
			  (glob:make-matcher like char=? char<=? )))))
    (nftw in
	  (lambda (filename statinfo flag base level)
	    (case flag
	      (#;(regular symlink)
	       (regular)
	       (if (or recursively (= level 1))
		   (receive (d-name b-name f-name f-ext)
		       (split-filename filename)
		     (when (or (not matcher)
			       (matcher b-name))
		       (set! file-nb (1+ file-nb))
		       (if dir-assoc
			   (let ((values (assoc-ref d-assoc d-name)))
			     (if values
				 (set! d-assoc (assoc-set! d-assoc d-name (cons b-name values)))
				 (begin
				   (set! dir-nb (1+ dir-nb))
				   (set! d-assoc (assoc-set! d-assoc d-name (list b-name))))))
			   (push! filename files)))))))
	    #t) ;; do not stop
	  'physical)
    (if dir-assoc
	(values d-assoc dir-nb file-nb)
	(values (reverse! files) dir-nb file-nb))))

(define (pair-files files a b)
  (if (< (length files) 2)
      files
      (let* ((a-files (sort (filter-map (lambda (file)
					  (and (string-contains file a) file))
				files)
			    string<?))
	     (no-a-files (filter-map (lambda (file) (str/delete file a)) a-files))
	     (b-files (sort (filter-map (lambda (file)
					  (and (string-contains file b) file))
				files)
			    string<?))
	     (no-b-files (filter-map (lambda (file) (str/delete file b)) b-files))
	     (paired-files (list)))
	(dotimes (i (length no-a-files) paired-files)
	  (let ((pos (list-pos (list-ref no-a-files i) no-b-files string=?)))
	    (when pos
	      (push! (cons (list-ref a-files i)
			   (list-ref b-files pos))
		     paired-files)))))))

(define (mk-dir dirname)
  ;; the builtin mkdir guile core function calls the operating system one,
  ;; not the GNU Linux shell script, which does not implement the '-p'
  ;; option, which is essential for us.  This version always apply -p.
  (let ((cmd (string-append "mkdir -p " dirname)))
    (or (system cmd)
	(error "subprocess returned non-zero result code" cmd))))

(define (rm-dir dirname)
  (let ((cmd (string-append "rm -rf " dirname)))
    (or (system cmd)
	(error "subprocess returned non-zero result code" cmd))))

(define (cp-files files from to)
  (for-each (lambda (file)
	      (copy-file (string-append from "/" file) (string-append to "/" file)))
      files))

(define* (rm-files files #:key (in #f))
  (for-each (lambda (file)
	      (cond ((and in
			  (char=? (string-ref file 0) #\/))
		     (error "rm-files: expecting a basename: " file))
		    (in
		     (delete-file (if (char=? (string-ref in
							  (- (string-length in) 1))
					      #\/)
				      (string-append in file)
				      (string-append in "/" file))))
		    (else
		     (delete-file file))))
      files))

(define* (cat-file a b #:key (mode ">>"))
  (let ((cmd (format #f "cat ~A ~A ~A" a mode b)))
    (or (system cmd)
	(error "subprocess returned non-zero result code" cmd))))

(define (is-more-recent? filename1 filename2)
  (let ((stat1 (stat filename1))
	(stat2 (stat filename2)))
    (or (> (stat:mtime stat1) (stat:mtime stat2))
        (and (= (stat:mtime stat1) (stat:mtime stat2))
             (>= (stat:mtimensec stat1)
                 (stat:mtimensec stat2))))))

(define (read-file-string-all filename)
  (call-with-input-file filename read-string))
