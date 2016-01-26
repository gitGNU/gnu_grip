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

;; re-export-public-interface, copied from guile-gnome:
;;   git://git.sv.gnu.org/guile-gnome.git
;;     glib/gnome/gw/support/modules.scm

;;; Code:


(define-module (grip reexport)
  #:export (re-export-public-interface))


(define-macro (re-export-public-interface . args)
  "Re-export the public interface of a module or modules. Invoked as
@code{(re-export-public-interface (mod1) (mod2)...)}."
  (if (null? args)
      '(if #f #f)
      `(begin
	 ,@(map (lambda (mod)
		  (or (list? mod)
		      (error "Invalid module specification" mod))
		  `(module-use! (module-public-interface (current-module))
				(resolve-interface ',mod)))
		args))))
