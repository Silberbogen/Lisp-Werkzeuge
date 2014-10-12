;-*- coding: utf-8 -*-
;;;; Dateiname: magie.lisp
;;;; Beschreibung: Routinen, die mich bei diversen Aufgaben unterstützen
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha Biermanns, <skkd.h4k1n9@yahoo.de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2014 Sascha Biermanns
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;; ------------------------------------------------------------------------
;;;; Hinweis:
;;;; Das Laden dieser Datei in Common Lisp erfolgt per:
;;;; (load "magie.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "magie.lisp")



(defmacro dosequence ((var seq &optional result) &body body
					  &aux (seq-len (length seq)))
  "Iteriert durch eine gegebene Sequenz."
  (with-gensym (i)
	`(do ((,i 0 (1+ ,i)))
		 ((= ,i ,seq-len)
		  ,result)
	   (let ((,var (elt ,seq ,i)))
		 ,@body))))



(defmacro for ((var start stop) &body body)
  "Eine for-Schleife"
  (with-gensym (gstop)
	`(do ((,var ,start (1+ ,var))
		  (,gstop ,stop))
		 ((> ,var ,gstop))
	   ,@body)))



(defmacro forever (&body body)
  "Eine Endlos-Schleife"
  `(do ()
	   (nil)
	 ,@body))



(defmacro in (obj &rest choices)
  "Prüft ob OBJ in CHOICES vorkommt und gibt entsprechend T oder NIL zurück."
  (with-gensym (gobj)
	`(let ((,gobj ,obj))
	   (or ,@(mapcar #'(lambda (c) `(eql ,gobj ,c))
					 choices)))))



(defmacro let1 (var val &body body)
  "Dient zum schnellen Anlegen und Zuweisen einer einzigen Variablen."
  `(let ((,var ,val))
	 ,@body))



(defmacro mac (form)
  "Expandiert ein Macro und gibt es schön formatiert aus."
  `(pprint (macroexpand-1 ',form)))



(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
		  (exp (macroexpand exp1))
		  (*print-circle* nil))
     (cond ((equal exp exp1)
			(format t "~&Macro expansion:")
			(pprint exp))
		   (t (format t "~&First step of expansion:")
			  (pprint exp1)
			  (format t "~%~%Final expansion:")
			  (pprint exp)))
     (format t "~%~%")
     (values)))



(defmacro until (test &body body)
  "Eine until-Kontrollstruktur"
  `(do ()
	   (,test)
	 ,@body))



(defmacro while (test &body body)
  "Eine while-Kontrollstruktur"
  `(do ()
	   ((not ,test))
	 ,@body))



(defmacro with-gensym (syms &body body)
  "Generiert ein gensym je Element aus der Liste SYMS."
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
				 syms)
	 ,@body))
