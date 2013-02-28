;-*- coding: utf-8 -*-
;;;; Dateiname: euler.lisp
;;;; Beschreibung: Diese Datei enthält Lösungen für Euler Aufgaben
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha Biermanns, <skkd.h4k1n9@yahoo.de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2013 Sascha Biermanns
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
;;;; (load "euler.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "euler.lisp")

(defun fibonacci-folge (n &optional (a 0) (b 1))
	   "Bildet die Fibonacci-Folge zur n. Zahl; Beispiel: (fibonacci-reihe 20) => 6765"
  (if (= n 0)
      a
	  (fibonacci-folge (- n 1) b (+ a b))))

(defun euler1 ()
  "Add all the natural numbers below one thousand that are multiples of 3 or 5."
  (loop for i below 1000
	   if (or (zerop (mod i 3)) (zerop (mod i 5)))
	   sum i))

(defun euler2 (&optional (summe 0) (i 1))
  "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
  (let ((fibonacci-zahl (fibonacci-folge i)))
	(cond
	  ((>= fibonacci-zahl 4000000) ; Ausstiegsklausel 
	   summe)
	  ((oddp fibonacci-zahl) ; nächste Iteration bei ungerader Fibonacci-Zahl
	   (euler2 summe (1+ i)))
	  (t ; Additionsklausel bei gerader Fibonacci-Zahl
	   (euler2 (+ summe fibonacci-zahl) (1+ i))))))

(defun euler3 (&optional (zahl 600851475143) (i 2))
  "Find the largest prime factor of a composite number."
  (cond
	((>= i zahl) ; Ausstiegssklausel
	 zahl)
	((zerop (mod zahl i)) ; Teiler ist Primzahl
	 (euler3 (/ zahl i) (1+ i)))
	(t ; Teiler ist keine Primzahl
	 (euler3 zahl (1+ i)))))
	   