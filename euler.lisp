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

(defparameter *primenumbers* nil)

(defun fibonacci-folge (n &optional (a 0) (b 1))
	   "Bildet die Fibonacci-Folge zur n. Zahl; Beispiel: (fibonacci-reihe 20) => 6765"
  (if (= n 0)
      a
	  (fibonacci-folge (- n 1) b (+ a b))))

(defun palindrom-p (sequenz) "(palindrom-p sequenz)
Palindromp testet, ob eine übergebene Sequenz, eine übergebene Zeichenkette oder ein übergebenes Symbol ein Palindrom darstellt.
Beispiele: (palindrom-p '(1 2 3 4 3 2 1)) => T
 (palindrom-p 'otto) => T
 (palindrom-p 'otta) => NIL
 (palindrom-p \"Otto\") => T"
       (typecase sequenz
		 (null nil)
		 (number (string= (write-to-string sequenz) (reverse (write-to-string sequenz))))
		 (string (string= sequenz (reverse sequenz)))
		 (symbol (string= (symbol-name sequenz) (reverse (symbol-name sequenz))))
		 (list (equal sequenz (reverse sequenz)))
		 (otherwise nil)))

(defun primzahl-p (x)
  "Prüft ob eine Zahl eine echte Primzahl ist."
  (labels ((versuche (&optional (n 2))
			 (if (> (expt n 2) x)
				 t
				 (if (zerop (rem x n))
					 nil
					 (versuche (1+ n))))))
    (and (/= 1 x)
		 (versuche))))

(defun erzeuge-primzahl (x)
  "Erzeugt die x. Primzahl."
  (labels ((durchgang (&optional (test 2) (zähler 0))
			 (if (=  zähler x)
				 (1- test)
				 (if (primzahl-p test)
					 (durchgang (1+ test) (1+ zähler))
					 (durchgang (1+ test) zähler)))))
    (durchgang)))

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

(defun euler4 ()
  "Find the largest palindrome made from the produkt of two 3-digit numbers."
  (let ((x 0))
	(loop
	   for i downfrom 999 to 100
	   do (loop
		  for j downfrom i to 100
		  when (and (palindrom-p (* i j)) (> (* i j) x))
		  do (setf x (* i j))))
	   x))

(defun euler5 ()
  "What is the smallest number divisible by each of the numbers 1 to 20?"
  (lcm 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(defun euler6 ()
  "What is the difference between the sum of the squares and the square of the sums?"
  (let ((summe 0)
		(quadratsumme 0))
	(loop for i from 1 to 100
		 do (progn
			  (setf summe (+ summe i)
					quadratsumme (+ quadratsumme (expt i 2)))))
	(- (expt summe 2) quadratsumme)))

(defun euler7 ()
  "Find the 10001st prime."
  (erzeuge-primzahl 10001))