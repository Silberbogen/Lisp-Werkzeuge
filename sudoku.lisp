;-*- coding: utf-8 -*-
;;;; Dateiname: sudoku.lisp
;;;; Beschreibung: Routinen, um ein Sudoku, ein Sudok-Rätsel eine Sudoku-
;;;;               Lösung zu erstellen
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
;;;; (load "sudoku.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "sudoku.lisp")



(defun möglichkeiten (zeile spalte tab)
  "Gibt eine Liste aller Möglichkeiten einer Position zurück"
  (flet ((zeile-nachbarn (zeile spalte &aux (nachbarn '()))
		   (dotimes (i 9 nachbarn)
			 (let ((x (aref tab zeile i)))
			   (unless (or (eq '_ x) (= i spalte))
				 (push x nachbarn)))))
		 (spalte-nachbarn (zeile spalte &aux (nachbarn '()))
		   (dotimes (i 9 nachbarn)
			 (let ((x (aref tab i spalte)))
			   (unless (or (eq x '_) (= i zeile))
				 (push x nachbarn)))))
		 (box-nachbarn (zeile spalte &aux (nachbarn '()))
		   (let* ((zeile-min (* 3 (floor zeile 3)))    (zeile-max (+ zeile-min 3))
				  (spalte-min (* 3 (floor spalte 3))) (spalte-max (+ spalte-min 3)))
			 (do ((r zeile-min (1+ r))) ((= r zeile-max) nachbarn)
			   (do ((c spalte-min (1+ c))) ((= c spalte-max))
				 (let ((x (aref tab r c)))
				   (unless (or (eq x '_) (= r zeile) (= c spalte))
					 (push x nachbarn))))))))
	(nset-difference
	 (list 1 2 3 4 5 6 7 8 9)
	 (nconc (zeile-nachbarn zeile spalte)
			(spalte-nachbarn zeile spalte)
			(box-nachbarn zeile spalte)))))



(defun erzeuge-sudoku (&aux (max 9))
  "Erzeugt ein gültiges Sudoku"
  (flet ((suche-sudoku ()
		   (let ((tab (make-array '(9 9) :initial-element '_)))
			 (flet ((setze-feld (i j)
					  (let ((lst (möglichkeiten i j tab)))
						(when (null lst)
						  (return-from suche-sudoku 'nil))
						(setf (aref tab i j) (elt lst (random (length lst)))))))
			   (dotimes (i max tab)
				 (do ((j i (1+ j)))
					 ((= j max))
				   (setze-feld i j)
				   (when (eql '_ (aref tab j i))
					 (setze-feld j i))))))))
	(let ((tab (suche-sudoku)))
	  (when tab
		(return-from erzeuge-sudoku tab))
	  (erzeuge-sudoku))))



(defun erzeuge-rätsel (&optional (tab (erzeuge-sudoku)) (anz 25) &aux (max 9) (felder 81))
  "Erstelle aus einem gültigen Sudoku ein Rätsel zum Ausfüllen"
  (let ((rätsel (make-array '(9 9) :initial-element '_)))
	(dotimes (i max rätsel)
	  (dotimes (j max)
		(when (< (random felder) anz)
		  (setf (aref rätsel i j) (aref tab i j)))))))



(defun gültige-lösung-p (tab &aux (max 9))
  "Prüft ob die übergebene Lösung ein gültiges Sudoku ist"
  (flet ((zeile (nr)
		   (let ((lst '()))
			 (dotimes (i max lst)
			   (push (aref tab nr i) lst))))
		 (spalte (nr)
		   (let ((lst '()))
			 (dotimes (i max lst)
			   (push (aref tab i nr) lst))))
		 (box (nr)
		   (let ((lst '())
				 (x-kor (* 3 (rem nr 3)))
				 (y-kor (* 3 (truncate (/ nr 3)))))
			 (dotimes (i 3 lst)
			   (dotimes (j 3)
				 (push (aref tab (+ i x-kor) (+ j y-kor)) lst)))))
		 (gültige-sequenz-p (seq)
		   (let* ((sortiert (sort seq #'<))
				  (gültig (apply #'< sortiert))
				  (länge (length sortiert)))
			 (and gültig (= max länge)))))
	(dotimes (i max t)
	  (unless (and (gültige-sequenz-p (zeile i))
				   (gültige-sequenz-p (spalte i))
				   (gültige-sequenz-p (box i)))
		(return-from gültige-lösung-p 'nil)))))



(defun löse-sudoku (tab &optional (zeile 0) (spalte 0) &aux (max 9))
  "Löst ein Sudoku"
  (cond
   ((= zeile max)
    tab)
   ((= spalte max)
    (löse-sudoku tab (1+ zeile) 0))
   ((not (eq '_ (aref tab zeile spalte)))
    (löse-sudoku tab zeile (1+ spalte)))
   (t (dolist (auswahl (möglichkeiten zeile spalte tab) (setf (aref tab zeile spalte) '_))
        (setf (aref tab zeile spalte) auswahl)
        (when (eq tab (löse-sudoku tab zeile (1+ spalte)))
          (return tab))))))





(defun pprint-sudoku (tab)
  "Gibt ein Sudoku-Array als formatiertes Sudoku aus"
  (flet ((2d-array->list (array)
		   (loop for i below (array-dimension array 0)
			  collect (loop for j below (array-dimension array 1)
						 collect (aref array i j)))))
	(let ((i 0))
	  (dolist (zeile (2d-array->list tab))
		(format t "~{ ~A ~A ~A | ~A ~A ~A | ~A ~A ~A ~}~%" zeile)
		(incf i)
		(when (and (zerop (rem i 3)) (/= i 9))
		  (format t "-------+-------+-------~%"))))))
