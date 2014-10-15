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



(defun erstelle-kopie (orig)
  "Erzeugt eine Kopie des übergebenen Sudoku-Arrays"
  (let ((kopie (make-array '(9 9))))
	(dotimes (i 9 kopie)
	  (dotimes (j 9)
		(setf (aref kopie i j) (aref orig i j))))))



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



(defun erzeuge-rätsel (&optional (tab '()) (anz 25) (versuch '()) (runde 81))
  (labels ((entferne-zahl (orig)
			 (let ((i (random 9))
				   (j (random 9)))
			   (if (not (eq '_ (aref orig i j)))
				   (let ((kopie (erstelle-kopie orig)))
					 (setf (aref kopie i j) '_)
					 (return-from entferne-zahl kopie))
				   (entferne-zahl orig)))))
	(cond ((null tab)
		   (erzeuge-rätsel (erzeuge-sudoku) anz versuch runde))
		  ((null versuch)
		   (let ((versuch (erstelle-kopie tab)))
			 (erzeuge-rätsel tab anz versuch runde)))
		  ((= runde anz)
		   (if (equalp (löse-sudoku (erstelle-kopie versuch)) tab)
			   (return-from erzeuge-rätsel (values versuch tab))
			   nil))
		  (t
		   (let ((temp (entferne-zahl (erstelle-kopie versuch))))
			 (if (equalp (löse-sudoku (erstelle-kopie temp)) tab)
				 (erzeuge-rätsel tab anz temp (1- runde))
				 (erzeuge-rätsel tab anz versuch runde)))))))



(defun gültige-lösung-p (tab &aux (max 9))
  "Prüft ob die übergebene Lösung ein gültiges Sudoku ist"
  (labels ((gültigp (lst)
			 (unless (numberp (first lst))
			   (return-from gültige-lösung-p 'nil)))
		   (zeile (nr)
			 (let ((lst '()))
			   (dotimes (i max lst)
				 (push (aref tab nr i) lst)
				 (gültigp lst))))
		   (spalte (nr)
			 (let ((lst '()))
			   (dotimes (i max lst)
				 (push (aref tab i nr) lst)
				 (gültigp lst))))
		   (box (nr)
			 (let ((lst '())
				   (x-kor (* 3 (rem nr 3)))
				   (y-kor (* 3 (truncate (/ nr 3)))))
			   (dotimes (i 3 lst)
				 (dotimes (j 3)
				   (push (aref tab (+ i x-kor) (+ j y-kor)) lst)
				   (gültigp lst)))))
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



(defun pprint-sudoku (tab &optional (koordinaten 'nil))
  "Gibt ein Sudoku-Array als formatiertes Sudoku aus"
  (flet ((2d-array->list (array)
		   (loop for i below (array-dimension array 0)
			  collect (loop for j below (array-dimension array 1)
						 collect (aref array i j)))))
	(let ((i 0))
	  (dolist (zeile (2d-array->list tab))
		(format t "~{ ~A ~A ~A | ~A ~A ~A | ~A ~A ~A ~}" zeile)
		(incf i)
		(if koordinaten
			(format t " ~A~%" i)
			(format t "~%"))
		(when (and (zerop (rem i 3)) (/= i 9))
		  (format t "-------+-------+-------~%"))
		(when (and koordinaten (= i 9))
		  (format t "~% A B C   D E F   G H I~%~%"))))))



(defun hole-eingabe ()
  "Holt aus einer Eingabe 2 Koordinaten und einen Wert heraus"
  (format t "Deine Eingabe (? für Anleitung): ")
  (let ((eingabe (string-upcase (string-trim " " (read-line)))))
	(format t "Eingabe: ~A~%" eingabe)
	(if (find-if #'digit-char-p eingabe)
		(let* ((y (- (char-int (aref eingabe 0)) 65)) ; 1 mehr, da Zählung bei 0 beginnt
			   (lst (coerce (string-trim " " (subseq eingabe 1)) 'list))
			   (x (- (char-int (first lst)) 49)) ; 1 mehr, da Zählung bei 9 beginnt
			   (wert (- (char-int (first (last lst))) 48)))
		  (format t "x:~A y:~A wert:~A~%" x y wert)
		  (list 'koordinaten x y wert))
		(list (intern eingabe)))))
	


(defun spiele-sudoku ()
  "Ein vollständiges Sudoku-Spiel, inklusive Hinweisen und Zugrücknahme"
  (format t "Erzeuge neues Rätsel, bitte warten!~%")
  (let* ((original (erzeuge-sudoku))
		 (spiel (erzeuge-rätsel (erstelle-kopie original)))
		 (unberührt 't)
		 (züge '()))
	(flet ((zeige-anleitung ()
			 (format t "--- ANLEITUNG ---~%")
			 (format t "Durch Eingabe von ?, MENÜ, ANLEITUNG oder ANWEISUNG bekommst du diese Übersicht angezeigt.~%")
			 (format t "Die Eingabe von >, HINWEIS, TIP oder HILFE deckt genau eine weitere Zahl auf.~%")
			 (format t "Die Eingabe von AUFLÖSUNG, BEENDEN, ENDE, SCHLUß, QUIT oder EXIT beendet das Spiel und zeigt~%dir die vollständige Auflösung an.~%")
			 (format t "Die Eingabe von <, ZURÜCK, UNDO, UPS, ENTSCHULDIGUNG, DAVOR nimmt die letzte Änderung zurück.~%")
			 (format t "Um einen Wert in ein Feld einzutragen, mußt du eine Eingabe in der Form der Koordinaten (erst~%Buchstabe, dann Zahl) gefolgt von einem Leerzeichen und der Zahl die du an dieser Stelle~%wünschst.~%~%"))
		   (mache-zug (ergebnis)
			 (let ((i (second ergebnis))
				   (j (third ergebnis))
				   (wert (fourth ergebnis)))
			   (push (list i j (aref spiel i j)) züge)
			   (setf (aref spiel i j) wert)
			   (when (gültige-lösung-p spiel)
				 (pprint-sudoku spiel t)
				 (format t "Gratuliere!~%Du hast das Sudoku gelöst!~%")
				 (unless unberührt
				   (format t "Allerdings, mit ein klein wenig Hilfe von mir, hm? ;-)~%"))
				 (return-from spiele-sudoku))))
		   (gebe-hinweis ()
			   (setf unberührt 'nil)
			 (do* ((i (random 9) (random 9))
				   (j (random 9) (random 9)))
				  ((eq (aref spiel i j) '_)
				   (push (list i j '_) züge)
				   (setf (aref spiel i j) (aref original i j))))
			 (when (gültige-lösung-p spiel)
			   (pprint-sudoku spiel t)
			   (format t "~%Schade!~%Jetzt habe ich deine Arbeit übernommen.~%")
			   (format t "Beim nächsten Mal, solltest du versuchen, es selber zu lösen!~%")
			   (return-from spiele-sudoku)))
		   (nimm-zug-zurück ()
			   (unless (null züge)
				 (let* ((zug (pop züge))
						(i (first zug))
						(j (second zug))
						(wert (third zug)))
				   (setf (aref spiel i j) wert))))
		   (print-lösbarkeit ()
			 (if (equalp (löse-sudoku (erstelle-kopie spiel)) original)
				 (format t "(lösbar) ")
				 (format t "(unlösbar) "))))
	  (do ()
		  (nil)
		(pprint-sudoku spiel t)
		(print-lösbarkeit)
		(let ((ergebnis (hole-eingabe)))
		  (case (first ergebnis)
			((? menü anleitung anweisung)
			 (zeige-anleitung))
			((koordinaten)
			 (mache-zug ergebnis))
			((> hinweis tip hilfe)
			 (gebe-hinweis))
			((löse auflösung beenden ende schluß quit exit)
			 (return-from spiele-sudoku (pprint-sudoku original t)))
			((< zurück undo ups entschuldigung davor entschuldige)
			 (nimm-zug-zurück))
			(otherwise
			 (format t "~%Gib ? oder ANLEITUNG ein, wenn du nicht weißt, wie man spielt.~%"))))))))
				   
