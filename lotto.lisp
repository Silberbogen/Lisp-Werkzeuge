;-*- coding: utf-8 -*-
;;;; Dateiname: lotto.lisp
;;;; Beschreibung: Eine kleine Lotto-Simulation, um sich mal die Wirklichkeit
;;;;               vor Augen zu halten
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha Biermanns, <skkd.h4k1n9@yahoo.de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2011-2014 Sascha Biermanns
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
;;;; (load "lotto.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "lotto.lisp")



(defun würfelwurf (&optional (seiten 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (1+ (random seiten)))



(defun lottoziehung (&optional (ziehungen 6) (gesamt 49) (zurücklegen nil))
  "(lottoziehung &optional ziehungen gesamt zurücklegen)
LOTTOZIEHUNG gibt eine anzahl an ziehungen Objekten zurück die aus einer Menge gesamt gezogen werden. Ist das optionale Argument zurücklegen NIL, so werden die Objekte entnommen und können nicht wiederverwendet werden.
Beispiel: (lottoziehung 6 49) => (37 44 13 41 4 3)"
  (let ((zahlen nil))
    (do ((i 1 (1+ i)))
		((or (and zurücklegen (> i ziehungen)) ; mit zurücklegen
			 (and (null zurücklegen) (= (length zahlen) ziehungen))) ; ohne zurücklegen
		 zahlen)
      (if zurücklegen
		  (push (würfelwurf gesamt) zahlen) ; mit zürucklegen
		  (setf zahlen (adjoin (würfelwurf gesamt) zahlen)))))) ; ohne zurücklegen



(defun zusatzzahl (gewinnzahlen &optional (gesamt 49))
  "(zusatzzahl gewinnzahlen &optional gesamt)
ZUSATZZAHL errechnet eine mögliche Zusatzzahl aus GESAMT Zahlen, ohne dabei in der Menge der GEWINNZAHLEN enthalten zu sein.
Beispiel: (zusatzzahl '(8 34 27 26 10 37)) => 12"
  (let ((zahlen gewinnzahlen)
		(zzahl nil))
	(setf zahlen (adjoin (würfelwurf gesamt) zahlen))
	(setf zzahl (set-difference zahlen gewinnzahlen))
	(if zzahl
		(car zzahl)
		(zusatzzahl gewinnzahlen gesamt))))



(defun gewinnrang-6-aus-49 (meinezahlen gewinnzahlen zusatzzahl)
  "GEWINNRANG-6-AUS-49 ermittelt den Gewinnrang einer Ziehung anhand der eigenen Zahlen."
  (let ((zahlen (length (intersection meinezahlen gewinnzahlen)))
		(zzahl (member zusatzzahl meinezahlen)))
	(if zzahl
		(cond
		  ((= zahlen 6) 1)
		  ((= zahlen 5) 3)
		  ((= zahlen 4) 5)
		  ((= zahlen 3) 7)
		  ((= zahlen 2) 9)
		  (t 0))
		(cond
		  ((= zahlen 6) 2)
		  ((= zahlen 5) 4)
		  ((= zahlen 4) 6)
		  ((= zahlen 3) 8)
		  (t 0)))))



(defun lotto-6-aus-49 (meinezahlen)
  "LOTTO-6-AUS-49 simuliert eine Ziehung der Lottozahlen und gibt die gezogenen Zahlen, die Superzahl und den eventuellen Gewinnrang aus."
  (let* ((gewinnzahlen (lottoziehung))
		 (zzahl (zusatzzahl gewinnzahlen)))
	(list gewinnzahlen zzahl (gewinnrang-6-aus-49 meinezahlen gewinnzahlen zzahl))))



(defun simuliere-ziehungen-6-aus-49 (meinezahlen &optional (anzahl 1))
  "SIMULIERE-ZIEHUNGEN-6-AUS-49 nimmt simulierte Ziehungen der Lottozahlen vor und ermittelt die Gewinnränge des Spielers. Zum Schluß wird eine Übersicht der Gewinnränge ausgegeben."
  (let ((ränge (list 0 0 0 0 0 0 0 0 0 0)))
	(do ((i 1 (1+ i)))
		((> i anzahl)
		 (rest ränge))
	  (let ((rang (third (lotto-6-aus-49 meinezahlen))))
		(setf (nth rang ränge) (1+ (nth rang ränge)))))))



(defun klartext-gewinnränge-6-aus-49 (ränge)
  "KLARTEXT-GEWINNRÄNGE-6-AUS-49 nimmt die Antwort aus SIMULIERE-ZIEHUNGEN-6-AUS-49 entgegen und gibt einen Klartext des Ergebnisses auf dem Bildschirm aus."
  (format t "1. Rang: 6 Richtige + SZ: ~A~%" (first ränge))
  (format t "2. Rang: 6 Richtige     : ~A~%" (second ränge))
  (format t "3. Rang: 5 Richtige + SZ: ~A~%" (third ränge))
  (format t "4. Rang: 5 Richtige     : ~A~%" (fourth ränge))
  (format t "5. Rang: 4 Richtige + SZ: ~A~%" (fifth ränge))
  (format t "6. Rang: 4 Richtige     : ~A~%" (sixth ränge))
  (format t "7. Rang: 3 Richtige + SZ: ~A~%" (seventh ränge))
  (format t "8. Rang: 3 Richtige     : ~A~%" (eighth ränge))
  (format t "9. Rang: 2 Richtige + SZ: ~A~%" (ninth ränge))
  ränge)



;;; Ein Beispiel für 2080 Ziehungen
;;; (entspricht einer Ziehung je Woche über 40 Jahre)
;;;
;(klartext-gewinnränge-6-aus-49 (simuliere-ziehungen-6-aus-49 '(1 2 3 4 5 6) 2080))
