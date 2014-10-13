;-*- coding: utf-8 -*-
;;;; Dateiname: spiele.lisp
;;;; Beschreibung: Kleine, einfache Spiele
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
;;;; (load "spiele.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "spiele.lisp")



;;; ==========================
;;; Spiele - wir lieben Spiele
;;; ==========================



(defun hole-zahl (string)
  "(hole-zahl string)
HOLE-ZAHL gibt die Zeichenkette String aus und erzwingt die Eingabe einer Zahl."
  (format t "~A" string)
  (let ((zahl (read)))
    (if (not (numberp zahl))
		(hole-zahl string)
		zahl)))



(defun j-oder-n-p (zeichenkette)
  "(j-oder-n-p zeichenkette)
J-ODER-N-P gibt die übergebene Zeichenkette aus und wartet auf die Eingabe von j,J,n oder N, ansonsten ruft sich die Funktion rekursiv erneut auf.
Beispiel: (j-oder-n-p \"Ist jetzt Sommer (j/n)? \")
Ist jetzt Sommer (j/n)? j
T"
  (princ zeichenkette)
  (let ((eingabe (read)))
    (case eingabe
      ((j J ja Ja JA) t)
      ((n N nein Nein NEIN) nil)
      (otherwise (j-oder-n-p zeichenkette)))))



(defun mischen (liste &optional (durchgang (* 2 (length liste))) &aux (länge (length liste)))
  "(mischen liste &optional durchgang)
MISCHEN dient dazu, eine Liste mit einer frei wählbaren Anzahl an Durchgängen zu mischen. Wird keine Anzahl an Durchgängen genannt, so wird der Vorgang 20 Mal durchgeführt.
Beispiel: (mischen '(1 2 3 4 5)) => (5 2 1 4 3)"
  (let ((zufallszahl (random länge)))
	(cond ((zerop durchgang)
		   liste)
	  ((oddp zufallszahl)
	   (mischen (append (reverse (nthcdr zufallszahl liste))
						(butlast liste (- länge zufallszahl)))
				(1- durchgang)))
	  ((evenp zufallszahl)
	   (mischen (append (nthcdr zufallszahl liste)
						(butlast liste (- länge zufallszahl)))
				(1- durchgang)))
	  (t
	   (mischen (append (nthcdr zufallszahl liste)
						(reverse (butlast liste (- länge zufallszahl)))
						(1- durchgang)))))))



(defun nur-buchstaben (text)
  "Entfernt die Nicht-Buchstaben eines Textes."
  (remove-if #'(lambda (string) (not (alpha-char-p string)))
			 text))



(defun würfelwurf (&optional (seiten 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (1+ (random seiten)))



;;; -----------------------------------------------------
;;;                   Das Hauptprogramm
;;; -----------------------------------------------------



(defun haupt-programm ()
  (let* ((spiele-liste (list #'rate-die-zahl
							 #'spiele-rate-meine-zahl
							 #'craps
							 #'addiere-bis-999
							 #'schere-stein-papier
							 #'begriffe-raten))
		 (anzahl (length spiele-liste))
		 (beenden nil))
	(do ()
		(beenden)
	  (format t "~%Dir stehen folgende Möglichkeiten zur Auswahl:~%~%")
	  (do ((i 0 (1+ i)))
		  ((= i anzahl))
		(format t "   ~2D ~A~%" (1+ i) (documentation (elt spiele-liste i) 'function)))
	  (format t "   99 Beenden~%~%")
	  (let ((eingabe (hole-zahl "Deine Wahl? ")))
		(when (numberp eingabe)
		  (when (= eingabe 99)
			(setf beenden t))
		  (when (and (>= eingabe 1) (<= eingabe anzahl))
			(terpri)
			(funcall (elt spiele-liste (1- eingabe)))))))))


	
   
;;; ------------------------------------------------------
;;; Zahlenratespiel
;;; Das Spiel wird aufgerufen mit (spiele-rate-meine-zahl)
;;; ------------------------------------------------------



(defun rate-die-zahl (&optional (minimum 1) (maximum 1000))
  "Versuche die Zahl zu erraten, die der Computer sich ausgedacht hat!"
  (format t "Versuche eine Zahl zwischen ~:d und ~:d zu erraten!~%" minimum maximum)
  (do ((anzahl 0 (1+ anzahl))
	   (zahl (+ (random (1+ (- maximum minimum))) minimum))
	   (versuch))
	  ((and (numberp versuch) (= versuch zahl))
	   (format t "Du hast die richtige Zahl in ~A Versuchen erraten!~%" anzahl))
	(princ "Dein Versuch? ")
	(setf versuch (read))
	(format t "Dein Versuch ist ~[keine Zahl.~;zu klein.~;zu groß.~;richtig!~]~%"
			(cond ((not (numberp versuch)) 0)
				  ((< versuch zahl) 1)
				  ((> versuch zahl) 2)
				  (t 3)))))



(defun spiele-rate-meine-zahl (&optional (minimum 1) (maximum 1000) &aux (beenden nil))
  "Lasse den Computer erraten, welche Zahl du dir ausgedacht hast!"
  (labels ((rate ()
			 (ash (+ minimum maximum) -1)) 
		   (kleiner (zahl)
			 (setf maximum (1- zahl)))
		   (größer (zahl)
			 (setf minimum (1+ zahl)))
		   (fragerunde (zahl)
			 (format t "Ist deine Zahl vielleicht die ~A?~%" zahl) 
			 (let ((eingabe (read)))
			   (case eingabe
				 ((= ja stimmt korrekt gleich) (setf beenden t))
				 ((< kleiner weniger niedriger tiefer drunter) (kleiner zahl))
				 ((> größer mehr höher drüber) (größer zahl))
				 (otherwise (princ "Ich verstehe nicht, was du meinst! ")
							(fragerunde zahl)))))
		   (erraten (zahl)
			 (format t "Deine Zahl ist die ~A!~%" zahl)
			 (setf beenden t)))
	(format t "Ich werde versuchen, eine Zahl zu erraten, die du dir ausgedacht hast und die zwischen ~A und ~A liegen muß.~%" minimum maximum)
	(do ((i 0 (1+ i))
		 (zahl (rate) (rate)))
		(beenden (format t "Somit hätte ich die Zahl im ~A. Versuch erraten!~%" i))
	  (if (= minimum maximum)
		  (erraten zahl)
		  (fragerunde zahl)))))



;;; -----------------------
;;; Craps - ein Würfelspiel
;;; -----------------------



(defun craps ()
  "Spiele eine Partie Craps nach den amerikanischen Casinoregeln!"
  (labels ((werfe-zwei-würfel ()
			 "Gibt eine Liste zurück, in der 2 Würfelwürfe mit 6-seitigen Würfeln enthalten sind."
			 (list (würfelwurf) (würfelwurf)))
		   (schlangenaugen-p (liste)
			 "Überprüft, ob eine übergebene Liste den Wurf zweier Einsen enthält."
			 (when (and (eql (first liste) 1) (eql (second liste) 1)) t))
		   (güterwagen-p (liste)
			 "Überprüft,ob eine übergebene Liste den Wurf zweier Sechsen enthält."
			 (when (and (eql (first liste) 6) (eql (second liste) 6)) t))
		   (sofort-gewinn-p (liste &aux (wurf (apply #'+ liste)))
			 "Der Wurf von 7 oder 11 ein Sofortgewinn."
			 (when (or (eql wurf 7) (eql wurf 11)) t))
		   (sofort-verlust-p (liste &aux (wurf (apply #'+ liste)))
			 "Der Wurf von 2, 3 oder 12 ein Sofortverlust."
			 (when (or (eql wurf 2) (eql wurf 3) (eql wurf 12)) t))
		   (sage-wurf (liste)
			 "Liest das Ergebnis des per Liste übergebenen Wurfs, addiert die Werte und gibt entweder SCHLANGENAUGEN, GÜTERWAGEN oder die Summe der Augenpaare zurück."
			 (cond ((schlangenaugen-p liste) 'schlangenaugen)
				   ((güterwagen-p liste) 'güterwagen)
				   (t (apply #'+ liste))))
		   (versuche-zu-punkten (zahl)
			 "Ermöglicht es zu versuchen, die vorherige Zahl noch einmal zu würfeln und so zu gewinnen."
			 (let* ((wurf (werfe-zwei-würfel))
					(liste ())
					(geworfen (list (first wurf) 'und (second wurf) 'gewürfelt)))
			   (cond ((or (sofort-gewinn-p wurf) (eql zahl (apply #'+ wurf)))
					  (setf liste (list '-- (sage-wurf wurf) '-- 'du 'gewinnst))
					  (append geworfen liste))
					 ((sofort-verlust-p wurf)
					  (setf liste (list '-- (sage-wurf wurf) '-- 'du 'verlierst))
					  (append geworfen liste))
					 (t
					  (setf liste (list '-- (apply #'+ wurf) '-- 'würfle 'nochmal))
					  (format t "~A~%" (append geworfen liste))
					  (versuche-zu-punkten zahl))))))
	(let* ((wurf (werfe-zwei-würfel))
		   (liste ())
		   (geworfen (list (first wurf) 'und (second wurf) 'gewürfelt)))
	  (cond ((sofort-gewinn-p wurf)
			 (setf liste (list '-- (sage-wurf wurf) '-- 'du 'gewinnst))
			 (append geworfen liste))
			((sofort-verlust-p wurf)
			 (setf liste (list '-- (sage-wurf wurf) '-- 'du 'verlierst))
			 (append geworfen liste))
			(t
			 (setf liste (list '-- 'du 'hast (apply #'+ wurf) 'punkte))
			 (format t "~A~%" (append geworfen liste))
			 (versuche-zu-punkten (apply #'+ wurf)))))))
   


;;; --------------
;;; Additionsspiel
;;; --------------

(defun addiere-bis-999 (&optional (zahl 0) (anzahl 0))
  "Addiere Zahlen, bis du auf 999 kommst!"
  (format t "Bitte gib eine Zahl ein: ")
  (let ((eingabe (read)))
    (cond ((not (integerp eingabe))
		   (addiere-bis-999 zahl anzahl))
		  ((= eingabe 999)
		   (format t "Momentan bist du bei ~A und hast bisher ~A Eingaben getätigt.~%" zahl anzahl)
		   (addiere-bis-999 zahl anzahl))
		  (t
		   (incf zahl eingabe)
		   (incf anzahl)
		   (when (= zahl 999)
			 (format t "Du hast die Zahl 999 mit ~A Eingaben erreicht.~%" anzahl)
			 (return-from addiere-bis-999 (values zahl (/ zahl anzahl 1.0))))
		   (addiere-bis-999 zahl anzahl)))))


;;; --------------------------------------
;;; Schere, Stein, Papier (, Echse, Spock)
;;; --------------------------------------


(defun schere-stein-papier ()
  "Spiele Schere-Stein-Papier, wahlweise auch mit Echse und Spock!"
  (labels ((auswahl-spielart ()
			 (format t "Möchtest du:~%1. Die klassische Variante~%2. Die moderne Variante~%3. Die Spielregeln beider Fassungen lesen~%4. Doch nicht spielen~%> ")
			 (let ((auswahl (read)))
			   (cond ((not (numberp auswahl))
					  (auswahl-spielart))
					 ((or (< auswahl 1) (> auswahl 4))
					  (auswahl-spielart))
					 ((= auswahl 3)
					  (format t "~%Die klassische Variante~%Hier gilt folgendes:~%Schere schneidet Papier (und gewinnt)~%Papier bedeckt Stein (und gewinnt)~%Stein schleift Papier (und gewinnt, klar)~%~%Die moderne Variante~%hat ein paar weitere Objekte, Echse und Spock, und daher auch weitere Regeln:~%Stein zerquetscht Echse~%Echse vergiftet Spock~%Spock zertrümmert Schere~%Schere köpft Echse~%Echse frisst Papier~%Papier widerlegt Spock~%Spock verdampft Stein~%~%")
					  (auswahl-spielart))
					 (t auswahl))))
		   (computerwahl (&optional (tbbt nil))
			 (case (if (null tbbt)
					   (würfelwurf 3)
					   (würfelwurf 5))
			   ((1) 'schere)
			   ((2) 'stein)
			   ((3) 'papier)
			   ((4) 'echse)
			   (otherwise 'spock)))
		   (sieger (s1 s2)
			 (cond ((and (equal s1 'schere) (equal s2 'papier)) "Schere schneidet Papier")
				   ((and (equal s1 'papier) (equal s2 'stein)) "Papier bedeckt Stein")
				   ((and (equal s1 'stein) (equal s2 'echse)) "Stein zerquetscht Echse")
				   ((and (equal s1 'echse) (equal s2 'spock)) "Echse vergiftet Spock")
				   ((and (equal s1 'spock) (equal s2 'schere)) "Spock zetrümmert Schere")
				   ((and (equal s1 'schere) (equal s2 'echse)) "Schere köpft Echse")
				   ((and (equal s1 'echse) (equal s2 'papier)) "Echse frisst Papier")
				   ((and (equal s1 'papier) (equal s2 'spock)) "Papier widerlegt Spock")
				   ((and (equal s1 'spock) (equal s2 'stein)) "Spock verdampft Stein")
				   ((and (equal s1 'stein) (equal s2 'schere)) "Stein schleift Schere")
				   (t nil)))
		   (spielerwahl (&optional (tbbt nil))
			 (format t "Du hast zur Auswahl:~%1. Schere~%2. Stein~%3. Papier~%")
			 (unless (null tbbt)
			   (format t "4. Echse~%5. Spock~%~%Bitte triff deine Entscheidung: "))
			 (case (read)
			   ((1 schere) 'schere)
			   ((2 stein) 'stein)
			   ((3 papier) 'papier)
			   ((4 echse) 'echse)
			   ((5 spock) 'spock)
			   (otherwise (spielerwahl tbbt))))
		   (ssp (spieler &optional (tbbt nil))
			 (let* ((computer (computerwahl tbbt))
					(spieler-gewinnt (sieger spieler computer))
					(computer-gewinnt (sieger computer spieler)))
			   (cond (spieler-gewinnt
					  (format nil "~A. Du gewinnst!" spieler-gewinnt))
					 (computer-gewinnt
					  (format nil "~A. Ich gewinne!" computer-gewinnt))
					 (t (format nil "Unentschieden!"))))))
	(let ((tbbt nil)
		  (auswahl nil))
	  (format t "Willkommen zu Schere-Stein-Papier!~%Ich beherrsche beide Spielarten, das gute alte Schere-Stein-Papier, oder die moderne Version aus TBBT. Bei der klassichen Version stehen lediglich Schere, Stein und Papier zur Auswahl, bei der modernen Version kommen noch Echse und Spock hinzu.~%")
	  (setf auswahl (auswahl-spielart))
	  (if (= auswahl 2)
		  (setf tbbt t))
	  (unless (= auswahl 4)
		(loop
		   (format t "~A~%" (ssp (spielerwahl tbbt) tbbt))
		   (unless (j-oder-n-p "Nochmal? [j/n]: ") (return))))
	  (format t "Danke für's mitspielen!~%")
	  'ciao!)))



;;; --------------------
;;;    Begriffe raten
;;; --------------------



(defun begriffe-raten (&optional (dateiname "begriffe.txt"))
  "Errate einen Begriff, den sich der Computer ausgedacht hat!"
  (labels ((versuch ()
			 (let ((eingabe (string-trim " " (read-line))))
			   (if (= 1 (length eingabe))
				   (coerce (string-downcase (elt eingabe 0)) 'character)
				   eingabe)))
		   (ausgabe (bekannt gesucht)
			 (let ((anzahl (length gesucht))
				   (kleingeschrieben (string-downcase gesucht)))
			   (do ((i 0 (1+ i)))
				   ((= i anzahl)
					kleingeschrieben)
				 (unless (subsetp (list (elt kleingeschrieben i)) bekannt)
				   (setf (elt kleingeschrieben i) #\_)))))
		   (noch-ungenutzt (bekannt)
			 (let* ((möglich (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜß" 'list))
					(großgeschrieben (coerce (string-upcase (coerce bekannt 'string)) 'list))
					(anzahl (length möglich)))
			   (do ((i 0 (1+ i)))
				   ((= i anzahl)
					möglich)
				 (when (subsetp (list (elt möglich i)) großgeschrieben)
				   (setf (elt möglich i) #\_)))))	   
		   (spiele (gesucht &optional (bekannt (list #\space #\, #\. #\; #\? #\!))
							(runde 1)
							(fgesucht (string-trim " " gesucht)))
			 (format t "~%*** ~A. Runde ***~%" runde)
			 (format t "~A~%~%" (noch-ungenutzt bekannt))
			 (format t "~%~A~%~%Dein Tip? " (ausgabe bekannt fgesucht))
			 (let ((eingabe (versuch)))
			   (typecase eingabe
				 (character (push eingabe bekannt)
							(when (subsetp (coerce (string-downcase (nur-buchstaben gesucht)) 'list) bekannt)
							  (format t "~%Glückwunsch!~%Du hast es geschafft!~%")
							  (return-from spiele)))
				 (string (when (equalp eingabe fgesucht)
						   (format t "~%Glückwunsch!~%Du hast es geschafft!~%")
						   (return-from spiele))
						 (spiele fgesucht bekannt (1+ runde)))
				 (otherwise (format t "Bitte gib einen einzelnen Buchstaben oder die gesamte Lösung ein!~%")))
			   (spiele fgesucht bekannt (1+ runde))))
		   (erstelle-suchliste (stream-name)
			 "Suchbegriffe zeilenweise einlesen"
			 (let ((liste ()))
			   (with-open-file (stream stream-name)
				 (do ((i (read-line stream nil)
						 (read-line stream nil)))
					 ((null i)
					  liste)
				   (push (string-trim " " i) liste))))))
	(do* ((begriffe (mischen (erstelle-suchliste dateiname)))
		  (beenden nil))
		 ((or (null begriffe) beenden)
		  (format t "Vielen Dank für's Spielen!~%"))
	  (spiele (pop begriffe))
	  (when begriffe
		(setf beenden (not (j-oder-n-p "Willst du ein weiteres Spiel spielen (j/n)? ")))))))


