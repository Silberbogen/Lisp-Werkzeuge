;-*- coding: utf-8 -*-
;;;; Dateiname: alien-zukunft.lisp
;;;; Beschreibung: Eine interaktive Abenteuergeschichte
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
;;;; (load "alien-zukunft.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "alien-zukunft.lisp")


(defparameter *ereignis* (make-hash-table)
  "Hält fest, ob gewisse Ereignisse eingetroffen sind")


(defparameter *inventar* (make-hash-table)
  "Das Inventar des Spielers")


(defparameter *person* (make-hash-table)
  "Verwaltet Aufenthaltsorte anderer Personen")


(defparameter *richtung* 0
  "Hält fest, in welcher Richtung die Plattform rotiert ist")


(defparameter *spieler* (make-hash-table)
  "Die Charakterwerte des Spielers")


(defparameter *zug* 'nil
  "Speichert den Verlauf des Weges, den der Spieler nimmt")


;;; ----------------------------
;;;  Allgemeine Hilfsfunktionen
;;; ----------------------------


(defun auswahl (orte ctrl &rest args &aux (anzahl (length orte)))
  "Ermittelt durch Abfrage den nächsten Ort, den der Spieler aufsuchen möchte"
  (let ((auswahl (apply #'zahlen-auswahl anzahl ctrl args)))
	(elt orte (1- auswahl))))


(defun eingabe (&optional ctrl &rest args)
  "Erzwingt eine Eingabe."
  (do ((danach nil t)
	   (ctrl (concatenate 'string "~&" ctrl " > ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe deine Antwort ein und drücke dann die Eingabe-Taste.~%"))
	(apply #'format *query-io* ctrl args)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (unless (string-equal antw "")
		(return-from eingabe antw)))))


(defun j-oder-n-p (&optional ctrl &rest args)
  "Erzwingt die Beantwortung einer Eingabe mit j oder n."
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " (j oder n) ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe \"j\" für Ja oder  \"n\" für Nein.~%"))
	(apply #'format *query-io* ctrl args)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (cond ((string-equal antw "j")
			 (return-from j-oder-n-p 't))
			((string-equal antw "n")
			 (return-from j-oder-n-p 'nil))))))


(defun ja-oder-nein-p (&optional ctrl &rest args)
  "Erzwingt die Beantwortung einer Eingabe mit Ja oder Nein."
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " (Ja oder Nein) ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe \"ja\" für Ja oder  \"nein\" für Nein.~%"))
	(apply #'format *query-io* ctrl args)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (cond ((string-equal antw "ja")
			 (return-from ja-oder-nein-p 't))
			((string-equal antw "nein")
			 (return-from ja-oder-nein-p 'nil))))))


(defun löse-rätsel (rätsel antworten ctrl &rest args)
  "Dient zum Lösen eines Rätsels."
  (let ((ctrl (concatenate 'string ctrl " > ")))
	(apply #'format *query-io* ctrl args)
	(let ((antw (read-from-string (read-line *query-io*))))
	  (if (member antw antworten)
		  (progn
			(textausgabe "Deine Antwort war ... weise!")
			(ereignis rätsel 1)
			't)
		  (progn
			(textausgabe "Deine Antwort war ... unwissend!")
			'nil)))))


(defun nur-ziffern (text)
  "Entfernt die Nicht-Ziffern eines Textes."
  (remove-if #'(lambda (string) (not (digit-char-p string))) text))


(defun textausgabe (ctrl &rest args)
  "Eine vereinfachte Ausgabe, die die Ausgabe stets am Anfang der Zeile beginnt und nach der Ausgabe die Zeile abschließt."
  (let ((ctrl (concatenate 'string "~&" ctrl "~%")))
	(apply #'format t ctrl args)))


(defun text-auswahl (lst ctrl &rest args &aux mehrfach)
  "Erzwingt die Auswahl aus einer Liste."
  (when (member 'alles lst)
	(setf mehrfach 't
		  lst (set-difference lst '(alles))))
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " > ")))
	  (nil)
	(when danach
	  (if mehrfach
		  (format *query-io* "~&Bitte wähle, was du benötigst, aus der Liste aus. Gib \"nichts\" ein, wenn du nichts möchtest oder \"alles\" wenn du gerne alles hättest.~%")
		  (format *query-io* "~&Bitte wähle, was du benötigst, aus der Liste aus. Gib \"nichts\" ein, wenn du nichts möchtest.~%")))
	(apply #'format *query-io* ctrl args)
	(let* ((antw (string-trim " " (read-line *query-io*)))
		   (antw-lst (string-aufteilen antw))
		   antwort)
	  (dolist (i antw-lst)
		(push (read-from-string i) antwort))
	  (unless (null antwort)
		(cond ((not mehrfach)
			   (if (null (rest antwort))
				   (return-from text-auswahl (first antwort))
				   (format *query-io* "~&Du darfst nur eines auswählen!~%")))
			  ((subsetp antwort lst)
			   (return-from text-auswahl antwort))
			  ((eql (first antwort) 'alles)
			   (return-from text-auswahl lst))
			  ((eql (first antwort) 'nichts)
			   (return-from text-auswahl 'nil))
			  (t (format *query-io* "~&Etwas aus deiner Eingabe ist mir unbekannt!~%")))))))


(defun string-aufteilen (str &key (trennzeichenp #'(lambda (x)
													 (position x " ,.;?!/\\"))))
  "Wandelt einen String in eine Liste von Worten um."
  (loop :for beg = (position-if-not trennzeichenp str)
	 :then (position-if-not trennzeichenp str :start (1+ end))
	 :for end = (and beg (position-if trennzeichenp str :start beg))
	 :when beg :collect (subseq str beg end)
	 :while end))


(defun w4 (&optional (anzahl 1))
  "Simuliert Würfe mit einem vierseitigen Würfel"
  (wurfserie anzahl 4))


(defun w6 (&optional (anzahl 1))
  "Simuliert Würfe mit einem sechsseitigen Würfel"
  (wurfserie anzahl 6))


(defun w8 (&optional (anzahl 1))
  "Simuliert Würfe mit einem achtseitigen Würfel"
  (wurfserie anzahl 8))


(defun w10 (&optional (anzahl 1))
  "Simuliert Würfe mit einem zehnseitigen Würfel"
  (wurfserie anzahl 10))


(defun w12 (&optional (anzahl 1))
  "Simuliert Würfe mit einem zwölfseitigen Würfel"
  (wurfserie anzahl 12))


(defun w20 (&optional (anzahl 1))
  "Simuliert Würfe mit einem zwanzigseitigen Würfel"
  (wurfserie anzahl 20))


(defun w100 (&optional (anzahl 1))
  "Simuliert Würfe mit einem 100seitigen Würfel"
  (wurfserie anzahl 100))


(defun wurfserie (&optional (anzahl 1) (seiten 6))
  "Simuliert mehrere Würfe mit einer Anzahl Würfeln"
  (when (plusp anzahl)
	(do ((i 0 (1+ i))
		 (summe 0))
		((= i anzahl)
		 summe)
	  (incf summe (würfelwurf seiten)))))


(defun würfelwurf (&optional (seiten 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (1+ (random seiten)))


(defun zahlen-auswahl (max ctrl &rest args)
  "Erzwingt die Eingabe einer Zahl."
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " > ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte gib eine Zahl zwischen 1 und ~A ein.~%" max))
	(apply #'format *query-io* ctrl args)
	(let* ((antw (string-trim " " (read-line *query-io*))))
	  (unless (string-equal "" (nur-ziffern antw))
		(let ((antwort (parse-integer (nur-ziffern antw))))
		  (if (and (> antwort 0) (<= antwort max))
			  (return-from zahlen-auswahl antwort)))))))


;;; ------------------------------------
;;;  besondere Funktionen für das Spiel
;;; ------------------------------------


(defun ereignis (key &optional wert)
  (if (null wert)
	  (gethash key *ereignis* nil)
	  (setf (gethash key *ereignis* nil) wert)))


(defun erhöhe (key wert)
  (case key
	(gewandheit
	 (let ((max (spieler 'max-gewandheit)))
	   (if (<= (+ (spieler 'gewandheit) 4) max)
		   (spieler 'gewandheit wert)
		   (spieler 'gewandheit max))))
	(glück
	 (let ((max (spieler 'max-glück)))
	   (if (<= (+ (spieler 'glück) 4) max)
		   (spieler 'glück wert)
		   (spieler 'glück max))))
	(stärke
	 (let ((max (spieler 'max-stärke)))
	   (if (<= (+ (spieler 'stärke) 4) max)
		   (spieler 'stärke wert)
		   (spieler 'stärke max))))
	(max-gewandheit
	 (spieler 'max-gewandheit wert))
	(max-glück
	 (spieler 'max-glück wert))
	(max-stärke
	 (spieler 'max-stärke wert))))


(defun inventar (key &optional (wert 0))
  (typecase wert
	(number
	 (if (zerop wert)
		 (gethash key *inventar* 0)
		 (let ((aktuell (gethash key *inventar* 0)))
		   (cond ((zerop aktuell)
				  (values nil 0))
				 ((< (+ aktuell wert) 0)
				  (values nil '<0))
				 ((zerop (+ aktuell wert))
				  (remhash key *inventar*)
				  (values t 0))
				 (t
				  (values t (incf (gethash key *inventar* 0) wert)))))))
	(otherwise
	 (setf (gethash key *inventar* nil) wert))))


(defun kampf (gegner &optional flucht treffer-verboten)
  (flet ((kampfrunde (gegner)
		   (let ((g1 (+ (spieler 'gewandheit) (w6 2) (spieler 'angriffsbonus)))
				 (g2 (+ (third gegner) (w6 2))))
			 (when (ereignis 'unsichtbar)
			   (incf g1 2))
			 (when (= g1 g2)
			   (return-from kampfrunde 't)) ; Unentschieden, nicht getroffen
			 (textausgabe "~A: Stärke ~A, Gewandheit ~A, Glück ~A" (spieler 'name)
						  (spieler 'stärke) (spieler 'gewandheit) (spieler 'glück))
			 (textausgabe "~A: Stärke ~A, Gewandheit ~A" (first gegner)
						  (second gegner) (third gegner))
			 (when flucht
			   (if (j-oder-n-p "Möchtest du die Möglichkeit nutzen und fliehen?")
				   (return-from kampf flucht)))
			 (let* ((glückstest (and (plusp (spieler 'glück))
									 (j-oder-n-p "Möchtest du dein Glück versuchen?")))
					(wirklichglück (and glückstest (versuche-dein-glück))))
			   (when (> g1 g2) ; Der Spieler geht als Sieger hervor
				 (if (and (ereignis 'nur-silber-trifft) (not (inventar 'silberwaffe)))
					 (return-from kampfrunde 't)) ; Das Monster ist imun - dumm
				 (if (ereignis 'unsichtbar)
					 (decf (second gegner) 2)) ; Unsichtbare treffen besser ;)
				 (when (and (or (plusp (inventar 'gewehr)) (plusp (inventar 'pistole)))
							(plusp (inventar 'patronen)))
				   (decf (second gegner))
				   (inventar 'patronen -1))
				 (when glückstest
				   (if wirklichglück
					   (decf (second gegner) 4)
					   (decf (second gegner)))
				   (return-from kampfrunde 't))
				 (decf (second gegner) 2)
				 (return-from kampfrunde 't))
			   (when (< g1 g2)
				 (let ((schildbonus (if (and (plusp (inventar 'schild)) (= (w6) 6)) 1 0)))
				   (when (ereignis 'unsichtbar)
					 (let ((wurf (w6)))
					   (if (or (= wurf 2) (= wurf 4))
						   (if (< (spieler 'stärke) (spieler 'max-stärke))
							   (spieler 'stärke 1))
						   (if (= wurf 6)
							   (return-from kampfrunde 't)))))
				   (when glückstest
					 (if wirklichglück
						 (spieler 'stärke (+ -1 schildbonus))
						 (spieler 'stärke (+ -4 schildbonus)))
					 (return-from kampfrunde 'nil))
				   (spieler 'stärke (+ -2 schildbonus))
				   (return-from kampfrunde 'nil)))))
		   't)
		 (gesamt-stärke ()
		   (let ((accu 0))
			 (dolist (i gegner accu)
			   (incf accu (second i))))))
	(do ((kein-treffer-erhalten 't))
		((or (<= (spieler 'stärke) 0) (<= (gesamt-stärke) 0)))
	  (dolist (i gegner)
		(if (plusp (spieler 'stärke))
			(if (plusp (second i))
				(progn
				  (setf kein-treffer-erhalten (kampfrunde i))
				  (if (and treffer-verboten (not kein-treffer-erhalten))
					  (return-from kampf 'nil))))
			(return-from kampf 'nil))))
	(when (plusp (spieler 'stärke))
	  't)))


(defun mahlzeit ()
  (when (j-oder-n-p "Möchtest du eine Mahlzeit zu dir nehmen")
	(if (plusp (inventar 'proviant))
		(progn
		  (textausgabe "Nachdem du dich versichert hast, das niemand in der Nähe ist, entnimmst du ein Proviantpaket aus deinem Rucksack. Genüsslich und so leise wie möglich verzehrst du es. Du kannst spüren, wie etwas Kraft in deinen müden Körper zurückkehrt.")
		  (inventar 'proviant -1)
		  (erhöhe 'stärke 4))
		(textausgabe "Nachdem du dich versichert hast, das niemand in der Nähe ist, durchwühlst du deinen Rucksack auf der Suche nach einem Proviantpaket. Nach einigen Minuten und mehrfachem aus- und einpacken des Rucksacks gibst du verzweifelt auf. Es ist tatsächlich kein einziger Brotkrummen mehr übrig."))))


(defun person (key &optional wert)
  (if (null wert)
	  (gethash key *person* nil)
	  (setf (gethash key *person* nil) wert)))


(defun rotiere-plattform ()
  (if (< 7 *richtung*)
	  (incf *richtung*)
	  (setf *richtung* 0)))


(defun spieler (key &optional (wert 0))
  (typecase wert
	(number
	 (if (zerop wert)
		 (gethash key *spieler* 0)
		 (incf (gethash key *spieler* 0) wert)))
	(otherwise
	 (setf (gethash key *spieler* nil) wert))))


(defun versuche-dein-glück ()
  (let ((glück (spieler 'glück)))
	(when (plusp glück)
	  (spieler 'glück -1)
	  (if (<= (w6 2) glück)
			't))))


// Funktion: Zweisamkeit
(defun zweisamkeit (wert)
  (person 'arianna (1+ (person 'arianna)))
  (case (person 'arianna)
	(20
	 (textausgabe "Euch beiden gefällt es, zusammen etwas zu unternehmen, tatsächlich könnte man sagen, das ihr echte Freunde geworden seid."))
	(40
	 (textausgabe "In der letzten Zeit wurde es euch immer bewußter, daß die gemeinsame Zeit für euch beide die beste Zeit des Tages ist. Nach einem ausführlichen Gespräch seid ihr zu dem Ergebnis gekommen, das es nicht verkehrt werde, den Großteil der Zeit ab sofort gemeinsam zu verbringen."))
	(60
	 (textausgabe "Arianna sieht dich verstohlen an. Sie gibt dir verschmitzt einen Kuss auf den Mund und läßt einen Schlüssel in deine Hand gleiten. \"Mein Heim soll ab jetzt auch dein Heim sein!\" sagt sie dabei und errötet kräftig unter dem Flaum ihres Backenbarts.")
	 (inventar 'schlüssel-arianna 1))
	(80
	 (textausgabe "Arianna druckst etwas herum, schließlich aber bringt sie auf den Punkt, was sie sagen möchte: \"Was hältst du davon, wenn wir den Ewigen Bund der Schmiede eingehen?\" Sie sieht jetzt irgendwie schüchtern aus.")
	 (if (j-oder-n-p "Willst du dich mit Arianna verloben?")
		 (progn
		   (textausgabe "Du mußt nicht einen Augenblick lang nachdenken, sondern grinst frech zurück: \"Na klar!\" und erhältst von ihr einen Knuff der dich zu Boden schickt, dann liegt sie auch schon auf dir und bedeckt dein Gesicht mit Küssen. Und dann wird sie plötzlich wieder ernst: \"Dann wirst du dich mit meinen Eltern treffen müssen - sie müssen dem Bund zustimmen!\"\nIrgendwie war es klar, das nichts im Leben besonders einfach isti")
		   (inventar 'verloben 1))
		 (progn
		   (textausgabe "Ganz liebevoll sagst du ihr, daß du das zum jetzigen Zeitpunkt nicht für eine gute Idee hältst - und erhältst dafür ein Schmollgesicht von ihr, wie du es noch nie zuvor gesehen hast.")
		   (person 'arianna 61))))
	(99
	 (if (< (inventar 'verloben) 6)
		 (person 'arianna 98)
		 (textausgabe "Jetzt ist alles perfekt! Ihr beide solltet so bald als möglich in die große Schmiede gehen!")))
	(100
	 (textausgabe "Jetzt ist der große Tag da, an dem du mit Arianna den Bund eingehen wirst!"))))


;;; -----------
;;;  Das Spiel
;;; -----------


(defun hauptprogramm ()
  (intro)
  (vorwort)
  (push #'ort-1 *zug*)
  (do ((ende))
	  (ende
	   (textausgabe "Ich hoffe sehr, es hat dir Spaß gemacht!"))
	(push (funcall (first *zug*)) *zug*)
	(when (eql (first *zug*) 'ende)
	  (setf ende 't))))
  

(defun intro ()
    (textausgabe "Hinweis!")
	(textausgabe "In diesem Roman wird niemand anderes als du selbst die Person sein, die das Abenteuer durchlebt. Von daher würde ich sagen, würdest du dir selber beim Erleben der Spielatmosspähre helfen, wenn du die Spielfigur nach dir benennst, oder ihr einen Namen gibst, der dir gefällt oder den du gerne tragen würdest.")
	(textausgabe "Viel Spaß beim Lesen und Spielen!")
	(textausgabe "- Sascha -")
	(spieler 'name (string-capitalize (eingabe "Welchen Namen möchtest du dir im Spiel geben?")))
	(textausgabe "Nett dich kennenzulernen, ~A!" (spieler 'name))
	(textausgabe "Jetzt verteilen wir erst einmal ein paar Punkte für dich ...")
	(textausgabe "Gewandheit ... ~A" (spieler 'gewandheit (+ (w6) 6)))
	(spieler 'max-gewandheit (spieler 'gewandheit))
	(textausgabe "Stärke ... ~A" (spieler 'stärke (+ (w6 2) 12)))
	(spieler 'max-stärke (spieler 'stärke))
	(textausgabe "Glück ... ~A" (spieler 'glück (+ (w6) 6)))
	(spieler 'max-glück (spieler 'glück))
	(textausgabe "Zu Beginn dieses Abenteuer wirst du nur ein absolutes Minimum an Objekten bei dir führen, als da wären ein Rucksack, ein Multifunktionstaschenmesser, eine Pumptaschenlampe und etwas Proviant. Außerdem ein Engergydrink, den du dir selber auswählen kannst.")
	(inventar 'rucksack 1)
	(inventar 'multifunktionstaschenmesser 1)
	(inventar 'pumptaschenlampe 1)
	(inventar 'proviant 10)
	(case (text-auswahl '(Stärketrank Gewandheitstrank Glückstrank) "Zur Auswahl stehen ein Stärketrank, ein Gewandheitstrank oder ein Glückstrank. Welchen willst du?")
	  (stärketrank
	   (textausgabe "Wenn du den Stärketrank trinkst, wird er deine Stärke wieder auf den Anfangswert zurücksetzen.")
	   (inventar 'stärketrank 1))
	  (gewandheitstrank
	   (textausgabe "Wenn du den Gewandheitstrank trinkst, wird er deine Gewandheit auf den Anfangswert zurücksetzen.")
	   (inventar 'gewandheitstrank 1))
	  (glückstrank
	   (textausgabe "Wenn du den Glückstrank trinkst, wird er dein Glück auf den Anfangswert zurücksetzen.")
	   (inventar 'glückstrank 1)))
	(textausgabe "So, das hätten wir! Zeit, daß du dich in dein Abenteuer stürzt, hm? :-)"))


(defun vorwort ()
	(textausgabe "Vorwort")
	(textausgabe "Viele Menschen würden der Wiedervereinigung und der Politik die Schuld geben, das die Schwächeren in unserer Gesellschaft ins Abseits geraten. Die meisten wissen nicht, was das wirklich bedeutet, du jedoch schon. Seit Jahren bereits kämpfst du um dein Überleben, mit nicht mehr als deiner Kleidung am Körper, einem Ruckscack, einem Zelt, einer Pumptaschenlampe, einem Multifunktionstaschenmesser billigster Machart und einigen wenigen weiteren Kleinigkeiten. Momentan ist das Wetter noch Mild - und so hast du die Nacht in den Überresten einer alten Turmruine verbracht, die unbeachtet im Park des Museums am Münster steht.")
	(textausgabe "Doch diese Nacht war anders, als jede andere Nacht, die du im Freien verbracht hast. Sie war unheimlich. Du kannst dich erinnern, wie du aus dem Zelt klettertest - aber es war noch dunkel, und pechschwarz. Der ganze Himmel, alles war nur Schwarz, kein einziger Stern war zu sehen, nicht die Silhouette des Mondes, keine Wolken - rein gar nichts! Und dann war da dieses röhrende, metallische Geräusch. Und noch bevor du weißt, wie dir geschieht, wird es langsam hell - und du stehst immer noch da, in der gleichen Position. Bewegungslos. Die Frage ist nur, wie lange schon hast du bewegungslos herumgestanden? Und wieso überhaupt konntest du dich nicht bewegen?")
	(textausgabe "Da es noch immer recht früh ist, gehst du hinunter zu dem kunstvollen Brunnen des Museums. Der Park gehört in diesen Stunden dir allein. Du nimmst ein kurzes Bad und fühlst dich direkt viel wohler, so sauber, wenn es auch ein klein wenig eiskalt war. Mit einem alten Handtuch reibst du dich notdürftig trocken und ziehst dir saubere Unterwäsche an, bevor der Rest deiner alltäglichen Kleidung folgt. Den Rest deiner Kleidung verstaust du in einem Stoffbeutel und gehst zurück zu deinem Zelt. Mit wenigen Handgriffen baust du es auseinander, dann faltest du es und verstaust es vorsichtig.")
	(textausgabe "Mit deinem ganzen Habseligkeiten bepackt gehst du die Treppen des Münster hinunter. Als Kind hast du dafür immer den schnellen Weg gewählt, mit den glatten Sandahlensohlen seid deine Freunde und ihre einfach immer stehend das Geländer hinuntergerutscht. Kinder kenen keinerlei Angst vor Gefahren. Du überquerst den Platz hinüber in Richtung des ehemaligen Dortin-Hotels und schlägst den Weg, die Hügelstraße hinauf ein. Nahe dem alten Wasserturm am Odenkirchweg versteckst du den Rucksack mit deinem Zelt, der Isoliermatte und all den anderen Dingen, die du nur des Nachts brauchst - und wanderst nur mit deinem \"leichten Gepäck\" die Vitusstraße hinunter. Du wanderst die Lüperzender Straße entlang und gehst schließlich die Brücke am ehemaligen Zentralbad hinauf - um noch kurz den kleine Park zu durchqueren, bevor du in Kürze die Hindenburgstraße nahe des Stadttheaters, oder einem seiner Nachfahren erreichst."))


(defun ort-1 ()
  (textausgabe "Die Innenstadt macht um diese Zeit noch einen gänzlich unbelebten Eindruck - und kein einiges Licht erhellt die Straße. Keine Straßenlaterne, keine Glühbirne hinter einem Fenster, nicht einmal die Werbetafeln leuchten auf. Wenn du so drüber nachdenkst, auf deinem ganzen Weg hierhin hast du bisher keinerlei eingeschaltete Lichtquellen gesehen.\nNun stehst du an der Kreuzung Stepgesstraße Ecke Hindenburgstraße. Ein Stück die große Einkaufsstraße hinauf ist das Bäckereigeschäft, an dem dir jeden Morgen ein guter Geist eine braune Papiertüte mit dampfend warmen Gebäck bereitstellt. Du schaust in die Richtung, doch diesmal steht auf dem Stromkasten neben der Türe nicht der verlockende Beutel, sondern eine Gestalt steht in der Türe und winkt dir zu.")
  (auswahl '(ort-2 ort-3) "Wirst du auf die winkende Gestalt zugehen (1) oder ziehst du es vor, dich lieber scheu und mit knurrendem Magen aus dem Staub zu machen (2)?"))


(defun ort-2 ()
  (textausgabe "Vorsichtig und etwas nervös näherst du dich der offenstehenden Ladentüre, als ein regelrechter Platzregen losbricht, dann hört man es auch donnern. Jetzt läufst du auf die Türe zu. Eine Frau im weißen Kitel geht hinein und hält sie dir offen. Du gleitest hinein und bleibst neben einem Stehtisch stehen, während sie hinter dir die Türe zumacht und abschließt.")
  (textausgabe "\"Ist das nicht ein unheimlicher Morgen? Die ganze Stadt ist ohne Strom. Momentan habe ich ein Feuer im alten Backofen gemacht - gleich wird es hier ein paar frische Brötchen geben.\" sagt sie und geht an der Theke vorbei auf eine Tür die nach hinten führt zu.")
  (textausgabe "\"Na kommen sie!\" lädt sie dich ein, ihr zu folgen.")
  (textausgabe "Du gehst unsicher hinterher. Im Hinterzimmer steht ein Tisch, auf dem eine Thermoskanne und zwei Tassen bereitstehen. Eigentlich wolltest du sie schon längst einmal angesprochen haben. Daß das nun an einem so schlechten Tag passiert, tut dir leid!")
  (textausgabe "Sie kommt mit einem Tablet voller dampfender Brötchen aus einem Nebenraum, während du unschlüssig neben dem Tisch stehst.")
  (textausgabe "\"Bitte setzen sie sich doch! Ich dachte, wir könnten zusammen frühstücken!\" sagt sie und stellt das Tablett auf dem Tisch ab. Dann setzt sie sich hin.")
  (textausgabe "\"Ich bin Elke.\" stellt sie sich vor.")
  (textausgabe "Als du ihr gerade mit deinem Namen antworten willst, geschieht das unfassbare: die Sirenen fangen an zu heulen, mitten in dieser Kackophonie des Gewitters - und das um Fünf Uhr, Morgens in der Frühe.")
  (textausgabe "\"Was geht da vor sich?\" fragt Elke, während du dir das Sirenensignal genauer anhörst.")
  (textausgabe "\"Haben sie ein Radio hier, oder einen Fernsehe?\" fragst du Elke, aber die zuckt nur mit den Schultern.")
  (textausgabe "\"Kein Batteriebetriebenes, der Strom ist ausgefallen, da tut's der Fernseher nicht.\"")
  (textausgabe "Sie deutete mit der Hand neben deinem Ohr entlang hinter dich. Du drehst dich auf der Sitzfläche des Stuhls um und erblickst das Gerät. Die Sirenen heulen jetzt bereits seit mehr als einer Minute.")
  (textausgabe "\"Das ist keine Übung, das ist eine echte Warnung, aber vor was?\" sagst du laut zu dir. Du sprichst sehr oft mit dir selbst, vermutlich, um nicht vor Einsamkeit den Verstand zu verlieren. Wieder erklingt das Donnern und Grollen.")
  (auswahl '(ort-4 ort-5 ort-6) "Ignorierst du das Geheul, das Donnern und die Blitze und wirst  erst einmal in Ruhe Frühstücken (1) oder würdest du lieber Elke fragen, ob sie dich rauslassen kann, damit du einmal nach dem Rechten sehen kannst (2)? Vielleicht würdest du sie ja auch lieber fragen, ob sie über einen anderen Empfänger verfügt, der nicht an das Stromnetz angeschlossen werden muss (3)?"))


(defun ort-3 ()
  (textausgabe "Du bist ein äußerst mißtrauischer Mensch und vertraust der unbekannten, winkenden Gestalt nicht. Zu deinem Glück fängt es an zu regnen, eine hervorragende Möglichkeit um sich zu verpissen. Leider ist es aber ein echter Platzregen, daher solltest du möglichst schnell ein Dach über den Kopf bekommen. Du rennst ein Stück weit die Hindenburgstraße hinab und biegst in den Lichthof ein, wo du dich während des Regens unterstellts.")
  (textausgabe "Kaum bist du eine Minute drin, als plötzlich überall die Sirenen anfangen zu heulen. Draußen hörst du ein lautes Donnergrollen. Auch ist es viel dunkler geworden, seit die dichten grauen Wolken sich mit ihrem Regen auf Gladbach stürzen. Plötzlich pfeift ein Schuß an dir vorbei und schlägt in die Fensterscheibe ein Stück weit vor dir ein.")
  (textausgabe "Schneller, als du selbst es für möglich gehalten hast, hechtest du in den nächsten Eingang und siehst zurück. Du bist nicht alleine. Von oben herab kommt eine Gestalt gelaufen. Aus deinem Rucksack holst du das Taschenmesser heraus und klappst es auf. Fast ist es so, als wärst du unbewaffnet, aber eben nur fast. Dein Gegner kommt weiter auf dich zugeeilt.")
  (when (eql (ort-13) 'ende)
	(return-from ort-3 'ende))
  (textausgabe "Es war zwar Notwehr, dennoch hockst du jetzt hier neben einer Leiche. Dein Gefühl sagt dir, daß es wohl das Beste wäre, dich so schnell wie möglich aus dem Staub zu machen, Unwetter hin oder her.")
  (auswahl '(ort-50 ort-51) "Möchtest du den Lichthof nach Norden in Richtung Kaiserstraße verlassen (1), oder nach Süden in Richtung Hindernburgstraße (2)?"))


(defun ort-4 ()
  (textausgabe "Du setzt dich zu Elke an den Tisch. Das Wetter, den Donner und den fehlenden Strom ignorieren, lernt ihr euch langsam besser kennen, während die Anzahl der Backwaren auf dem Tablett deutlich schrumpfen und der heiße Kaffee aus der Thermoskanne sich wohlig in deinem Inneren verteilt. Der Ort, die Zeit, die Situation, alles könnte man als idyllisch bezeichnen, wenn, ja wenn nicht auf einmal dieses seltsame Geräusch eine absterbenden Sirene gewsen wäre. Es war kein Abschwellen, wie man es kenn, sondern klang eher, wie ein entenartiger Aufschrei.")
  (textausgabe "Und dann nimmst du plötzlich wahr, das mit dem Verstummen dieser Sirene, die Masse an Sirenen, die noch klingen, weniger hoch ist, als zuvor. Aber was viel wichtiger ist, einige gehen immer noch. Langsam wirst du dir der Situation bewußt, die da draußen herrscht - und du beschließt, nachsehen zu gehen, was da los ist.")
  #'ort-5)


(defun ort-5 ()
  (textausgabe "Du schlägst vor, daß du dich draußen umsiehst und zurückkommst, sobald du weißt, was los ist. Elke begleitet dich zur Ladentüre und läßt dich raus. Der Regen prasselt von oben herab und immer wieder donnert es. Du winkst ihr kurz zu und rennst so gut es geht an den Hauswänden entlang die Hindenburgstraße hinauf, trotzdem bist du nach weniger als einer Minute bis auf die Unterhose durchnäßt. Als du am ehemaligen Heinemann vorbeikommst und durch die kurze Passage läuftst, bemerkst du an der Straßenecke zum Sankt Vith hinunter einen brennenden Polizeiwagen. Ein mulmiges Gefühl geht dir durch den Magen. Eigentlich wolltest du ja in das Haus, das früher deinem Großvater gehört hat - und von dem aus man eine Übersicht über die ganze Stadt hat. Trotzdem ergreift dich gerade die Angst.")
  (auswahl '(ort-7 ort-8 ort-9) "Vielleicht wäre es ja besser, die Straße wieder hinunter zulaufen - und sich im Geschäft bei Elke zu verstecken (1)? Du könntest auch auf den Polizeiwagen zulaufen, vielleicht kannst du jemandem helfen, der im Wagen eingeklemmt ist (2)? Natürlich kannst du auch deinem ursprünglichen Plan weiter verfolgen,, das Haus deines Großvaters zu erreichen (3)!"))


(defun ort-6 ()
  (textausgabe "Elke schüttelt den Kopf, und verneint deine Frage.")
  (textausgabe "\"Tut mir leid, so etwas ist nicht hier im Geschäft. Das einzige Radio, das mir einfallen würde, wäre mein Autoradio.\" entgegnet sie, nimmt sich ein warmes Brötchen vom Tablett und kaut daran herum.")
  (auswahl '(ort-17 ort-4 ort-5) "Willst du sie wirklich überreden, mit dir zu ihrem Auto zu gehen (1), oder würdest du jetzt nicht viel lieber frühstücken (2)? Möchtest du hingegen unbedingt draußen nachsehen was los ist, könntest du Elke auch bitten, dich rauszulassen (3)."))


(defun ort-7 ()
  (textausgabe "Vollkommen durchnäßt kommst du wieder am Geschäft an. Drinnen ist alles dunkel. Du klopfst mehrfach, aber nichts rührt sich.")
  (auswahl '(ort-14 ort-15 ort-16) "Willst du es weiter mit klopfen und rufen probieren (1), oder willst du versuchen, ob du die Türe öffnen kannst (2), oder aber möchtest du dir einen anderen Weg suchen (3)?"))


(defun ort-8 ()
  (if (plusp (inventar 'gewehr))
	  (progn
		(textausgabe "Du läufst erneut über den Platz um dir den Polizeiwagen noch einmal anzusehen. Das Feuer ist erloschen. Du überlegst dir gerade, ob du eine noch gründlichere Durchsuchung des Wagens vornehmen sollst, als du ein Geräusch aus der Richtung der Kirche hören kannst. Ein Geschoss schlägt in die Karosserie des Wagens ein.")
		(auswahl '(ort-10 ort-11) "Willst du dich dem Kampf stellen (1) oder versuchen zu fliehen (2)?"))
	  (progn
		(textausgabe "Der Polizeiwagen raucht eigentlich nur noch. Fahrer- und Beifahrertüre stehen weit offen. Vorsichtig umrundest du den Wagen. Kein Mensch ist darin, aber auch kein Anzeichen der beiden Polizisten, die sich darin hätte befinden müssen. Im offenen Seitenfach der Beifahrertüre ist ein Gewehr, die Verriegelung der Waffe ist offen. Du könntest sie an dich nehmen. Du durchsuchst den Kofferraum. In diesem befindet sich ein Munitionspäckchen mit 100 Patronen, ein Verbandskasten und ein Warndreieck.")
		(let ((mitgenommen (text-auswahl '(alles gewehr patronen verbandskasten warndreieck) "Willst du etwas von den Sachen mitnehmen?")))
		  (when (member 'gewehr mitgenommen)
			(inventar 'gewehr 1))
		  (when (member 'patronen mitgenommen)
			(inventar 'patronen 100))
		  (when (member 'verbandskasten mitgenommen)
			(inventar 'verbandskasten 1))
		  (when (member 'warndreieck mitgenommen)
			(inventar 'warndreieck 1))
		  (auswahl '(ort-9 ort-7) "Willst du jetzt weiter zum Haus deines Großvaters (1) oder zurück zum Geschäft (2)?")))))


(defun ort-9 ()
  (if (ereignis 'dreistelzer)
	  (progn
		(textausgabe "Der Kampf ist nicht spurlos am Haus deines Großvaters vorbeigezogen. Die Fenster sind zersplittert, die Haustüre liegt am Boden, die Wände rußgeschwärzt.")
		(auswahl '(ort-21 ort-28) "Willst du in das Haus deines Großvaters hinein (1) oder versuchen, um die Ecke zu laufen und den Kapuzinerplatz zu erreichen (2)?"))
	  (if (ereignis 'dreistelzer-gesehen)
		  (progn
			(textausgabe "Du läufst auf das Haus deines Großvaters zu und in den Eingang zwischen Gaststätte und Kleidungsgeschäft. Für einen Moment mußt du vor der alten Holztüre mit der Glasscheibe halten. Mit Hilfe deines Taschenmessers daudert es nur ein paar Sekunden, dann ist die Türe öffnet und du schlüpfst hinein.")
			#'ort-21)
		  (progn
			(textausgabe "Du läufst an der Häuserwand des Alten Marktes entlang und gelangst schließlich zum Haus deines Großvaters. Du machst dich mit Hilfe deines Taschenmessers kurz am Schloß der alten Holztüre zu schaffen, sie geht fast sofort aus. Den Trick hast du schon als Kind gemacht, wenn du mal wieder deinen Schlüssel zu Hause vergessen hattest - und er klappt immer noch wunderbar. Du hastest die Türe hinauf. Immer noch donnert es draußen, so laut, wie du es schon lange bei keinem Gewitter mehr gehört hast. Auf jeder Etage drückst du den Lichtschalter, aber keiner schaltet das Licht an. Auch hier ist vollkommener Stromausfall. Kurz bevor du die oberste Etage erreichst, hören die Sirenen auf zu heulen, was für dich nur zum Vorteil sein kann, steht doch noch eine alte motorbetriebene E57. Du obersten Treppen sind am kürzesten und am verwinkelsten. Links die Waschküche läßt du liegen, da kannst du nichs sehen. Du stürmst nach rechts, den kurzen Gang entgang und reißt die hintereste Türe auf der rechten Seite auf. Du stürmst auf das Dachfenster an der Südseite zu. Überall siehst du dunkelgraue Wolkenberge sich auftürmen. Statt bis nach Düsseldorf kannst du nicht einmal bis an die Westgrenze Mönchengladbachs sehen. Du wendest den Blick ab und läufst zu einem der Fenster an der Nordseite. Hier bietet sich die ein gleiches Bild. Die Wolken sind so dicht, daß du nicht einmal den Gladbacher Wasserturm in dieser Brühe sehen kannst. Bleiben noch die Fenster an der Südseite. Bereits als du dich ihnen näherst, erkennst du, daß du hier ein weiteres Sichtfeld haben wirst. Du reißt das Fenster auf um besser sehen zu können. Von oben peitschen dicke Regentropen herab, aber das aufgeklappte Fenster schützt dich weitestgehend. Die Wolkenwand ist hier einige Kilometer entfernt. Da plötzlich wird es hell in der Wolkenwand. Wie gebannt starrst du dahin. War das ein Blitz? Da wieder. Wieder ein Blitz. Wieder in der Wolkenwand. Das ist jetzt aber sehr ungewöhnlich. Minutenlang starrst du auf die Wolkenwand - und auch an zwei oder drei anderen Stellen erblickst du immer wieder kurze Blitze - mitten in der Wolkenwand, aber fast auf Bodenhöhe. Ein mulmiges Gefühl breitet sich in deinem Magen aus. Gerade, als du das Fenster schließen willst, kommt etwas aus der Wolke. Etwas riesiges. Ein Objekt, wie ein Turm, aber es bewegt sich, wie ein Stelzenläufer. Und ein Lichtbogen, wie ein Blitz spannt sich von ihm aus, dann brennt etwas zu seinen - Beinen? - während es sich weiter in Richtung der Gladbacher Innenstadt voranschiebt.")
			(textausgabe "Deine Nackenhärchen haben sich aufgerichtet. Du weißt zwar nicht genau, was das ist, aber es bringt Zerstörung, soviel ist sicher. Hastig schließt du das Fenster. Du rennst aus dem Dachstuhl heraus zurück in den Flur und eilst die Treppen hinab, bis du unten an den der Haustüre ankommst. Was wirst du tun?")
	(ereignis 'dreistelzer-gesehen 't)
	(auswahl '(ort-8 ort-20 ort-7) "Du läufst hinaus und zu dem Polizeiwagen (1), du läufst die Kellertreppe hinab und suchst dort Schutz vor dem was kommt (2) oder du läufst zurück zu Elke, der Frau, die dich täglich mit Backwaren versorgt hat und erzählst ihr, was du gesehen hast (3)?")))))


(defun ort-10 ()
  (when (eql (ort-13) 'ende)
	(return-from ort-10 'ende))
  (textausgabe "Du bist nicht stolz darauf, einen anderen Menschen getötet zu haben, aber du warst ja nicht der Angreifer. Trotzdem fühlst du dich schäbig. Etwas in dir hat sich verändert, das kannst du spüren. Noch immer prasselt der Regen auf dich, so als wäre nichts gewesen. Und hinter den Wolkenbergen, da bist du dir sicher, scheint immer noch die Sonne.")
  (auswahl '(ort-9 ort-7) "Willst du jetzt weiter zum Haus deines Großvaters (1) oder zurück zum Geschäft (2)?"))


(defun ort-11 ()
  (if (versuche-dein-glück)
	  #'ort-12
	  (if (eql (ort-13) 'ende)
		  (return-from ort-11 'ende)
		  #'ort-10)))


(defun ort-12 ()
  (textausgabe "Geduckt rennst du an der Häuserwand entlang und dann rechts in die Straße, auf das Kabuff zu, dort rennst du links um die Ecke in Richtung Kapuzinerplatz, läufst dann aber stattdessen um die Kirche herum, ein kurzes Stück über den Parkplatz - und dann die Stepgesstraße hinunter, bis du schließlich wieder die Hindenburgstraße erreichst. Etwas atemlos stellst du dich an die Wand. Mehr als nur einmal bist du beim Laufen geschlittert. Das Wasser läuft in Strömen in die Kanalisation. Du vergewisserst dich, bleibst mehrere Minuten in deiner versteckten Position, bis du dir sicher bist, daß der Angreifer dir nicht gefolgt ist. Erst jetzt begibst du dich zurück zur Bäckerei.")
  #'ort-7)


(defun ort-13 ()
  "Begegnung mit einem Zufallsgegner der Zivilseite"
  (let* ((gegner (case (w6)
				   (1 (list "verängstigte Frau" 2 3))
				   (2 (list "Plünderer" 6 3))
				   (3 (list "aggressiver Jugendlicher" 6 4))
				   (4 (list "Polizist" 5 4))
				   (5 (list "Gutbürger" 6 5))
				   (6 (list "Heckenschütze" 8 4))))
		 (kampfausgang (kampf (list gegner))))
	(if kampfausgang
		(progn
		  (spieler 'getötete-menschen 1)
		  (if (>= (w6) 4)
			  (if (zerop (inventar 'pistole))
				  (if (j-oder-n-p "Willst du die Pistole deines Gegners an dich nehmen?")
					  (inventar 'pistole 1)))
			  (if (zerop (inventar 'gewehr))
				  (if (j-oder-n-p "Willst du das Gewehr deines Gegners an dich nehmen?")
					  (inventar 'gewehr 1))))
		  (inventar 'patronen (w8)))
		(progn
		  (textausgabe "Das war nicht dein bester Kampf. Um ehrlich zu sein, das war dein schlechtester Kampf - und auch dein letzter Kampf. Dein allerletzter Kampf, den du nicht überlebt hast. Mit dir ist es zu ENDE gegangen.")
		  'ende))))


(defun ort-14 ()
  (if (>= (w6) 4)
	  (progn
		(textausgabe "Dein Klopfen hat dich unvorsichtig gemacht. Du hast einen unerwarteten Gast angelockt.")
		(if (eql (ort-13) 'ende)
			(return-from ort-14 'ende)))
	  (textausgabe "Bisher scheinen deine Bemühungen nicht zu fruchten. Drinnen reagiert Nichts und niemand auf dein Klopfen."))
  (auswahl '(ort-14 ort-15 ort-16) "Willst du es weiter mit klopfen und rufen probieren (1), oder willst du versuchen, ob du die Türe öffnen kannst (2), oder willst du dir einen anderen Weg suchen (3)?"))


(defun ort-15 ()
  (if (versuche-dein-glück)
	  #'ort-18
	  #'ort-19))


(defun ort-16 ()
  ;; Versuche einen anderen Weg zu finden
  ;; bisher nicht zu Ende verfolgt
  'ende)


(defun ort-17 ()
  (textausgabe "Du schulterst dir deinen Rucksack, während Elke ein paar Papiertüten mit Backwaren füllt. Danach siehst du ihr zu, wie sie ein paar Flaschen aus dem Kühlschrank holt und in eine weitere Tüte stopft. Als sie alles gepackt hat, verschwindet sie kurz im Nebenzimmer und kommt, jetzt einen Mantel tragend, eine Damenhandtusche um die Schulter hängend zurück. Ihr nennt die ganzen Taschen und verlasst das Geschäft. Elke schließt den Laden hinter sich ab, macht sogar die Bewegung zum Scharfschalten der Alarmanlage, dann sagt sie: \"Komm!\" und geht dir voraus, die Stepgesstraße hinunter.")
  (textausgabe "Mißtrauisch wirfst du einen Blick zum Jugendheim hinüber. Oft genug, haben dir da schon junge Kerle aufgelauert. Diesmal aber bleibt alles ruhig. Elke führt dich ein kurzes Stück weiter und hält vor einer Parkhaustüre. Sie holt einen Schlüssel heraus und schließt auf. Gemeinsam geht ihr in das Innere des Parkhauses. Das Treppenhaus stinkt nicht so muffig und nach Urin, wie es oft der Fall ist, wenn Betrunkene sich eine Stelle für ihre Notdurft suchen. Ihr steigt hinab bis zum dritten Untergeschoß.")
  (textausgabe "Schließlich erreicht ihr Elkes Auto, einen schon etwas älteren Kombi. Sie entriegelt das Auto per Knopf. Ihr verladet eure Taschen und deinen Rucksack im Kofferraum und setzt euch in das Auto, wobei du auf dem Beifahrersitz Platz nimmst. Das kleine Licht der Mittelkonsole ist im Moment die einzige aktive Lichtquelle im ganzen Parkhaus.")
  (textausgabe "Elke fummelt am Autoradio herum, aber das Digitalradio findet keinen Sender den es darstellen kann. Schließlich gibt sie die Suche auf.")
  (textausgabe "\"Hier drinnen haben wir keinen Empfang, ich müßte schon aus dem Parkhaus herausfahren, damit wir etwas hören könnten.\" sagt sie mürrisch.")
  (auswahl '(ort-100 ort-101) "Stimmst du Elke zu - und ihr fahrt mit dem Auto aus dem Parkhaus heraus in den Regen (1) oder hast du ein mulmiges Gefühl und glaubst nicht, daß das eine gute Idee ist (2)?"))


(defun ort-18 ()
  (textausgabe "Du betrittst den Laden, hast aber sofort das Gefühl, alleine zu sein. Du durchstöberst jedes Zimmer, jeden Raum, jeden Winkel, aber Elke ist nicht mehr da. Sie hat wohl nicht geglaubt, daß du wieder kommst. Dir bleibt nichts anderes übrig, als den Laden zu verlassen.")
  #'ort-34)


(defun ort-19 ()
  (textausgabe "Du bekommst es einfach nicht hin, die Türe geht nicht auf. Du denkst, daß sie wohl nicht mehr da sein wird und wendest dich ab.")
	#'ort-34)


(defun ort-20 ()
  (textausgabe "Du öffnest die eiserne Treppe und gehst die Kellertreppe hinunter. Als du am Fuße der Treppe ankommst, siehst du vor dir die eiserne Doppeltüre, die in den Heizungskeller führt. Nach rechts führt ein weiterer Weg zum Lagerraum, wo deine Großmutter ihre Kartoffeln lagerte. Neben der Treppe führt ein Weg nach hinten, wo die Mieter des Hauses ihre Kellerabteile haben.")
  (auswahl '(ort-21 ort-22 ort-23 ort-24) "Wenn du zurück nach oben gehen willst (1). Möchtest du in den Heizungskeller (2). Willst du in den Gang, der zum Kartoffellager deiner Großmutter führt (3). Würdest du gerne in den rückwärtigen Bereich gehen (4)."))


(defun ort-21 ()
  (if (ereignis 'dreistelzer)
	  (progn
		(textausgabe "Der Hausflur sieht mitgenommen aus, doch die Steintreppe ist immer noch intakt. Der Zählerkasten hängt von der Wand herunter und die Treppe zur Kneipe wurde eingetreten. Dahinter siehst du das Spiel von Flammen an der Wand. Du gehst durch die Türe. Die Kneipe sieht verwüstet aus. Du gehst durch den Gang in die Küche, durchquerst sie und stehst in der ehemaligen Garage deines Großvaters, wo bequem vier Autos Platz hätten. Du erinnerst dich, wie vor vielen Jahren hier der Vogelschuß des Schützenfestes stattgefunden hatte - und schaust hinauf zu den Glasbausteinen, die einen Teil des Bodens vom Dachgarten ausgemacht haben. Der Ort, wo deine Schaukel stand, wo du Schnittlauch von dem gepflanzten Gemüse gegessen hattest, wo Clarence und Judy manchmal Auslauf haben durften. Von innen kannst du die Türe öffnen und zum Kapuzinerplatz hinaushuschen.")
		(auswahl '(ort-25 ort-20 ort-28) "Willst du es riskieren, die Treppe hochzusteigen (1), der Steintreppe in den Keller folgen (2) oder das Haus in Richtung Kapuzinerplatz verlassen (3)?"))
	  (progn
		(textausgabe "Du stehst in einem großen Treppenflur. An der Seite befindet sich der Hängeschrank mit den ganzen Ablesegeräten für Elektrizität und Wasser. Rechts an der Wand für eine Steintreppe hinauf in das erste Stockwerk. Geradeaus ist eine Holztüre, durch die man in den Kellerbereich der Gaststätte kommt, allerdings ist sie dauerhaft abgeschlossen. Rechts neben der Holztüre, unterhalb der Steintreppe, befindet sich eine Eisentüre, hinter der sich der Abstieg in den Keller befindet.")
		(auswahl '(ort-25 ort-20 ort-26) "Willst du die Treppe hinaufsteigen (1), in den Keller hinuntergehen (2), oder das Haus verlassen und zurück auf den Alten Markt (3)?"))))


(defun ort-22 ()
  (if (ereignis 'agartha)
	  (textausgabe "Ein großteil der Kohlen liegt auf dem Boden. An der rußgeschwärtzten Wand kannst du das Wort \"Agartha\" lesen. Der Heizungskessel arbeitet derzeit nicht. Sonst befindet sich nichts im Heizungskeller.")
	  (progn
		(textausgabe "Als du die Türe öffnest, ist es recht stickig im Raum. Der Heizkessel ist aus, obwohl in einem großen Verschlag an der Seite sich die Kohlen nur so stapeln. Als du genauer hinguckst, hast du das Gefühl, den geritzen Buchstaben \"A\" an der Wand zu sehen.")
		(when (j-oder-n-p "Willst du soviele der Kohlen zur Seite räumen, um zu sehen, ob das \"A\" vielleicht Teil eines Wortes ist? Eine letzte Nachricht deines Großvaters an dich?")
		  (textausgabe "Deine Kleidung verdreckt vollständig, während du auf dem Kohleberg herum krabbelst - und Brikett um Brikett wegräumst. Und noch dazu machst du eine ganz schöne Menge Krach!")
		  (if (>= (w6) 4)
			  (if (eql (ort-13) 'ende)
				  (return-from ort-22 'ende)))
		  (textausgabe "Endlich hast du das Gekritzel an der Wand freigelegt. Was du liest, ist ein Wort: \"Agartha\"")
		  (ereignis 'agartha 't))))
  (auswahl '(ort-23 ort-24 ort-21) "Willst du in in den Gang zum Kartoffelkeller (1), oder willst du zu den rückwärtigen Keller (2) oder möchtest du zurück nach oben in den Treppenflur (3)?"))


(defun ort-23 ()
  (if (and (ereignis 'agartha) (not (ereignis 'schlüssel-gefunden)))
	  (progn
		(textausgabe "Du gehst durch den nach Moder riechenden Gang auf das ehemalige Kartoffellager deiner Großmutter zu. Du wendest dich nach links und stehst im Türrahmen. Da erinnerst du dich. Du siehst nach links, neben dem Rahmen war ein kurzer Nagel, an dem ein Schlüssel mit einem weißumrandeten Plastik-Schlüsselkärtchen hin. Auf dem weißen Schild war mit der krakeligen Handschrift deines Großvaters dein eigener Name niedergekritzelt.")
		(textausgabe "Der Nagel steckt zwar noch in der Hand, aber er deutet nach unten - und kein Schlüssel hängt daran. Du kniest dich nieder - und durchwühlst den mit Würmern belebten Karttoffel-Schimmel-Matsch am Boden. Der Geruch steigt dir unangenehm durch die Nase. Minutenlang durchpflügst du den Matsch. Der Verzweiflung nahe willst du schon aufgeben, als dein Finger etwas kaltes berührt. Schnell wühlst du den Gegenstand frei. Es ist der Schlüsselring - mit dem Schlüssel daran.")
		(textausgabe "Zärtlich streichst du den Schmutz von dem Schlüsselkärtchen ab, doch die Handschrift deines Großvaters ist nicht mehr erkennbar. Dennoch nimmst du den Schlüssel an dich.")
		(textausgabe "Du stehst auf und verläßt den Kartoffelkeller und gehst zurück.")
		(ereignis 'schlüssel-gefunden 't))
	  (textausgabe "Du gehst durch den nach Moder riechenden Gang. Als du an das ehemalige Kartoffellager deiner Großmutter kommst, mußt du feststellen, das es einfach nur leer und verdreckt ist. Seit ihrem Tod hat es wohl niemand mehr betreten. Die Kartoffeln müssen schon vor Jahren verschimmelt sein. Du wendest den Blick zur Seite. Die Klappe, durch die früher immer die Bierfässer hinabgelassen wurden, befindet sich immer noch am Ende des Ganges. Jetzt würde sie allerdings in den Wintergarten der Gaststätte führen. Da du hier nichts weiter finden kannst, beschließt du zurückzugehen."))
  #'ort-20)



(defun ort-24 ()
  (textausgabe "Du gehst an der Kellertreppe vorbei. Hinter ihr macht der Gang einen Knick nach rechts, und nach einem kurzen Durchang stehst du vor einem breiten Gang. Überall sind durch hölzerne Boxen getrennt die Kellerabteile der Mieter dieses Hauses. Der Verschlag, hinter dem deine Sachen gesichert waren, ist leer, bis auf die Erinnerungen daran in deinem Kopf. Du drehst dich um und gehst zurück.")
  #'ort-20)


(defun ort-25 ()
  (if (and (not (ereignis 'buch-gefunden)) (ereignis 'schlüssel-gefunden))
	  (progn
		(textausgabe "Du steigst die Treppen hinauf bis zum Dachgeschoss. Hinter der vorletzten Türe auf der rechten Seite, befindet sich das Spielzimmer, daß dein Großvater dir schenkte. Hier hatte er auf einer Spanplatte eine Eisenbahn mit Bergen und Tunneln für dich aufgebaut, auch wenn er es meistens war, der die Züge fahren ließ. Du steckst den Schlüssel, den du im Keller gefunden hast, ins Schloss - und schließt die Türe auf. Du öffnest sie - und vor dir schwingt sie nach innen. Die Eisenbahn ist immer noch fest montiert auf der Spanplatte, die auf dem rötlichen Boden liegt. Du trittst ein - und schließt die TÜre hinter dir, schließt sie sogar ab, um ungestört zu sein. Auf einem Holzstuhl findest du ein Buch. Als du es dir nimmst und umdrehst, ist es der fehlende Band aus der Bibliothek. Groß prangt der Name Agartha darauf. Du setzt dich auf den Stuhl und blätterst es durch. Immer wieder stößt du auf Passagen in dem Buch, die dein Großvater mit Bleistift unterstrichen hat. In den Passagen ist die Rede von einer Stadt, die sich in den Tiefen der Erde befinden soll. Es finden sich Referenzen darauf, das es Eingänge zu dieser Stadt geben soll - und das wenigstens einer dieser Eingänge in Tibet ist. Plötzlich flattert ein Blatt aus dem Buch. Es muß zwischen den Seiten gelegen haben. Du bückst dich und hebst es auf. Mit der krakeligen Schrift deines Großvaters steht darauf notiert: \"Münster   Sarg   Innenhof   Keller   Rheydt\"")
		(textausgabe "Du steckst den Zettel und das Buch in deinen Rucksack.")
		(ereignis 'buch-gefunden 't))
	  (progn
		(if (> (w6) 4)
			(textausgabe "Du steigst die Treppen bis zur dritten Etage hinauf. Von hier aus schaust du hinüber zu dem Balkon, wo früher dein Freund Guido gelebt hat. Du erinnerst dich daran, wie ihr ein primitives Blechdosentelefon gespannt hattet, dessen eines Ende ihr mit einem Holzflitzebogen ihr hinübergeschossen hattet. In Erinnerungen versunken steigst du danach die Treppe wieder hinunter ins Erdgeschoss.")
			(textausgabe "Du ersteigst die Treppenstufen bis hinauf in das Dachgeschoss. Noch einmal versuchst du, ob du eine der verschlossenen Türen aufbekommst und schaust durch die Schlüssellöcher, doch erscheinen dir alle Räume leer und verlassen. Du drehst dich um und gehst die Treppenstufen wieder hinab in das Erdgeschoss."))))
  #'ort-21)


(defun ort-26 ()
  (if (ereignis 'dreistelzer)
	  (progn
		(textausgabe "Die Fassaden der Häuser am Alten Markt brennen knisternd, einige drohen einzustürzen. Tote Leiber bedecken den Ort, den ein ein Dreistelzer entweiht hat, der nun an damit beschäftigt ist, aus dem Polizeirevier einen Aschehaufen zu produzieren. Eine Art Panzerwagen steht unter ihm - und du könntest schwören, das gerade zwei Gestalten in die Bank gelaufen sind. Hoffentlich bist du bisher niemandem aufgefallen.")
		(auswahl '(ort-9 ort-44 ort-28) "Wenn du Glück hast, kannst du ungesehen in das Haus deines Großvaters huschen (1), du kannst aber auch versuchen, die Turmstiege ungesehen zu erreichen (2) oder versuchen, die unbemerkt an den Häusernwänden des Kapuzinerplatzes entlang zu bewegen (3)"))
	  (if (and (ereignis 'dreistelzer-gesehen) (plusp (spieler 'getötete-gegner)))
		  (progn
			(textausgabe "Der Alte Markt ist in beißenden Qualm gehüllt, während zeitgleich Regen in Strömen herniederprasselt. Ein riesiger Dreistelzer steht auf der Kreuzung zur Aachener Straße und aus einer Öffnung leckt züngelt eine lange blaugrüne Flamme gegen die Fassade der naheliegenden Häuser. Zu versuchen, den Kapuzinerplatz oder die Turmstiege zu erreichen, dürfte keine gute Idee sein.")
			(auswahl '(ort-43 ort-35 ort-27 ort-34) "Wie es aussieht, kannst es versuchen die Waldhausener Straße zu erreichen (1), der Weg den Abteiberg hinunter an der Polizeiwache vorbei sollte auch möglich sein (2). Auch der Weg den Marktstieg entlang (3) oder die Hindenburgstraße hinab (4) erscheinen dir noch sicher."))
		  (progn
			(textausgabe "Du befindest dich auf dem Alten Markt. Regen gießt in Strömen herab, so daß du nur undeutlich die Silhouette der Kirche Mariä-Himmelfahrt ausmachen kannst, während der Brunnen mit der kleinen Statue davor vollkommen unsichtbar ist. Der Brunnen füllt sich langsam mit dem herniedergehenden Wasser, dennoch erinnert er dich, nur an ideenlos aufeinandergestapelte Zementbauklötze. Die Stühle der Eisdiele sind über den Markt und die Straße verteilt.")
			(when (> (w6) 4)
			  (textausgabe "Ein Schuß schlängt hinter dir in die Hausfassade ein.")
			  (when (eql (ort-113) 'ende)
				(return-from ort-26 'ende)))
			(auswahl '(ort-43 ort-28 ort-27 ort-35 ort-34 ort-9) "Von hier aus kannst du weiter zur Waldhausener Straße (1), dem Kapuzinerplatz (2), dem Marktstieg (3), ebenso kannst du den Abteiberg hinunter (4), hinüber zur Hindenburgstraße (5) oder weiter zum Haus deines Großvaters (6)")))))


(defun ort-27 ()
  (textausgabe "Atemlos kommst du an der Ecke Stadtmauer/Marktstieg an. Du denkst nicht, daß der Dreistelzer in der Ferne dich bemerkt hat.")
  (if (> (w6) 4)
	  (textausgabe "Der Marktstieg ist für dich mit vielen sentimentalen Erinnerungen verbunden. An den schrängen blonden Sohn des Metzgers zum Beispiel, mit dem du dich hier öfters getroffen hattest. Einmal warst du sogar in der Schlachterei hier drin. Es war wohl einer der Auslöser, warum du nie wirklich auf Fleisch standest. An der Ecke war eine Bäckerei. Du hast sie geliebt, vor allem für die Süßigkeiten und das Eis, das du dir hier stets gekauft hast. Schade, daß die nette alte Bäckerin nicht mehr da ist."))
  (if (ereignis 'dreistelzer)
	  (auswahl '(ort-28 ort-29 ort-45) "Von hier aus könntest du weiter zum Kapuzinerplatz (1) oder aber, wenn du mutig genug bist, die Kaiserstraße hinab (2). Auch die Wallstraße (3) dürfte noch sicher sein.")
	  (progn
		(textausgabe "Du bewegst dich an der Hauswand entlang bis zur Ecke.")
		(auswahl '(ort-34 ort-26 ort-28 ort-45 ort-29) "Möchtest du von hier aus weiter zur Hindenburgstraße (1), zum Alten Markt (2), zum Kapuzinerplatz (3), zur Wallstraße (4) oder zur Kaiserstraße (5)?"))))


(defun ort-28 ()
  (if (ereignis 'dreistelzer)
	  (progn 
		(textausgabe "Der Kapuzinerplatz ist in beißenden Qualm gehüllt, während zeitgleich Regen in Strömen herniederprasselt. An der Westseite des Platzes sieht man ab und zu grüne Flammen durch die Luft züngeln. Der Dreistelzer ist weitergezogen, wobei er eine Schneise der Verwüstung hinterlassen hat. Der Platz hat sich in eine Kakophonie aus toten Leibern und geplatzten Träumen verwandelt, die sich auf den düsteren Pflastersteinen verteilen. Flammen züngeln aus den Schächten der Tiefgarage hinauf.")
		(when (eql (ort-113) 'ende)
		  (return-from ort-28 'ende))
		(when (> (w6) 4)
		  (textausgabe "Der Kapuzinerplatz ist wohl nicht länger ein Ort des Friedens in deiner Erinnerung. Nicht der Ort des Glücks, in dem du in deiner Kindheit so manche Kirmes und Trödelmarkt erlebt hast. Dafür aber spukt dir durch den Kopf, wie du einmal Karneval mitten während der Kirmes hier überfallen wurdest.")
		  (auswahl '(ort-26 ort-44 ort-30 ort-29 ort-27 ort-9) "Von hier aus kannst du zum Alten Markt (1), zur Turmstiege (2), das Haus Zoar betreten (3), zur Kaiserstraße (4), zum Marktstieg (5) oder zum Haus deines Großvaters (6)")))
	  (progn
		(when (> (w6) 4)
			(when (eql (ort-113) 'ende)
			  (return-from ort-28 'ende))
			(textausgabe "Du siehst hinauf zu dem Haus, wo Guido mit seiner Mutter gewohnt hat. Vor deinem geistigen Augen siehst du für einige Momente den Krahn, um dessen Arm herum Marios und du Frisbees in einer Art Bumerang-Technik geworfen haben. Du siehst die aufgebaute Bühne vor deinem inneren Auge, die Plakette der kölschen Rockband, der Figuren ihr ausgeschnitten habt. Du stellst dir vor, wie du Bierdeckel wie Shuriken durch die Gegend wirfst, während gleichzeitig deine Kleidung immer nasser wird vom Regen."))
		(when (> (w6) 3)
			(textausgabe "Deine Erinnerung an den Überfall auf den Juwelier kommt dir wieder in den Sinn. Wie du das Auto gesehen hast, und die Polizei riefst - und als du dann hinuntergingst, als der Polizeiwagen da war, schickten sie dich weg - weil du ein Kind warst. Nicht mal an dem Nummernschild hatten diese Idioten Interesse."))
		(auswahl '(ort-26 ort-44 ort-30 ort-29 ort-27 ort-9) "Von hier aus kannst du zum Alten Markt (1), zur Turmstiege (2), das Haus Zoar betreten (3), zur Kaiserstraße (4), zum Marktstieg (5) oder in das Haus deines Großvaters (6)"))))


(defun ort-29 ()
  (when (> (w6) 4)
	(textausgabe "Vermutlich wirst du dich niemals daran gewöhnen, daß die Autos jetzt in verkehrter Richtung die Kaiserstraße und die Regentenstraße entlang fahren."))
  (when (> (w6) 4)
	(textausgabe "Für einen Moment bist du abgelenkt, und wärst fast zu dem Kinocenter hinübergegangen, um dir die Bilder zu Filmen anzugucken, wie ihr als Kinder das immer auf den Kinos am Kapuzinerplatz gemacht habt. Damals gab es das Kinocenter hier nicht. Stattdessen waren hier Häuser - und ein großer Parkplatz. Auch an die alte Marienschule erinnerst du dich noch."))
  (when (> (w6) 4)
	(textausgabe "Am liebsten würdest du zu dem Haus gehen, wo früher der Kindergarten war, den du besuchtest. Du erinnerst dich an die Spielkameraden von damals, die Turnhalle im ersten Stock, den Hof hinter dem Haus und das fahruntüchtige Auto, das ihr dort hattet. An das Klettergerüst, das Laufen auf Stelzen - und auch an die Prügel, die du oft von anderen, die kräftiger waren als du, bezogen hast."))
  (if (= (w6) 6)
	  (when (eql (ort-113) 'ende)
		(return-from ort-29 'ende))
	  (when (eql (ort-13) 'ende)
		(return-from ort-29 'ende)))
  (textausgabe "Es scheint so, als wäre die Kaiserstraße nicht der beste Ort, um sich dort lange aufzuhalten.")
  (auswahl '(ort-31 ort-45 ort-27 ort-28) "Möchtest du die Straße hinab zur Blücherstraße (1), zur Wallstraße (2), zum Marktstieg (3) oder zum Kapuzinerplatz (4)?"))


(defun ort-30 ()
  (when (> (w6) 4)
	(textausgabe "Du näherst dich dem Haus in dem du in deiner Jugend oftmals Billard gespielt hast, bis dieser Irre aus Hephata mit einer Pistole auf dich geschossen hatte. Vergessen hast du den Tag niemals, er war für dich wie eine Wiedergeburt. Du hast das Drumherum nie vergessen. Wenn du genau drüber nachdenkst, sah der Blödmann Chico tatsächlich so aus, wie einer der Zeichner der MAD-Comics."))
  (if (> (w6) 4)
	  (progn
		(textausgabe "Die Eingangstüren hängen nur lose in den Angeln. Jemand hat sich wohl bereits heute Zutritt zu dem Haus verschafft. Du bist dir nicht sicher, ob es eine gute Idee war, sich dem Gebäude zu nähern, aber deine Erinnerungen und deine Neugierde siegt. Als du die Treppe hinuntergehst, hörst du ein Geräusch aus Richtung des Kickers. Du springst mit einem Hechtsprung von der Treppe auf das nahe Sofa.");
		(when (eql (ort-113) 'ende)
		  (return-from ort-30 'ende)))
	  (progn
		(textausgabe "Die Eingangstüren hängen nur lose in den Angeln. Jemand hat sich bereits Zutritt verschafft. Vorsicht gehst du in das Innere, aber alles ist ruhig. Du schleichst die Treppe herunter. Es sieht immer noch so aus wie in deiner Jugend. Der Kicker und der Billardtisch, nichts hat sich hier verändert, selbst die beiden Sofas sind noch da. In der Turnhalle brennt eine Notlampe - und ein Fenster steht weit offen.")
		(auswahl '(or-28 ort-29) "Willst du zurück auf den Kapuzinerplatz (1) oder durch das geöffnete Fenster hinabspringen und hinüber zum Beginn der Kaiserstraße laufen (2)?"))))


(defun ort-31 ()
  (let ((raum (second *zug*)))
	(case raum
	  (ort-51
	   (textausgabe "Du siehst dich um, aber im Moment ist alles still und du fühlst dich sicher.")
	   (mahlzeit))
	  (ort-29
	   (textausgabe "Nachdem du die Kaiserstraße entlang gegangen bist, ist zu deiner rechten Hand der Adenauerplatz mit seinem häßlichen Sandspielplatz, während zu deiner Linken die Stadtbibliothek lockt. Rechts von dir verläuft die Kleiststraße bis hin zum Lichthof."))
	  (ort-50
	   (textausgabe "Du folgst der Kleiststraße entlang, passierst den Adenauerplatz und stehst schließlich an der Kreuzung Kaiserstraße. Dir gegenüber auf der anderen Straßenseite liegt die Stadtbibliothek.")))
	(when (> (w6) 4)
	  (textausgabe "Du erinnerst dich, das ein Stück weiter geradeaus, Nikos Mutter ihr Geschät hatte, während Niko auf der Regentenstraße wohnte. Das letzte Mal, als du etwas von ihm hörtest, war seine Stimme, aus dem Radio. Die kanntest viele Leute, die auf der Regentenstraße wohnten. Dagmar, Nicola, ihren Bruder den Nazi (den du nicht abkonntest, weil er ihr Bruder war - und weil er Nazi war) - und Thomas, der Computerfreak und Ärztehörer, Dietrich der Tänzer lebte auch dort und Dirk, der total auf Adam Ant abfuhrt. Für dich war die Regentenstraße immer der Ort, wo du deine Grundschule besuchtest, Fußball spieltest und dir ab und zu bei der Apotheke dein Junior-Heft oder eine Dose Pullmoll holtest."))
	(auswahl '(ort-29 ort-50 ort-32) "Willst du den Hügel hinauf die Kaiserstraße entlang (1), der Kleiststraße folgen (2) oder der Stadtbibliothek deine Aufwartung machen (3)?")))


(defun ort-32 ()
  (when (and (ereignis 'verzeichnis-gelesen) (ereignis 'agartha) (not (ereignis 'karte-gefunden)))
	(textausgabe "Du erkletterst hastig den Geröllhaufen, über den du in das Innere der Stadtbibliothek gelangst. Du gehst die Reihen der Apothekerschränke ab, bis du das Fach erwischst, in der Du dir einen Hinweis auf das Wort \"Agartha\" erhoffst. Als du endlich die Karte mit dem Begriff hast, stehen darauf 3 Bücher. Du nimmst dir die Karte heraus und durchstöberst die Bibliothek. Zwanzig Minuten später, als du das dritte Buch in der Hand hältst, erkennst du unter den Ausleihern des Buches deinen Großvater in der Liste. Beim Durchblättern des Buches bemerkst du, daß darin eine Karte abgebildet ist. Du liest dir die Texte der Innenumschläge durch und staunst nicht schlecht. In dem Buch ist die Rede von einer inneren Welt. Du erinnerst dich an das Buch von Jules Verne, daß dir dein Großvater geschenkt hatte. Es liest sich unglaublich, daß auch diese Theorie Vernes wohl auf einem Körnchen Wahrheit beruht. Agartha soll einer dieser Orte sein. Das Buch verstaust du kurzerhand in deinem Rucksack, schulterst ihn und verläßt auf dem schnellsten Wege die Stadtbibliothek.")
	(ereignis 'karte-gefunden 't)
	(return-from ort-32 #'ort-31))
  (when (or (not (ereignis 'verzeichnis-gelesen)) (not (ereignis 'agartha)))
	(textausgabe "Die Stadtbibliothek erscheint von außen betrachtet menschenleer. Ein Teil der vorderen Fassade ist eingebrochen, du kletterst über den dadurch entstandenen Geröllhaufen und gelangst in das Innere der Bibliothek. Da es keinen Strom gibt, bist du dankbar für die Unmenge an Apothekerschränken in deren ausziehbaren Schubladen es nur so von Karteikartenbeschreibungen der Bücher wimmelt. Leider aber hast du keinerlei Idee, wonach du suchen könntest. Da du nichts in der Bibliothek findest, was dir momentan helfen könnte, verläßt du sie.")
	(ereignis 'verzeichnis-gelesen 't)
	(return-from ort-32 #'ort-31))
  (textausgabe "Kaum daß du die Bibliothek betreten hast, glaubst du hinter den Regalen eine Gestalt wahrzunehmen.")
  (when (eql (ort-13) 'ende)
	(return-from ort-32 'ende))
  (textausgabe "Um nicht noch in weitere Scherereien hineinzustolpern, verlä8t du die Bibliothek.")
  #'ort-31)


(defun ort-33 ()
  (textausgabe "Der Lichthof sieht aus wie ein Scherbenmeer. Hier muß es bereits zu heftigen Kämpfen gekommen sein. Die angrenzende Passage ist voller Rauch.")
  (when (ereignis 'dreistelzer)
	(textausgabe "Vor dem unteren Ausgang des Lichthofs steht der Metallfuß eines Dreistelzers. Das hier ist eine Falle, eine Sackgasse. Du drehst dich um und rennst so schnell du kannst Richtung der Stadtbibliothek davon.")
	#'ort-50)
  (let ((raum (second *zug*)))  
	(when (and (ereignis 'verzeichnis-gelesen) (> (w6) 3))
	  (when (eql (ort-113) 'ende)
		(return-from ort-33 'ende)))
	(case raum
	  (ort-50
	   (auswahl '(ort-51 ort-50) "Möchtest du zur Hindenburgstraße hinab laufen (1) oder lieber zurück die Kleiststraße entlang zur Kaiserstraße (2)?"))
	  (ort-33
	   (auswahl '(ort-50 ort-34 ort-51) "Willst du die Passage hinauf zur Kleistraße laufen (1) oder kehrst du lieber um und läufst die Hindenburgstraße hinauf (2) oder hinab (3)?")))))


(defun ort-34 ()
  (when (ereignis 'dreistelzer)
	(textausgabe "Von hier aus kannst du sehen, das sich am unteren Teil der Hindenburgstraße, hinter dem Knick beim Kaffeehausröster, sich ein weiterer Dreistelzer befindet. Du kannst nicht ihn selber sehen, aber ab und an sieht man den Schimmer seiner Flammenwaffe auf den Häuserfassaden und in den Fenstern wiederspiegeln. Es wäre nicht gerade klug, sich in die Richtung zu wagen. Ähnlich sieht es auch an der Stepgesstraße aus, auch von dort kommen die grauenvollen mechanischen Geräusche der Dreistelzer. Der dritte im Bunde steht immer noch neben dem Polizeirevier und verunstaltet das Gebäude.")
	(auswahl '(ort-37 ort-45 ort-27) "Du kannst von hier aus versuchen, die Krichelstraße zu erreichen - und es von dort aus zum Münsterplatz zu versuchen (1) oder du versuchst in die Wallstraße zu huschen (2), wenn du schnell genug bist, kannst du auch an der Stadtmauer entlang zum Marktstieg (3)"))
  (when (> (w6) 4)
	(textausgabe "Für einen Moment siehst du vor deinem geistigen Auge, wie aus dem klotzigen Modegeschäft wieder das Kaufhaus wurde, in dessen Verkaufsräumen du viele Stunden deiner Kindheit verbracht hast, mit dem Betrachten von Spielzeugen, dem Hineinschnuppern in Büchern und Herumtippen auf 8-Bit Computern. Du erinnerst dich sogar wieder daran, wie an deinem Geburtstag Ephraim Kishon hier war und Autogramme in seine Bücher schrieb."))
  (textausgabe "Während du die Hindenburgstraße entlang läufst, kannst du in der Ferne außer dem Nebelschleier auch einen gewissen Rauchschleier ausmachen.")
  (when (> (w6) 4)
	(textausgabe "Im obersten Stockwerk der Häuserzeile in dem sich auch der Burgerfresstempel befindet, steigen erste Flammen in die Höhe, die aber glücklicherweise durch den stetigen Regen in ihrer Ausbreitung behindert werden."))
  (when (= (w6) 6)
	(when (eql (ort-113) 'ende)
	  (return-from ort-34 'ende)))
  (auswahl '(ort-33 ort-51 ort-45 ort-26 ort-35 ort-29) "Von hier aus kannst du weiter die in Richtung Lichthof (1), in Richtung des Hauptbahnhofs (2), zur Wallstraße (3), in Richtung des Alten Markts (4) oder in Richtung Abteiberg (5). Du kannst es auch die Croonsallee entlang zur Kaiserstraße (6)."))


(defun ort-35 ()
  (let ((raum (second *zug*)))
	(when (eql raum #'ort-46)
	  (textausgabe "Als du langsam den Abteiberg hinaufsteigst, erblickst du am oberen Ende des Berges drei Gestalten in seltsamen Uniformen. Sie tragen eine Art von Helmen, die an Taucherglocken erinnern, mit großen Filtern daran wie von Gasmasken. Und wie es scheint, haben sie dich gesehen. Als Fluchroute hast du jetzt nur noch den hinter dir liegenden Geroweiher.")
	  (let ((gegner '(("1. Soldat" 7 6)
					  ("2. Soldat" 6 4)
					  ("3. Soldat" 7 8))))
		(case (kampf gegner #'ort-46)
		  (ort-46
		   (return-from ort-35 #'ort-46))
		  (ende
		   (textausgabe "Sich auf drei kampferprobte Soldaten war einerseits sehr mutig von dir, andererseits aber auch sehr dumm. Du sinkst tödlich getroffen von einer Kugel zu Boden, während du die Schritte ihrer genagelten Stiefel näher kommen hörst. In Gedanken blickst du durch das Haus rechts von dir hindurch. Dahinter befindet sich ein Hügel, die Mauer, die zum Münstervorplatz führt, und an dieser Mauer wachsen Ranken herunter. Guido, Marco und du - ihr habt hier früher Verstecken gespielt, und du bist immer klammheimlich an den Ranken  gewesen - hast gewartet, bis sie oben an dir vorbei sind - und dann hinter ihrem Rücken, während sie die Treppen hinunterstiegen, nach oben geklettert. Warum warst du dieses Mal nicht so schlau, diesen Weg zu nehmen. Dann, als die Tritte der Soldaten auf dich einprasseln, naht für dich das ENDE.")
		   (return-from ort-35 'ende)))))
	(when (> (w6) 4)
	  (textausgabe "Es kommt dir immer noch befremdlich vor, daß die \"neue\" Polizeiwache jetzt an der Ecke ist. Früher in deiner Kindheit, seit ihr Kinder immer außen herum an der Statue Balderichs herumgekraxelt. Das jetzt zu versuchen, würde wahrscheinlich eine Menge Ärger mit der Bullerei mit sich bringen, obwohl ... jetzt wohl nicht mehr. Die Eingangstüren liegen zertrümmert am Boden."))
	(textausgabe "Der Abteiberg führt hinab zum Geroweiher, gleichzeitig aber auch zum Rathaus und dem Münster. Man sieht die Wand aus dem Boden ragen - und fühlt sich ein wenig an eine mittelalterliche Burg erinnert, auch wenn niemals eine hier stand.")
	(when (> (w6) 4)
	  (textausgabe "Wer hatte dir bloß die Geschichte erzählt, das früher der Urin, der Kot, und das Wasser der Waschzuber den Abteiberg hinunterlief, während unten die Leprakranken ihr krankes Dasein fristeten?"))
	(auswahl '(ort-26 ort-34 ort-37 ort-36 ort-52) "Möchtest du von hier aus zum Alten Markt (1), die Hindenburgstraße hinab (2), den Vorplatz am Münster entlang (3), das alte Rathaus betreten (4)? oder zum Park am Spatzenberg (5)?")))


(defun ort-36 ()
  (textausgabe "Das alte Rathaus von Mönchengladbach ist vermutlich viel weniger alt, als der Name vermuten läßt. Es hat eine große Toreinfahrt - und man kann von ihm aus in den Innenhof des Münsters gelangen, wenn du dich nicht irrst. Etwas idiotisch ist der Parpkatz davor - und die Straße die nach Links zum Abteiberg-Museum abbiegt. Nur ein Spiegel soll die Autofahrer davor schützen, das sie an dieser uneinsehbaren stelle mit ihren Autos Unfälle bauen. Viel intelligenter wäre es wohl, die Durchfahrt hier bautechnisch nicht mehr zu ermöglichen, aber wer bist du, daß dich solche Gedanken überhaupt interessieren?")
  (when (> (w6) 4)
	(textausgabe "Beim Betreten des Rathauses erinnerst du dich, wie du für deine Laterne, die tatsächlich deine Mutter gebastelt hatte, du in das Rathaus gebeten wurdest - dir wurde für die Laterne gedankt - und dann wurde sie untem im \"Alten Zeug Haus\" am Abteiberg ausgestellt. Und das alles nur, weil \"deine\" Martinsfackel von den Wappen der Bundesrepublik Deutschland, des Landes Nordrhein-Westfalen und dem alten und neuen Wappen der Stadt Mönchengladbach geziert wurde. Heute würdest du deswegen wahrscheinlich von \"Urheberrechtsschützern\" gejagt werden."))
  (let ((raum (second *zug*)))
	(when (and (eql raum #'ort-41) (ereignis 'durchgang-geöffnet))
	  (ereignis 'dreistelzer 't)
	  (textausgabe "Der ganze Boden erhebt, und du hörst ein lautes mechanisch Geräusch, als du oben auf dem Alten Markt einen Dreistelzer sehen kannst, der sich in Stellung bringt. Schreie ertönen aus seiner Richtung, dann siehst du zwei fremde Soldaten, die in deine Richtung die Treppe von Mariä Himmelfahrt heruntergelaufen kommen. Ein weitere stürmt aus der Polizeiwache heran, er ist ein richtiger Hühne.")
	  (let ((gegner '(("1. Soldat" 6 7)
					  ("2. Soldat" 5 6)
					  ("3. Soldat" 8 7))))
		(case (kampf gegner #'ort-46)
		  (#'ort-46
		   (return-from ort-36 #'ort-46))
		  ('ende
		   (textausgabe "Sich auf drei kampferprobte Soldaten war nicht gerade deine klügste Entscheidung. Du sinkst tödlich getroffen von einer Kugel zu Boden, während du die Schritte ihrer genagelten Stiefel näher kommen hörst. In Gedanken blickst du hinüber zur Treppe, die von Mariä Himmelfahrt herunterführt. Du erinnerst dich, wie ihr als Kinder auf den Sohlen eurer Sandalen im Stehen das Geländer heruntergerutscht seid, so als wärt ihr Wellenreiter in den Brandungen vor Hawai. Dann landet der Absatz eines Stiefels in deinem Gesicht. Der Schmerz explodiert in deinem Gesicht, alles wird Schwarz. Und dann ist es auch schon vorbei. Dein ENDE ist gekommen.")
		   (return-from ort-36 'ende)))
		(auswahl '(ort-41 ort-35 ort-37 ort-51) "Du bist dir nicht sicher, ob es eine gute Idee ist, zum Alten Markt zu laufen. Immerhin bleibt dir noch der Rückzug in den Innenhof des Münsters (1), du könntest auch den Abteiberg hinunterlaufen, der Hügel sieht zu steil für den Dreistelzer aus (2), oder den Vorplatz des Münsters entlanglaufen (3) in der Hoffnung, es vielleicht zum Geroweiher zu schaffen. Zu guter letzt bleibt dir noch die Flucht durch die Gasse, in der Hoffnung, die Hindenburgstraße zu erreichen (4)"))))
  (auswahl '(ort-35 ort-41) "Möchtest du hinaus auf den Abteiberg (1) oder den Innenhof betreten (2)?"))


(defun ort-37 ()
  (let ((raum (second *zug*)))
	(case raum
	  (ort-52
	   (textausgabe "So schnell dich deine Beine tragen, rennst du am Waffengeschäft vorbei, vorbei an den Häusern, in denen Stefan und Dirk lebten (der diesen geilen TI 99/4a hatte) - und die Straße hinunter. Du greifst das Schild und schleuderst um die Ecke der Weiherstraße. Das \"Alte Zeug Haus\" läßt du rechts liegen, der \"Alte Ulan\" ist auch nicht dein Ziel, sondern die Treppenstufen, die zum Münsterplatz hochführen. Du nimmst sie so schnell wie möglich und fällst fast noch über die letzte Stufe, dann bist du wieder oben. Und vollkommen außer Atem. Du ringst nach Luft und betrachtest das stets geschlossene Portal des Münsters auf dieser Seite.")
	   (auswahl '(ort-36 ort-38 ort-46 ort-34) "Du kannst vo hier aus jetzt nach links und zum Rathaus gehen - wobei da natürlich die Gefahr ist, vom Dreistelzer an der Polizeiwache bemerkt zu werden (1), du kannst auch das Münster betreten (2), sinnloserweise die Treppen zum Geroweiher hinunterspurten (3) oder den Abteiberg entlang zum oberen Teil der Hindenburgstraße spurten (4)"))
	  (ort-34
	   (textausgabe "Du spurtest das kurze Stück \"An der Stadtmauer\" entlang und biegst in die Krichelstraße ab, wo du direkt in einen Soldaten rennst.")
	   (when (eql (ort-113) 'ende)
		 (return-from ort-37 'ende))
	   (textausgabe "Kaum daß du den Kampf gewonnen hast, läuft du weiter. Kurz bevor du den Kirchplatz erreichst, läufst du nach links, den Weg entlang, das kurze Stück über die Brücke hinüber zum Museum, dort die Treppe hinunter und um die Probstei herum, bis du schließlich vor dem Haupteingang des Münsters stehst.")
	   (auswahl '(ort-38 ort-36 ort-46) "Willst du in das Münster hinein (1), oder an ihm vorbei zum Rathaus laufen, in der Gefahr, auf den Dreistelzer zu stoßen (2), oder willst du die Treppenstufen hinab zum Park am Geroweiher (3)?"))
	  (ort-51
	   (textausgabe "Als du an oben am Hügel ankommst, rennst du am Elektronikladen links herum. Du nimmst die Treppe, hinauf zum Museum und rennst dort über die Steinplatten, auf denen du in deiner Kindheit mit deinen Freunden Skateboard gefahren und Roller Skater gelaufen bist, aber diesmal heißt der Sport nicht Bremstest (wobei ab und an das Skateboard unter dem Gitter hindurchschoß), sondern überleben, einfach nur überleben. Du rennst über das Museum und dann die Treppe hinab, ein kurzes Stück durch den Park und dann hinüber, um das Münster herum. Etwas erschöpft stehst du nun vor dem Haupteingang. Dein Blick schweift über den Geroweiher. Bis jetzt siehst du dort unten keine Dreistelzer, allerdings machen der Wind und der Regen es nicht leicht, viel zu erkennen.")
	   (auswahl '(ort-38 ort-36 ort-46) "Willst du in das Münster hinein (1), oder an ihm vorbei zum Rathaus laufen, in der Gefahr, auf den Dreistelzer zu stoßen (2), oder willst du die Treppenstufen hinab zum Park am Geroweiher (3)?"))
	  (ort-46
	   (when (ereignis 'dreistelzer)
		 (textausgabe "Statt offen die lange Treppe am Münster hochzulaufen, bist du das kurze Stück durch den Park rechts gespurtet, in dem du noch am Morgen dein Zelt aufgebaut hattest. Dann bist du seitlich hinten am Kirchenschiff herum gelaufen. Nun überlegst du, wie es weiter gehen soll.")
		 (auswahl '(ort-38 ort-27) "Du kannst in das Münster hinein (1), oder du könntest vesuchen, am ehemaligen Quelle vorbei und dann schnell über die Hindenburgstraße zu huschen, in der Hoffnung nicht bemerkt zu werden - und den Marktstieg erreichen (2)"))))
	(auswahl '(ort-35 ort-46 ort-38) "Willst du weiter in Richtung des Abteibergs (1), die Treppen hinab zum Geroweiher (2) oder willst du in das Gladbacher Münster hinein (3)?")))


(defun ort-38 ()
  (if (ereignis 'dreistelzer)
	  (textausgabe "Jetzt wo die Dreistelzer da draußen lauern, bist du dir nicht sicher, ob dieser uralte Bau dich noch lange schützen wird. Doch für einen Augenblick nimmst du dir Zeit. Die Zeit darüber nachzudenken, warum zum Teufel diese Typen da draußen sind - und deine Geburtsstadt in Schutt und Asche legen. Zu einer Erklärung gelangst du aber leider nicht.")
	  (textausgabe "Das Hauptschiff der Kirche ist erst vor kurzem neu restauriert worden und erstrahlt in hellstem weiß. Die Kirche wird trotz ihrer Größe weniger bedrückend als so manch alter Göttertempel. Der Duft des Weihwassers erfüllt die Luft. Ein Ständer beherbergt Bücher und Pamphlete, während an anderer Stelle, Kerzen aller Größe brennen. Es ist ein seltsamer Ritus in fast allen Religionen, sinnlose Brandrituale durchzuführen, die der Umwelt nicht wirklich helfen - dafür aber die Ressourcen dieses einzigen Planeten, auf dem wir leben angreifen. Du könntest dich auf einer der Bänke niederlassen, dennoch fühlst du dich hier nicht wohl. Seitlich an der Wand steht eine Türe offen. In deiner Kindheit war eine der Mieterinnen deines Großvaters verantwortlich für die Schatzkammer, sie war öfters mit dir in diesem Gang, der unter anderem zu jener Kammer führte."))
  (mahlzeit)
  (auswahl '(ort-37 ort-39) "Willst du das Münster verlassen (1) oder den Seitengang betreten (2)?"))


(defun ort-39 ()
  (textausgabe "Der Gang spannt sich um den Innenhof des Münsters. Glas gibt an jeder Stelle den Blick darauf frei, ebenso wie auf die Steinumtafelte Treppe, die in der Mitte des Rasenhofs nach unten führt. Auf der rechten Seite führt eine kurze Treppe hinab in die Gruft unter dem Hauptschiff.")
  (auswahl '(ort-38 ort-41 ort-40) "Möchtest du in das Hauptschiff des Münsters (1), den Innenhof betreten (2) oder die Treppe zur Gruft hinuntersteigen (3)?"))


(defun ort-40 ()
  (textausgabe "Du steigst die Treppe hinunter in die Gruft, die direkt unter dem Altar des Münsters liegt.")
  (when (> (w6) 4)
	(textausgabe "Du erinnerst dich, wie in der dritten Klasse einmal ein Klassenausflug hierhin führte und die Person, die euch das Münster zeigte, auch an diesem Ort mit euch kam. Sie erzählte eine krude Sage, von Soldaten, einem Spiel und dem Abdruck der Hand des Teufels, der sich auf dem Sarg befinden soll."))
  (when (and (ereignis 'durchgang-geöffnet) (not (ereignis 'sargdeckel-verschoben)))
	(textausgabe "Eigentlich hattest du erwartet, den Sargdeckel geöffnet vorzufinden, aber alle drei Särge sind geschlossen.")
	(when (j-oder-n-p "Möchtest du den Sargdeckel erneut verschieben?")
	  (ereignis 'sargdeckel-verschoben 't))
	(textausgabe "Das kratzende Geräusch, das beim Verschieben entsteht, läßt dir einen kalten Schauer über den Rücken laufen. Kommt es dir nur so vor, oder ist es gerade kühler geworden?")
	(textausgabe "Zu deiner Überraschung ist der Sarg vollkommen leer! Immer noch nervös wendest du dich ab und gehst die Treppe hinauf.")
	(return-from ort-40 #'ort-39))
  (when (and (ereignis 'karte-gefunden) (not (ereignis 'sargdeckel-verschoben)))
	(when (j-oder-n-p "Willst du dir den Inhalt des Sarges näher ansehen?")
	  (textausgabe "Mit aller Kraft gelingt es dir, wenn auch nur sehr langsam, den Sargdeckel zu verschieben - allerdings nicht sehr weit. Doch was du bereits sehen kannst, reicht dir aus: der Sarg ist vollkommen leer. Du blickst dich noch kurz um, und überlegst, ob du auch die Deckel der anderen Särge verschieben sollst. Angesichts des unnützen Kraftaufwands und der Tatsache, das bereits in diesem Sarg nichts drin ist, bist du der festen Überzeugung, das es sich bei den anderen ebenso verhält. Du wendest dich ab und gehst die Treppe hinauf.")
	  (ereignis 'sargdeckel-verschoben 't)
	  (return-from ort-40 #'ort-39)))
  (when (ereignis 'sargdeckel-verschoben)
	(when (j-oder-n-p "Willst du den Sargdeckel wieder in seine ursprüngliche Position schieben?")
	  (textausgabe "Mit aller Kraft gelingt es dir, wenn auch nur sehr langsam, den Sargdeckel wieder zu schließen. Als du es endlich geschafft hast, könntest du schwören, ein dumpfes, leises Geräusch von irgendwoher vernommen zu haben.")
	  (ereignis 'sargdeckel-verschoben 'nil)))
  (textausgabe "Da die Gruft ansonsten leer ist, verläßt du sie und steigst die Treppe hinauf.")
  #'ort-39)


(defun ort-41 ()
  (auswahl '(ort-39 ort-36 ort-42) "Durch eine Türe gelangst du in das Münster (1), ein weiterer Durchgang führt in das alte Rathaus (2). Eine Treppe in der Mitte des Innenhofes führt zu einer Stahltüre hinab (3)."))


(defun ort-42 ()
  (if (ereignis 'sargdeckel-verschoben)
	  (progn
		(textausgabe "Du steigst die Stufen in der Mitte des Innenhofes hinab und öffnest die Stahltüre. Als du in den Raum hineinblickst, erkennst du, das sich ein Spalt in einer Wand geöffnet hat. Du gehst in den Raum hiein und bemerkst beim näherkommen, das der Spalt tatsächlich eher ein richtiger Durchgang ist - und das dahinter ein Gang liegt.")
		(auswahl '(ort-53 ort-41) "Möchtest du den Gang betreten (1) oder willst du den Raum verlassen und die Treppe hinauf in den Innenhof gehen (2)?"))
	  (progn
		(textausgabe "Du steigst die Stufen in der Mitte des Innenhofes hinab und öffnest die Plastikklinge an der Stahltüre. Die Türe öffnet sich problemlos. Der Innenraum riecht muffig, die Wände sind alt und nicht überall eben. Der Raum wird offensichtlich für nichts mehr benutzt. Da es hier nichts weiter zu sehen gibt, drehst du dich um, verläßt den Raum und steigst die Treppe hinauf in den Innenhof.")
		#'ort-41)))


(defun ort-43 ()
  (textausgabe "Die Waldhausener Straße war früher das Herzstück der Mönchengladbacher Altstadt. Hier reihten sich die Kneipen und Diskotheken nur so aneinander, doch in den Anfangszeit der 1990er Jahre, hatten die christdemokratischen Hohlbirnen der Stadt dem ein Ende bereitet - und damit nachhaltig dem Flair der Stadt geschadet. Vor deinem geistigen Auge stellst du dir das ehemalige Blumengeschäfft der Schallenburgers vor. Du erinnerst dich daran, wie deine Mutter und ihr Mann oftmals in den Herbstmonaten dort Kränze gebunden hatten. Und daran, wie sie von den Schallenburgers die alte Nähmaschine bekamen, in der 10.000 Mark versteckt waren. Glücklicherweise waren sie so ehrlich, Micky und seiner Mutter das Geld zurückzugeben. Trotzdem wurde Micky nicht alt, und die Schallenburgers und die Geschichte ihres Blumen- und Friedhofsgeschäftes endeten bald darauf.")
  (if (ereignis 'dreistelzer)
	  (progn
		(textausgabe "Zurück in der Gegenwart jedoch mußt du erkennen, das in weiter Entfernung, die Waldhausener Straße entlang in Richtung Hardt, wenigstens 2 Dreistelzer sich an den Häusern und Menschen der Stadt vergehen - während ein weiteres dieser Ungetüme nicht weit bergauf steht.")
		(auswahl '(ort-52 ort-44) "Am sichersten wäre es wohl, den Fliescherberg hinabzulaufen (1), eventuell wäre auch der Schleichweg die Turmstiege entlang eine Alternative (2)"))
	  (auswahl '(ort-26 ort-44 ort-52) "Die Straße führt hinauf zum Alten Markt (1), neben dem Dicken Turm verläuft die Turmstiege (2) und ein weiterer Weg führt durch den kleinen Grünbereich des Fliescherberges (3)")))


(defun ort-44 ()
  (textausgabe "Die Turmstiege ist herrlich geschützt. An ihrem Ende zur Waldhausener Straße hin führt eine Treppe neben dem Dicken Turm hinab, von ihr selber eine Stiege den Turm hinauf und die lange Mauer ermöglicht es, dank der hervorragenden Steine an ihr hinaufzuklettern. Das taten deine Freunde und du schon als Kinder - und das tust du auch jetzt noch manchmal, den oben in der kleinen Zinne sieht einen Niemand.")
  (mahlzeit)
  (auswahl '(ort-43 ort-26 ort-28 ort-9) "Von hier aus hast du die Möglichkeit zur Waldhausener Straße zu gelangen (1), durch die Passage zum Alten Markt zu gehen (2) oder zum Kapuzinerplatz(3), wenn du schnell genug spurtest schaffst du es vielleicht sogar bis zum Haus deines Großvaters (4)"))


(defun ort-45 ()
  (when (ereignis 'dreistelzer)
	(textausgabe "Von hier aus kann man sehen, das aus Richtung des Wasserturms sich ein wenigstens 30 Meter hoher Dreibeiner dem Krankenhaus nähert."))
  (when (> (w6) 4)
	(textausgabe "Deine Gedanken schweifen ab zu Marios, der früher über dem Cafe wohnte. Oben in seiner Wohnung hattet ihr Commodore Basiclistings in den PC eingehämmert und stundenlang dann mit den Ergebnissen gespielt. Von hier aus seid ihr auch oft mit Dimmi zum Fußballspielen aufgebrochen. In der Bäckerei wohnte Michaela mit ihren Eltern, die die Backstube betrieben. Du hast leider nie erfahren, warum sie nach Ende der dritten Klasse nicht in die Schule zurückkehrte - oder wieso plötzlich die Bäckerei weg war."))
  (textausgabe "Du hast die Wallstraße immer gerne als Abkürzung benutzt, um zur Hindenburgstraße zu gelangen, nicht zuletzt wegen des Bücherladens, an dem du dir täglich am Schaufenster die Nase platt gedrückt hast, wo du dir das erste mal Michael Moorcocks \"Elric von Melnibone - Die Sage vom Ende der Zeit\" holtest, jenes Buch, das du dir sechs Mal kaufen mußtest, weil niemand es zurückgab, wenn man es ihm auch nur einmal auslieh. Und immer nach seinem Neuerwerb, hast du es nochmal gelesen. Du erinnerst dich auch noch an deine schräge Klassenkameradin, die einerseits total schüchtern her war vom Wesen - und die dennoch wie Boy George herumlief - und auch die Musik die ganze Zeit über hörte.")
  (auswahl '(ort-34 ort-29 ort-27 ort-28) "Du kannst von hier aus zur Hindenburgstraße (1), die Kaiserstraße hinab (2), den Marktstieg entlang (3) oder am Haus Zoar vorbei zum Kapuzinerplatz (4)"))


(defun ort-46 ()
  (let ((raum (second *zug*)))
	(if (eql raum 'ort-55)
		(textausgabe "Mit lange Zügen tauchst du weiter durch das eiskalte Wasser. Deine Lungenflügel leeren sich und beginnen langsam zu brennen. Du weißt, daß du den Rückweg nicht mehr schaffen würdest, also schwimmst du was das Zeug hält. Plötzlich bemerkst du über dir ein Licht - du hältst drauf zu - und durchbrichst die Wasseroberfläche. Luft strömt in deine gequälten Lungenflügel. Du ruderst mit den Armen schaffst es, nicht wieder einzutauchen und ruderst halb benommen zum Ufer, an dem du liegenbleibst. Du mußt etwas verschnaufen. Dann, als du wieder klarer denken kannst, nimmst du deine Umgebung wahr - und erkennst, das du am Ufer des Geroweihers liegst, unter den schützenden Ästen der alten Trauerweide, an denen ihr als Kind immer mit Tarzanschrei ins Wasser geschwunden seid. Du bleibst weiter einfach sitzen und schwelgst in Erinnerungen, bis du dir die aktuelle Gefahr wieder vor Augen führst und dich erhebst.")
		(textausgabe "Der Geroweiher. Ein kleiner Ort der Ruhe, an dem ein Spielplatz in deiner Jugend für ein gewisses Training deiner Muskeln sorgte, der aber auch der Austragungsort so mancher Keilerei war. Im Zentrum steht ein Stück alter Stadtmauer und wenn man hochguckt, sieht man auf dem Hügel das Münster. Der Weiher ist vielleicht gerade einmal zwei, höchstens drei Meter weit. Wenn du dich recht entsinnst, hatte eure Grundschullehrerin euch früher erzählt, der Geroweiher würde durch den Gladbach gespeist werden. Aber egal wie weit du auch zurückdenkst, den Gladbach hast du hier noch nirgendwo fließen sehen. Vielleicht, ist er ja nur noch eine Erinnerung, an alte Zeiten."))
	(if (ereignis 'dreistelzer)
		(progn
		  (textausgabe "Dank des Regens wäre es dir fast nicht aufgefallen, das zwei Dreistelzer sich nahe der Kreuzung zur Aachener Straße und in Gegenrichtung nahe der Rheydter Straße platziert haben. Zwar gibt es noch den kurzen Tunnel hinüber zur Turmstraße, aber wer weiß, wie es sonst in dem Viertel aussieht? In Anbetracht der Situation - und bei dem Gedanken daran, das noch ein weiterer Dreistelzer oben am Markt lauert, wären die beiden sicheren Routen, um von hier weg zu kommen, wohl nur der Weg den Spatzenberg entlang, oder der Weg hinauf zum Münster. Die Abhänge sind deine Verbündeten. Du kannst dir nicht vorstellen, daß diese 30 Meter hohen mechanischen Ungetüme dort entlangstelzen können.")
		  (auswahl '(ort-52 ort-37) "Willst du also zum Spatzenberg (1) oder zum Münster (2) hinauf?"))
		(auswahl '(ort-52 ort-35 ort-37) "Du kannst den Spatzenberg hinauf (1), oder den Abteiberg (2) oder die Treppen zum Münstervorplatz nehmen (3)"))))


(defun ort-47 ()
  (if (ereignis 'dreistelzer)
	  (progn
		(textausgabe "Von der Ecke der Hindenburgstraße aus kannst du sehen, das zwei Dreistelzer sich an den Bankgebäuden zu schaffen machen. Soldaten bringen aus der Bank heraus Säcke zu einem Panzerwagen. Für deinen Geschmack ist das etwas zu nah, um auch nur im Entferntesten daran zu denken, die Kreuzung zu überqueren. So drehst du dich lieber um und suchst dein Heil in der Flucht, die Hindenburg straße hinauf.")
		#'ort-51)
	  (progn
		(textausgabe "Ein Dreibeiner steht neben der Kaiser-Friedrich-Halle und bedeckt ihr Dach mit einem blau-grünlichen Flamme. Glücklicherweise ist er damit soweit weg, daß wohl kaum die Gefahr besteht, bemerkt zu werden.")
		(auswahl '(ort-51 ort-48) "Der Weg ist soweit sicher die Hindenburgstraße hinauf (1) oder weiter hinab bis zum Vorplatz des Hauptbahnhofs (2)"))))


(defun ort-48 ()
  (when (ereignis 'dreistelzer)
	(textausgabe "Von überall her, dringen die scheusslichen, mechanischen Geräusche der Dreistelzer auf dich ein, trotzdem ist hier auf dem Europaplatz noch keines dieser haushohen Metallmonster aufgetaucht."))
  (when (> (w6) 4)
	(textausgabe "Für die meisten Gladbacher ist das hier einfach der Platz vor dem Hauptbahnhof, für andere der Busbahnhof aber kaum jemand kennt den Namen des Platzes. Früher war da mal die Bank, wo der Mann deiner Mutter arbeitete. Amüsiert mußt du daran denken, wie er fassungslos nach Hause kam und davon erzählte, das die nette Frau aus dem Haus, die manchmal den Bankern etwas zu essen brachte, die Frau war, die ihren Mann zerstückelt und in Dosen im Bunten Garten verstreut hatte."))
  (textausgabe "Es stehen keinerlei Busse im Busbahnhof herum, auch sind kaum Menschen hier auszumachen. Betrachtet man nur den Platz, so wirkt alles so, wie es sein soll.")
  (auswahl '(ort-47 ort-49) "Du kannst von hier aus entweder die Hindenburgstraße entlang in Richtung Alter Markt gehen (1) oder das Innere des Hauptbahnhofs betreten (2)"))


(defun ort-49 ()
  (ereignis 'dreistelzer-gesehen 't)
  (when (> (w6) 4)
	(textausgabe "Während du auf die Doppeltüren des Bahnhofs zuschreitest kommt dir ein Bild aus deiner Erinnerung. Du wolltest dir das neueste Lustige Taschenbuch holen - und es war Sonntag. Normalerweise kam es erst Montags heraus, so wie auch das Yps-Heft, aber am Kiosk des Hauptbahnhofs gab es alles einen Tag früher. Und so fragtest du damals im Kiosk nach, aber dort sagte man, daß die Hefte noch nicht geliefert wurden - das es aber in der nächsten Stunde geschehen würde. Und so ging ich an die Modelleisenbahn, die dort stand - und spielte damit für die nächste Stunde. Und als ich mich schließlich wieder erinnerte warum ich gekommen war, holte mir die Verkäuferin das frisch gedruckte Taschenbuch aus dem Karton heraus. Ich pflückte mir noch das neue Yps mit seinem Gimmick aus dem Verkaufsständer, bezahlte und machte mich freudig auf den Nachhauseweg. Herr Jansen der Wirt war bei meinen Eltern, daran erinnerte ich mich, er hatte mir das Geld für das Taschenbuch gegeben."))
  (textausgabe "Beim Öffnen der Türe entfaltet sich ein grauenhafter Anblick vor mir, die Bahnhofshalle gleicht eher einem Schlachthaus und riecht strenger als eine Leichenhalle. Und am Ende des Bahnhofs sehe ich die metallenen Beine eines Dreistelzers. Mir bleibt nichts anderes übrig, als mich umzudrehen und das Weite zu suchen.")
	#'ort-48)


(defun ort-50 ()
  (let ((raum (second *zug*)))
	(if (eql raum 'ort-33)
		(textausgabe "Nervös siehst du hinter dich, aber es scheint nicht, als wäre dir jemand gefolgt.")
		(textausgabe "Die Kleiststraße ist jene kurze Straße zwischen dem Lichthof und dem Adenauerplatz, an dem sich das von allen Gladbachern gehasste Finanzamt befindet. Wenn man sich die Fassade des Gebäudes genauer betrachtet, so hat sie noch so etwas widerliches an sich, eine Ausstrahlung, als wäre sie ein verirrtes Stück 1940."))
	(when (ereignis 'dreistelzer)
	  (textausgabe "Ein Schuß erklingt!")
	  (when (eql (ort-113) 'ende)
		(return-from ort-50 'ende)))
	(auswahl '(ort-31 ort-33) "Du kannst am Adenauerplatz vorbei zur Blücherstraße (1) oder Richtung Hindenburgstraße in den Lichthof (2)")))


(defun ort-51 ()
  (let ((raum (second *zug*)))
	(when (eql raum 'ort-36)
	   (textausgabe "Du stürzt am Jugendheim vorbei, läßt das Museum rechts an dir vorbeigleiten. Früher hättest du wohl angehalten - und dir die Schaufenster des Elektronikladens angeguckt, jetzt aber hastest du quer über die Straße und läufst bergab auf die Stepgesstraße zu. Unten am Fuß der Stepgesstraße steht ein weiterer Dreistelzer, sein eines Standbein hat ein Auto plattgequetscht. Wer immer auch in dem Auto drin saß, er dürfte nun klein wie eine Briefmarke sein. Du läuftst nach links in Richtung Croonsallee. Als du an die Kreuzung zur Hindenburgstraße kommst, hältst du an. Auch hier sieht es nicht besser aus. Der Dreibeiner auf dem Alten Markt ist auch von hier aus zu sehen - er bleckt bläuliches Feuer gegen das ehemalige Heinemann. Und trotz des prasselnden Regens erkennst du auch am unten an der Hindenburgstraße, direkt um die Biegung herum noch ein weiterer Dreistelzer sein muß. Du mußt nicht lange überlegen. Du rennst zurück, hinauf zum Rathaus und dem Münster. Im Moment scheinen daß die letzten beiden Orte zu sein, wo die Dreistelzer noch nicht hingelangen. Verbissen rennst du los.")
	   (return-from ort-51 #'ort-37))
	(when (and (eql raum 'ort-47) (ereignis 'dreistelzer))
	  (textausgabe "Mit Mühe und Not gelingt es dir, die Bismarkstraße unbemerkt zu überqueren. Du hältst dich dicht an die Häuserfassade gedrückt, damit dich der andere Dreibeiner nicht bemerkt, der am Lichthof gegenüber der Friedrichstraße sein Zerstörungswerk fortsetzt. Belustigt denkst du an die Kuh Martina zurück. Zurück in der Realität rennst du so schnell dich deine Beine tragen in die Albertusstraße hinein, vorbei an dem Gebäude früher der Buchclub war, in dem deine Mutter sich seit einem halben Jahrhundert Mitgliedschaft durchgequält hat, vorbei an der Bank... Uff, das hätte auch schlecht ausgehen können. Auch hier ist eine Bank, aber noch kein Dreistelzer. Du läuftst weiter bis zum Adenauerplatz und rennst quer über diesen, bis du an die Ecke zur Blücherstraße kommst.")
	  (return-from ort-51 #'ort-31))
	(auswahl '(ort-34 ort-47 ort-33) "Von hier aus kannst du der Hindenburgstraße bergauf folgen (1) oder in die Gegenrichtung auf den Hauptbahnhof zu (2), oder Richtung Kleiststraße durch den Lichthof (3)")))


(defun ort-52 ()
  (if (ereignis 'dreistelzer)
	  (progn
		(textausgabe "Oben von der Annastiege aus, kannst du den Dreibeiner sehen - und auch die Männer, die allem Anschein nach die Bank leer räumen.")
		(auswahl '(ort-37 ort-43 ort-46) "Von hier aus kannst du versuchen die Neustraße langzuhuschen, ein kurzes Stück die Weiherstraße hinab zulaufen und dann die Treppen hinauf zum Münsterplatz (1), oder du rennst hinüber zur Waldhausener Straße, zum Dicken Turm (2) oder jagst wie in deiner Jugend die gesamte Anna-Schiller-Stiege bis zum Geroweiher hinab (3)"))
	  (progn
		(when (> (w6) 4)
		  (textausgabe "Der Fliescherberg ist ein ganz sentimentaler Ort für dich. Oben auf der kleine Plattform naher der Ecke Neustraße habt ihr immer Detektiv gespielt - und die Bank beobachtet. Hier wuchsen auch die Knallerbsen mit denen ihr die Leute erschreckt habt. An den Ästen der beiden Bäume neben der Plattofrm seid ihr immer hinaufgeklettert - und wenn ihr Glück hattet, dann konnte man im Winter an ein oder zwei Tagen auch mit dem Schlitten den ganzen Fliescherberg hinuntersausen."))
		(textausgabe "An der Seite des Fliescherberges führt die lange Anna-Schiller-Stiege hinab, neben der ihr als Kinder immer in den Büschen Cowboys und Indianer oder Ritter gespielt hattet. Hier waren immer eure selbst gemachten Pfeilbögen und Schwerter versteckt. Ein lang gewundener Weg führt von der Waldhausener Straße hinüber und ein weiterer von der Ecke Neustraße hinunter. Du weißt gar nicht mehr, wie oft ihr als Kinder wagemutig auf euren Skateboards hier heruntergerast seid, ja, gerast trifft es, denn der Berg ist so steil, das die Endgeschwindigkeit einfach nur verboten schnell war.")
		(auswahl '(ort-35 ort-46 ort-43) "Du kannst von hier aus zum Abteiberg hinaufgehen (1), hinab zum Geroweiher (2) oder hinüber zur Waldhausener Straße (3)"))))


(defun ort-53 ()
  (let ((raum (second *zug*)))
  	(if (eql raum 'ort-42)
		(textausgabe "Du betrittst den Gang. Er ist alt, die Luft riecht modrig, abgestanden. Aus deinem Rucksack hast du deine Taschenlampe und deinen alten Marschkompass aus der Bundeswehrzeit hervorgeholt. Du leuchtest die Wände ab und gehst langsam voran. Plötzlich hörst du hinter dir, wie sich der Spalt schließt. Du drehst dich noch um - aber es ist zu spät. Es scheint, als wäre dir der Rückweg versperrt. Hilflos zuckst du mit den Schultern. Wenn du richtig liegst, dann kannte dein Großvater diesen Ort - und er hätte dich niemals in eine Falle laufen lassen. So schreitest du weiter den Gang voran.")
		(textausgabe "Ganz im Westen ist der Gang eine Sackgasse. Du weißt, daß dort eine Türe ist, aber sie ist zu perfekt eingefasst - und du findest nichts, um sie zu öffnen. So gibst du nach einer Weile des Suchens auf und folgst dem Gang nach Osten."))
	(ereignis 'sargdeckel-verschoben)
	#'ort-54))


(defun ort-54 ()
  (when (zerop (inventar 'rucksack))
	(textausgabe "Du tastest dich durch den stockfinsteren Raum, bis du glaubst den Durchgang nach Norden gefunden zu haben und folgst diesem weiter.")
	(return-from ort-54 #'ort-55))
  (textausgabe "Du betrittst einen mehr oder weniger rechteckigen Raum. Es sind verschiedene, unleserliche Kritzeleien und Schriftzeichen an den Wänden zu sehen, jedoch nichts, was du wirklich entziffern könntest.")
  (if (ereignis 'durchgang-geöffnet)
	  (progn
		(textausgabe "In der Südostecke des Raumes hat sich ein Stück des Bodens verschoben. Dort ist ein Loch im Boden, in dem eine Rutsche ist. Nachdem du sie dir näher angesehen und betastet hast, kommst du zu dem Ergebnis, daß das Metall zu glatt ist - um im Falle eines Falles dort wieder hochklettern zu können - hingegen wäre es wohl eine Leichtigkeit - hinunterzurutschen.")
		(auswahl '(ort-53 ort-55 ort-56) "Es führen zwei Wege aus dem Raum heraus, der eine führt nach Westen (1), der andere nach Norden (2). Du könntest einen von ihnen nehmen - oder aber eine ungewisse Rutschpartie wagen (3)."))
	  (auswahl '(ort-53 ort-55) "Es führen zwei Wege aus dem Raum heraus, der eine führt nach Westen (1), der andere nach Norden (2). Welchen möchtest du einschlagen?")))


(defun ort-55 ()
  (if (zerop (inventar 'rucksack))
	  (progn
		(textausgabe "Du tastest dich durch den Raum, bis du schließlich auf deinen Rucksack stößt. Du nimmst alles wieder an dich und schaltest die Taschenlampe an. Da der Raum außer dem Wasserloch nun nichts mehr zu bieten hat, verläßt du ihn und gehst zurück nach Süden.")
		(inventar 'rucksack 1)
		#'ort-54)
	  (progn
		(textausgabe "Der Raum, in den du gelangst, gleicht eher einer Höhle mit Natursteinen. Es gibt nur einen Weg hinaus, der im Süden, durch den du hineingekommen bist. An seiner Westwand ist ein großes, dunkles Wasserloch.")
		(if (j-oder-n-p "Möchtest du deine Kleidung und deinen Rucksack samt Taschenlampe ablegen und in das dunkle Wasserloch tauchen?")
			(progn
			  (inventar 'rucksack -1)
			  (textausgabe "Vorsichtig steckst du einen Zeh in das Wasser. Es ist eiskalt. Langsam, ganz langsam steigst du immer tiefer rein. Der Boden sinkt soweit ab, das du ab einer gewissen Grenze nicht mehr stehen kannst. Es ist jetzt stockfinster im Raum. Schräg in der Tiefe glaubst du ein Licht wahrzunehmen. Du holst noch einmal tief Luft - und tauchst tief in das Wasser ein. Du tauchst durch eine Art natürlicher Tunnel, dessen Wände sich allerdings glatt anfühlen. An einer Stelle des Tunnels stößt du gegen einen Widerstand, aber der verflüchtigt sich sofort. Vermutlich war es nur ein verirrter Fisch.")
			  (ereignis 'durchgang-geöffnet 't)
			  #'ort-46)
			(progn
			  (textausgabe "Da der Raum außer dem Wasserloch nichts zu bieten hat, verläßt du ihn und gehst zurück nach Süden.")
			  #'ort-54)))))


(defun ort-56 ()
  (let ((raum (second *zug*)))
	(when (eql raum 'ort-54)
	  (textausgabe "Du nimmst deinen Rucksack vom Rücken und ziehst ihn verkehrt herum an, so daß er jetzt vor deinem Bauch ist. Das Seil an deiner Taschenlampe schlingst du fest um deine Hand, damit du sie auf keinen Fall verlieren kannst. Du hast dir die Richtung gemerkt, in der die Rutschaprtie beginnen wird. Schließlich setzt du dich auf den Rand und läßt deine Beine in diese seltsam glatte Metallröhre baumeln. Du atmest tief durch und denkst an Karl Kochs, deinen Großvater. Er hätte dich doch bestimmt niemals dazu verlockt, dich in eine Gefahr zu begeben, oder? Andererseits, wenn du an das Draußen denkst, an diesen Überfall fremder Truppen auf Mönchengladbach, an die Greuel, die sich deinen Augen heute bereits geboten haben, so kannst du froh sein, überhaupt noch am Leben zu sein. Dein Leben war so lange nur ein Kampf, auch noch den nächsten Tag erleben zu dürfen, was sollte dich von einem echten Abenteuer abhalten? Im schlimmsten Fall, wird das jetzt eine ungewöhnliche Todesart, eine Rutschpartie in den Tod.")
	  (textausgabe "Noch einmal holst du Luft, gerade so, als müßtest du tauchen, dann stößt du dich ab. Du rutschst, langsam nur. Vermutlich könntest du jetzt noch mit deinen Sohlen abbremsen. Aber schon jetzt stürst du, mit hinaufklettern, wir das nichts. Und dann plötzlich, wird es steiler, abschüssiger. Du breitest die Arme aus, bildest mit deinem langgezogenen Körper und deinen Elbogen eine halbwegs stabile Haltung. Du wagst es gar nicht mehr, zu versuchen mit Händen oder Schuhsohlen die Röhre zu berühren. Ja, die Röhre, denn es ist eine Röhre, eine Röhre, die vollkommen glatt ist - und dann spürst du, das du dich in einer langen Spirale bewegst, während es immer tiefer geht - und du wirst schneller in der Spirale, sie wird weiter.")
	  (textausgabe "Dachtest du vor Beginn deiner Rutschpartie noch, es wäre nach 2, 3, 4 Sekunden vorbei, so weißt du jetzt, daß du besser in Minuten hättest rechnen sollen. Und dann kommt es zu einer Art Korkenziehereffekt, du verlierst die Übersicht, wo oben und unten ist, dann wird es Spiralförmig - und die Spirale flacht immer weiter ab - es kommt zu einem letzten auf und ab - und dann plötzlich trittst du aus einer Röhre aus und fliegst in einem weiten Bogen - und landest in einem relativ flachen Winkel mitten im Wasser. Jetzt bemerkst du, daß das mit dem Rucksack nicht die wirklich tolle Idee war. Du ruderst - und schaffst es, seichteres Gewässer zu erreichen - und als du mit den Knien den Boden fühlst, hörst du auf zu paddeln und stellst dich auf, dann watest du die letzten Schritte an das Ufer und setzt dich hin.")
	  (textausgabe "Es war nicht wirklich schwierig, diese ganze Rutschpartie durchzustehen, aber noch dreht sich dir die Umgebung ein wenig. Du legst dich zurück und schließt die Augen.")
	  ;; Durch den Schlaf erholt sich der Spieler vollständig
	  (erhöhe 'stärke 24)
	  (erhöhe 'gewandheit 12)
	  (erhöhe 'glück 12)
	  (textausgabe "Du weißt nicht, wieviel Zeit vergangen ist, aber nachdem du nun endlich wach bist, fühlst du dich direkt viel wohler und auch viel ausgeruhter. Deine Kleidung ist nicht mehr nass. Und dein Rucksack liegt neben dir auf dem Boden. Langsam steigt es dir ins Bewußtsein, daß du sehen kannst. Du blickst dich um uns bemerkst, das die seltsamen Wurzeln - oder sind es Pflanzen - die sich an den Wänden langziehen, eine Art Lumineszenz ausstrahlen, in einem Spektrum, die die Höhle in eine Art angenehmes gelbliches Licht tauchen."))
	(rotiere-plattform)
	(textausgabe "Du befindest dich in einer Art Naturhöhle mit einem großen Teich. Das Wasser ist kristallklar. Die weit entfernte Nordwand ist spiegelglatt - nicht jene natürliche Glätte eines gebrochenen Felsens, sondern künstliche Glätte die Rückschlüsse auf eine Bearbeitung läßt. Mitten in dieser befindet sich ein beinahe mannsgroßes Loch. Das ist der Ausgang der Röhre, durch die du hierhin gerutscht bist. Die Höhle ist bewachsen mit einer seltsamen Pflanze oder Wurzel, die luminiszierend ist und ein angenehmes gelbliches Licht ausstrahlt. Das Ufer ist nicht sandig, wie du es von Stränden kennst, dafür liegen dort viele Kieselsteine. Soweit du sonst erkennen kannst, ist die Höhle leer. Im Süden führt ein Weg aus ihr hinaus. Da es hier nichts gibt, was dich hält, drehst du dich um und folgst dem Weg hinaus aus der Höhle.")
	#'ort-59))


(defun ort-57 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-58
	   (textausgabe "Du hast schon das Gefühl, der Gang würde niemals enden. Dann aber scheint er doch an sein Ende zu kommen und macht einen Knick nach links.")
	   (auswahl '(ort-131 ort-58 ort-63) "Möchtest du dem Gang nach links folgen (1) oder zurückgehen (2)? Du kannst natürlich auch nach Geheimgängen suchen (3)"))
	  (ort-63
	   (textausgabe "Der Geheimgang, den du gefunden hast, ist verwirrend. Er ändert immer wieder sporadisch die Richtung, mal mußt du kriechen, dann wieder über Hindernisse klettern. Einmal wenigstens hast du sogar Angst, stecken zu bleiben, doch schließlich gelangst du an sein Ende. In einer schattigen Mulde trittst du aus ihm hervor.")
	   (auswahl '(ort-58 ort-131 ort-63) "Der Gang führt hier weiter geradeaus (1), du kannst aber auch einem Weg nach rechts folgen (2) oder kehrt machen und wieder zurück an deinen Ausgangsort durch den Geheimgang, so du ihn denn wiederfindest (3)."))
	  (ort-131
	   (textausgabe "Der Gang macht an dieser Stelle einen Knick nach rechts.")
	   (auswahl '(ort-58 ort-131 ort-63) "Möchtest du dem Gang nach rechts folgen (1) oder zurückgehen (2)? Du kannst natürlich auch nach Geheimgängen suchen (3)"))
	  (otherwise
	   (auswahl '(ort-58 ort-131 ort-63) "Du kannst dem Tunnel nach Osten folgen (1) oder nach Süden (2) oder die Wände nach Geheimgängen absuchen (3)")))))


(defun ort-58 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-57
	   (textausgabe "Du kommst, nachdem du einem langen Tunnel gefolgt bist, an eine Abbiegung.")
	   (auswahl '(ort-59 ort-64 ort-57 ort-212) "Von hier aus kannst du weiter geradeaus gehen (1) oder der Abbiegung nach rechts folgen (2). Du kannst dich aber natürlich auch umdrehen und zurückgehen (3) oder nach Geheimgängen suchen (4)"))
	  (ort-59
	   (auswahl '(ort-57 ort-64 ort-59 ort-212) "Von hier aus kannst du weiter geradeaus gehen (1) oder der Abbiegung nach links folgen (2). Du kannst dich aber natürlich auch umdrehen und zurückgehen (3) oder nach Geheimgängen suchen (4)"))
	  (ort-64
	   (textausgabe "Der Gang kommt mündet hier in einen anderen Gang, der von rechts nach links verläuft.")
	   (auswahl '(ort-59 ort-57 ort-64 ort-212) "Möchtest du nach rechts gehen (1) oder nach links (2) oder drehst du dich um und gehst zurück (3)? Wenn du magst, kannst du auch nach Geheimwänden suchen (4)")))))


(defun ort-59 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-56
	   (textausgabe "Du folgst dem Weg. Er wird nach kurzer Zeit zu einem behauenen Gang, der in einen Tunnel mündet. Zu deinem Bedauern mußt du feststellen, daß dein Kompass hier verrückt spielt.")
	   (auswahl '(ort-60 ort-58 ort-56 ort212) "Willst du dem Tunnel nach links folgen (1) oder nach rechts (2) oder umdrehen und zurückgehen (3)? Wenn du es für möglich hältst, das hier ein Geheimgang ist, kannst du auch nach diesem suchen (4)"))
	  (ort-58
	   (textausgabe "Du gelangst an eine Abbiegung.")
	   (auswahl '(ort-60 ort-56 ort-58 ort-212) "Möchtest du dem Weg weiter geradeaus folgen (1) oder nach links gehen (2) oder dich umdrehen und zurückgehen (3)? Du kannst gerne auch nach Geheimgängen suchen (4)"))
	  (ort-60
	   (textausgabe "Du gelangst an eine Abbiegung.")
	   (auswahl '(ort-58 ort-56 ort-60 ort-212) "Möchtest du dem Weg weiter geradeaus folgen (1) oder nach rechts gehen (2) oder dich umdrehen und zurückgehen (3)? Du kannst gerne auch nach Geheimgängen suchen (4)")))))


(defun ort-60 ()
  (rotiere-plattform)
  (textausgabe "Der Gang, dem du folgst, scheint endlos lang zu sein.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-59
	   (auswahl '(ort-61 ort-59 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)"))
	  (ort-61
	   (auswahl '(ort-59 ort-61 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)")))))


(defun ort-61 ()
  (rotiere-plattform)
  (textausgabe "Der Gang, dem du folgst, scheint endlos lang zu sein.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-60
	   (if (> (w6) 3)
		   (auswahl '(ort-61 ort-60 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)")
		   (auswahl '(ort-62 ort-60 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)")))
	  (ort-62
	   (if (> (w6) 3)
		   (auswahl '(ort-61 ort-62 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)")
		   (auswahl '(ort-60 ort-62 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)"))))))


(defun ort-62 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-60
	   (if (> (w6) 3)
		   (progn
			 (textausgabe "Der Gang, dem du folgst, scheint endlos lang zu sein. Als du es fast schon nicht mehr für möglich hältst, gelangst du dennoch endlich an sein scheinbares Ende, er macht einen Knick nach rechts.")
			 (auswahl '(ort-135 ort-61 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)"))
		   (progn
			 (textausgabe "Der Gang, dem du folgst, scheint endlos lang zu sein.")
			 (auswahl '(ort-62 ort-61 ort-212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)"))))
	  (ort-133
	   (textausgabe "Der Gang macht hier einen Knick nach links.")
	   (auswahl '(ort-61 ort-135 ort212) "Du kannst entweder weiter dem Gang folgen (1) oder dich umdrehen und zurückgehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)")))))


(defun ort-63 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-57
	   (textausgabe "In den Schatten der Wand findest du einen Geheimgang. Du folgst ihm, doch schon nach einiger Zeit mußt du kriechen. Er windet sich - und mit der Zeit verlierst du die Orientierung, ja du bist dir nicht einmal sicher, ob du zwischendurch vielleicht nicht auf Abzweigungen gestoßen bist. Doch endlich, nach einer schier endlos langen Zeit, vielen Windungen, Kriechgängen und Hindernissen, die du in der Dunkelheit überwinden mußtest, hast du das Ende erreichst. Du schältst dich aus den Schatten und bist in einem erleuchteten Raum.")
	   (auswahl '(ort-136 ort-57) "Du kannst dem Tunnel folgen (1) oder dich umdrehen und versuchen den Rückweg durch den Geheimgang zu finden (2)"))
	  (otherwise
	   (textausgabe "Das war's dann wohl. Der Tunnel endet hier vor einer groben Felswand, in der es vor scharfen Kanten und tiefen Schatten nur so zu wimmeln scheint.")
	   (auswahl '(ort-136 ort-57) "Du kannst den Tunnel zurückgehen (1) oder die Wände hier nach Geheimgängen absuchen (2)")))))


(defun ort-64 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-58
	   (auswahl '(ort-139 ort-65 ort-58 ort-212) "Du kannst weiter geradeaus gehen (1) oder nach links abbiegen (2) oder dich umdrehen und zurückgehen (3). Wenn du magst, kannst du aber auch nach Geheimgängen suchen (4)."))
	  (ort-65
	   (auswahl '(ort-58 ort-139 ort-65 ort-212) "Du kommst an eine Abzweigung, von wo aus du nur nach rechts gehen kannst (1) oder nach links (2). Natürlich kannst du auch kehrt machen (3) oder versuchen, ob du einen Geheimgang finden kannst (4)"))
	  (otherwise
	   (auswahl '(ort-58 ort-65 ort-139 ort-212) "Du kannst weiter geradeaus gehen (1) oder nach rechts abbiegen (2) oder dich umdrehen und zurückgehen (3). Wenn du magst, kannst du aber auch nach Geheimgängen suchen (4).")))))


(defun ort-65 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-64
	   (textausgabe "Du gelangst an eine Abzweigung.")
	   (auswahl '(ort-133 ort-66 ort-64 ort-212) "Möchtest du weiter geradeaus gehen (1) oder der Abzweigung nach rechts folgen (2) oder dich umdrehen und zurückgehen (3)? Du kannst auch, wenn du magst, nach Geheimgängen suchen (4)"))
	  (ort-66
	   (textausgabe "Der Gang mündet an dieser Stelle in einen Tunnel der von links nach rechts führt.")
	   (auswahl '(ort-64 ort-133 ort-66 ort-212) "Möchtest du nach links gehen (1) oder nach rechts (2) oder dich umdrehen und zurückgehen (3)? Du kannst auch, wenn du magst, nach Geheimgängen suchen (4)"))
	  (ort-133
	   (textausgabe "Du gelangst an eine Abzweigung.")
	   (auswahl '(ort-64 ort-66 ort-133 ort-212) "Möchtest du weiter geradeaus gehen (1) oder der Abzweigung nach links folgen (2) oder dich umdrehen und zurückgehen (3)? Du kannst auch, wenn du magst, nach Geheimgängen suchen (4)")))))


(defun ort-66 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  ((ort-65 ort-133)
	   (textausgabe "Du gelangst an eine Abzweigung im Tunnel. Zu deiner großen Verblüffung funktioniert dein Kompass hier wieder."))))
  (auswahl `(ort-65 ,(elt '(ort-74 ort-73 ort-72 ort-71 ort-70 ort-69 ort-68 ort-67) *richtung*) ort-140) "Du kannst dem Tunnel nach Norden (1) oder nach Süden folgen (2). Du kannst aber auch die Abzweigung nach Osten nehmen (3)"))


(defun ort-67 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-67
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-74 ort-68 ort-81 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-74 ort-68 ort-82 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-74 ort-68 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-74 ort-68 ort-87 ort-77 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-74 ort-68 ort-88 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-74 ort-68 ort-80 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl '(ort-74 ort-68 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-74 ort-68 ort-66 ort-67) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-68 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-68
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-67 ort-69 ort-81 ort-68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-67 ort-69 ort-82 ort68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-67 ort-69 ort-68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-67 ort-69 ort-87 ort-77 ort-68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-67 ort-69 ort-88 ort-68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-67 ort-69 ort-80 ort-68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl '(ort-67 ort-69 ort-68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-67 ort-69 ort-66 ort-68) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-69 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-69
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-68 ort-70 ort-81 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-68 ort-70 ort-82 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-68 ort-70 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-68 ort-70 ort-87 ort-77 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-68 ort-70 ort-88 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-68 ort-70 ort-80 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl '(ort-68 ort-70 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-68 ort-70 ort-66 ort-69) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-70 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-70
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-69 ort-71 ort-81 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-69 ort-71 ort-82 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-69 ort-71 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-69 ort-71 ort-87 ort-77 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-69 ort-71 ort-88 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-69 ort-71 ort-80 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl '(ort-69 ort-71 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-69 ort-71 ort-66 ort-70) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-71 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-71
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-70 ort-72 ort-81 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-70 ort-72 ort-82 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-70 ort-72 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-70 ort-72 ort-87 ort-77 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-70 ort-72 ort-88 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-70 ort-72 ort-80 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl '(ort-70 ort-72 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-70 ort-72 ort-66 ort-71) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-72 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-72
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-71 ort-73 ort-81 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-71 ort-73 ort-82 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-71 ort-73 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-71 ort-73 ort-87 ort-77 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-71 ort-73 ort-88 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-71 ort-73 ort-80 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl '(ort-71 ort-73 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-71 ort-73 ort-66 ort-72) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-73 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-73
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-72 ort-74 ort-81 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-72 ort-74 ort-82 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-72 ort-74 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-72 ort-74 ort-87 ort-77 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-72 ort-74 ort-88 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-72 ort-74 ort-80 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl '(ort-72 ort-74 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-72 ort-74 ort-66 ort-73) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-74 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(case raum
	  (ort-95
	   (textausgabe "Du entdeckst im Schatten hinter einem scharfkantigen Vorsprung einen für das Auge beinahe unsichtbaren Tunnel. Du mußt dich auf Hände und Knie hinablassen und kriechst hinein. So gut es dir möglich ist, kriechst du durch einen stockdunklen Gang. Du bist dir nicht einmal sicher, in welche Richtung du krabbelst. Mehr als nur einmal hast du das Gefühl, mehr als einen Weg zur Auswahl zu haben, doch du kriechst intuitiv der Nase nach, bis du endlich, den Tunnel verlassen kannst."))
	    (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
	  (ort-74
	   (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case *richtung*
	(1
	 (auswahl '(ort-73 ort-67 ort-81 ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(2
	 (auswahl '(ort-73 ort-67 ort-82 ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(3
	 (auswahl '(ort-73 ort-67 ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(4
	 (auswahl '(ort-73 ort-67 ort-87 ort-77 ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3), den bläulich erleuchteten Ausgang (4) oder stehenbleiben und warten (5)"))
	(5
	 (auswahl '(ort-73 ort-67 ort-88 ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(6
	 (auswahl '(ort-73 ort-67 ort-80 ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
	(7
	 (auswahl `(ort-73 ort-67 (if (> (w6) 3) ort-95 ort98) ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem orange erleuchteten Ausgang oder stehenbleiben und warten (4)"))
	(otherwise
	 (auswahl '(ort-73 ort-67 ort-66 ort-74) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))))


(defun ort-75 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-75
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case (rem *richtung* 4)
	(1
	 (auswahl '(ort-76 ort-78 ort-75) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(2
	 (auswahl '(ort-76 ort-78 ort-75) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(3
	 (auswahl '(ort-76 ort-78 ort-75) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-76 ort-78 ort-79) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder dem gelblich erleuchteten Ausgang (3)"))))


(defun ort-76 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-76
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case (rem *richtung* 4)
	(1
	 (auswahl '(ort-75 ort-77 ort-76) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(2
	 (auswahl '(ort-75 ort-77 ort-76) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(3
	 (auswahl '(ort-75 ort-77 ort-79) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder dem gelblich erleuchteten Ausgang (3)"))
	(otherwise
	 (auswahl '(ort-75 ort-77 ort-76) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))))


(defun ort-77 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-77
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case (rem *richtung* 4)
	(1
	 (auswahl '(ort-78 ort-76 ort-77) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(2
	 (auswahl '(ort-78 ort-76 ort-79) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder dem gelblich erleuchteten Ausgang (3)"))
	(3
	 (auswahl '(ort-78 ort-76 ort-77) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(otherwise
	 (auswahl '(ort-78 ort-76 ort-77) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))))


(defun ort-78 ()
  (rotiere-plattform)
  (textausgabe "Du befindest dich in einem behauenen Raum, der Ausgänge in verschiedenfarbige Tunnel bietet.")
  (let ((raum (second *zug*)))
	(case raum
	  (ort-78
	   (textausgabe "Plötzlich verspürst du, wie der Raum sich bewegt."))))
  (case (rem *richtung* 4)
	(1
	 (auswahl '(ort-77 ort-75 ort-78) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(2
	 (auswahl '(ort-77 ort-75 ort-78) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))
	(3
	 (auswahl '(ort-77 ort-75 ort-79) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder dem gelblich erleuchteten Ausgang (3)"))
	(otherwise
	 (auswahl '(ort-77 ort-75 ort-78) "Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2) oder stehenbleiben und warten (3)"))))


(defun ort-79 ()
  (rotiere-plattform)
  (textausgabe "Der Raum in dem du dich befindest hat nur einen Ausgang, der im Norden liegt. Er besteht ganz aus Rosenmarmor, doch ist seine Oberfläche überall seltsam aufgeraut und macht es schwierig, eine Stelle genauer zu betrachten. In der Mitte des Raumes befindet sich ein großer Sockel, in dessen Mitte ein Becken mit kristallklarem Wasser ist.")
  (when (and (zerop (inventar 'schlüssel-66)) (ja-oder-nein-p "Willst du deine Hände in das Becken tauchen?"))
	(inventar 'schlüssel-66 1))
  (textausgabe "Du läßt die Hände in das Becken gleiten. Das Wasser fühlt sich seltsam warm an, so daß es beinahe deine Sinne betäubt. Du gleitest mit deinen Händen über die Wände des Beckens und plötzlich fühlst du, daß da ein Gegenstand ist. Du tastest danach und bekommst ihn zu greifen. Als du die Hände aus dem Becken herausholst, hälst du deinen Kristallschlüssel in der Hand, der vollkommen durchsichtig ist. Die Zinken scheinen die Zahl 66 zu formen.")
  (auswahl `((elt '(ort-78 ort-77 ort-76 ort-75) (rem *richtung* 4)) ort-152 ort-79) "Willst du den Raum wieder verlassen (1)? Du kannst auch nach Geheimtüren suchen (2) oder warten (3)"))


(defun ort-80 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(when (eql raum 'ort-143)
	  (textausgabe "Kaum daß du den Wasserfall durchschritten hast, bemerkst du eine Veränderung deiner Kompaßnadel. Sie hört auf mit ihrem Tanz und zeigt wieder konstant nach Norden.")))
  (when (zerop (inventar 'schlüssel-99))
	(textausgabe "Du siehst etwas funkelndes im Wasser.")
	(when (ja-oder-nein-p "Möchtest du im Wasser nach dem funkelnden etwas tauchen?")
	  (inventar 'schlüssel-99 1)
	  (textausgabe "Es ist ein kupferfarbener Schlüssel, dessen Zinken so geformt sind, als würden sie die Zahl 99 ergeben.")))
  (textausgabe "Der Raum, in dem du dich jetzt befindet, scheint eine große natürliche Höhle zu sein. Sie ist bewachsen von exotischen Pflanzen. Grün ist hier eine weniger dominierende Farbe, als es sie an der Erdoberfläche zu sein scheint. Der Raum ist viel stickiger, als die Tunnel, durch die du dich seit einiger Zeit bewegst. Im nördlichen Teil der Höhle befindet sich ein großer Teich, in den ein Wasserfall hineinstürzt.")
  (auswahl `(ort-143 ,(elt '(ort-72 ort-71 ort-70 ort-69 ort-68 ort-67 ort-74 ort-73) *richtung*)) "Willst du in den Teich hineinwaten und dich in die herabfallenden Wasser des Wasserfalls stellen (1) oder die Höhle durch ihren Ausgang im Osten verlassen (2)?"))


(defun ort-81 ()
  (rotiere-plattform)
  (textausgabe "Der Raum, in dem du dich jetzt befindest, ist aus einem einzigen großen Smaragd heraus geschnitten. An einer Wand ist ein Symbol sichtbar.")
  (textausgabe "*###*")
  (textausgabe "    #")
  (textausgabe "    #")
  (textausgabe "^###*")
  (textausgabe "#")
  (textausgabe "#")
  (textausgabe "*##n_")
  (when (zerop (ereignis 'rätsel-1))
	(textausgabe "Eine Stimme ertönt plötzlich und stellt dir eine Frage...")
	(löse-rätsel 'rätsel-1 '(bett) "Verfertigt ist's vor langer Zeit,~%doch mehrenteils gemacht erst heut;~%sehr schätzbar ist es seinem Herrn,~%und dennoch hütet's niemand gern."))
  (auswahl `(,(elt '(ort-67 ort-74 ort-73 ort-72 ort-71 ort-70 ort-69 ort-68) *richtung*))"Willst du den Tunnel nach Westen nehmen (1) oder den nach Südosten(2)?"))


(defun ort-83 ()
  (rotiere-plattform)
  (textausgabe "Der Raum, in dem du dich jetzt befindest, ist aus einem einzigen großen Rubin heraus geschnitten. An der Wand befindet sich ein Symbol.")
  (textausgabe "*####_")
  (textausgabe " #")
  (textausgabe "#")
  (textausgabe "#")
  (textausgabe " #")
  (textausgabe " #")
  (textausgabe "  V")
  (when (zerop (ereignis 'rätsel-3))
	(textausgabe "Eine Stimme ertönt plötzlich und stellt dir eine Frage...")
	(löse-rätsel 'rätsel-3 '(bauch) "Rat, wenn du kannst!~%Es nennen einen Wanst~%fünf Zeichen dir,~%und auch die letzten vier."))
  (auswahl '(ort-82 ort-212) "Du kannst den Weg zurück nach Südwesten gehen (1) oder die Wände nach Geheimgängen absuchen (2)"))


(defun ort-84 ()
  (rotiere-plattform)
  (textausgabe "Der Raum, in dem du dich jetzt befindest, ist aus einem einzigen großen Lapislazuli heraus geschnitten. An der Wand siehst du ein Symbol.")
  (textausgabe "*###*")
  (textausgabe " # #")
  (textausgabe "#   #")
  (textausgabe "#   #")
  (textausgabe "#   #")
  (textausgabe " # #")
  (textausgabe " \\ /")
  (when (zerop (ereignis 'rätsel-4))
	(textausgabe "Eine Stimme ertönt plötzlich und stellt dir eine Frage...")
	(löse-rätsel 'rätsel-4 '(zeit) "Etwas, das alles und jeden verschlingt:~%Baum, der rauscht, Vogel, der singt,~%frisst Eisen, zermalmt den härtesten Stein,~%zerbeißt jedes Schwert, zerbricht jeden Schrein,~%Schlägt Könige nieder, schleift ihren Palast,~%trägt mächtigen Fels fort als leicht Last."))
  (auswahl '(ort-82 ort-212) "Du kannst den Weg zurück nach Westen gehen (1) oder die Wände nach Geheimgängen absuchen (2)"))


(defun ort-85 ()
  (rotiere-plattform)
  (textausgabe "Du stehst in einem Raum, der aus eine einzigen, riesigen Tigerauge geschnitzt zu sein scheint. Du siehst ein Symbol an der Wand.")
  (textausgabe "*##*")
  (textausgabe " ##")
  (textausgabe " ##")
  (textausgabe "  #")
  (textausgabe "  ## /")
  (textausgabe "  # V")
  (textausgabe "  /")
  (textausgabe " /")
  (when (zerop (ereignis 'rätsel-5))
	(textausgabe "Eine Stimme ertönt plötzlich und stellt dir eine Frage...")
	(löse-rätsel 'rätsel-5 '(brezel bretzel brezen brezn) "Der arme Tropf~%hat keinen Kopf;~%das arme Weib~%hat keinen Leib,~%die arme Kleine~%hat keine Beine.~%Sie ist ein langer Darm,~%doch schlingt sie einen Arm~%bedächtig in den anderen ein.~%Was mag das für~%ein Weiblein sein?"))
  (auswahl '(ort-82 ort-212) "Du kannst den Weg zurück nach Nordwesten gehen (1) oder die Wände nach Geheimgängen absuchen (2)"))


(defun ort-86 ()
  (rotiere-plattform)
  (textausgabe "Der Raum, in dem du dich jetzt befindest, ist aus einem einzigen großen Türkis heraus geschnitten.")
  (textausgabe "  #")
  (textausgabe "   #")
  (textausgabe "##  #")
  (textausgabe " ###")
  (textausgabe " ##")
  (textausgabe "  #")
  (textausgabe "  #")
  (textausgabe " \\/")
  vordergrundfarbe(FARBE_WEISS)
  (when (zerop (ereignis 'rätsel-2))
	(textausgabe "Eine Stimme ertönt plötzlich und stellt dir eine Frage...")
	(löse-rätsel 'rätsel-2 '(handschuh) "Ich hab' ein Ding im Sinn,~%wohl lieben es die Mädchen traut,~%es liegt um ihre zarte Haut,~%doch stecken Nägel drin."))
  (auswahl '(ort-82 ort-212) "Du kannst den Weg zurück nach Nordosten gehen (1) oder die Wände nach Geheimgängen absuchen (2)"))


(defun ort-87 ()
  (rotiere-plattform)
  (textausgabe "Du betrittst eine Naturhöhle. Das Licht hier ist intensiver, als in den meisten Gängen, die du bisher betreten hast. Büsche wachsen hier, voller Beeren, die du nicht kennst, deren Farbe aber ein deutliches Signalrot aufweisen, was kein gutes Zeichen in deiner Erinnerung ist. In deinen Ohren klingt es, als würde ein Bach oder ein Fluß in der Nähe rauschen, doch sehen kannst du von hier aus keinen. Die Höhle hat Ausgänge in drei Richtungen: einen behauenen Tunnel im Norden, eine Loch in der Ostwand und einen natürlichen Tunnel im Süden.")
  (auswahl `(,(elt '(ort-70 ort-69 ort-68 ort-67 ort-74 ort-73 ort-72 ort-71) *richtung*) ort-151 ort-146) "Du kannst dem behauenen Tunnel nach Norden folgen (1), ebenso kannst du den Südtunnel nehmen (2), oder dich dem Weg hinein in das große Loch in der Ostwand anvertrauen (3)"))


(defun ort-88 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(unless (member raum '(ort-88 ort-89 ort-145))
	  (textausgabe "Nach einiger Zeit verändert sich der Tunnel und wird zu einem Stollen. Der Ort wo du jetzt erinnert mit seinen Balkenkonstruktionen sehr an ein Bergwerk. Die Beleuchtung ist jetzt viel spärlicher.")))
  (textausgabe "Du befindest dich an einer Kreuzung. Ein Tunnel führt von Norden nach Westen, während ein weiterer Stollen in Richtung Westen führt. Das Wurzelwerk in den Wänden schimmert weniger intensiv, als du es bisher gewohnt bist. Ein Schienenstrang führt an der Wand des nach Westen laufenden Gangs entlang.")
  (auswahl `(elt '(ort-71 ort-70 ort-69 ort-68 ort-67 ort-74 ort-73 ort-72) *richtung*) "Du kannst dem Tunnel nach Norden (1) oder nach Süden folgen (2), odern den Weg nach Westen nehmen (3)"))


(defun ort-89 ()
  (rotiere-plattform)
  (textausgabe "Du betrittst einen Raum, dessen Wände abgeschliffen und vollkommen Glatt sind. Nichts, nicht einmal Staub oder Schmutz, sind in diesem Raum sichtbar.")
  (when (> (w6) 3 )
	(textausgabe "Dich beschleicht ein mulmiges Gefühl."))
  (auswahl '(ort-88 ort-90 ort-212) "Du kannst den Raum durch den Ausgang im Norden verlassen (1) oder durch jenen im Süden (2). Wenn du tatsächlich glaubst, du könntest vielleicht einen Geheimgang oder eine Geheimtüre finden, so kannst du stattdessen auch die Wände absuchen (3)"))


(defun ort-90 ()
  (rotiere-plattform)
  (textausgabe "Der Raum den du betrittst verschlägt dir die Sprache. Er ist über und über voll mit Schnitzereien und Verzierungen im Fels und wie es aussieht, wurde einem riesigen Wandbild mit Hilfe von Edelsteinen Farbei eingehaucht. Es läßt Meisterwerke der Malerei neben sich zu einem Nichts verblassen. Wenn kein Erdbeben dieses Kunstwerk vernichtet, wird man es vermutlich noch in Jahrmillionen betrachten können. In der Ostwand ist ein beeindruckender, gigantischer Torrahmen.")
  (if (plusp (ergeignis 'gitter-offen))
	  (progn
		(textausgabe "Das Tor steht offen - doch kann man ganz obem am Türrahmen die feinen Verfärbungen des Gitters im Gestein sehen, das nur darauf zu lauern scheint, auf dich herabzustürzen, wenn du versuchst hindurchzugehen.")
		(auswahl 'ort-89 ort-91 ort-212() "Du kannst dem Weg nach Norden einschlagen (1) oder das gigantische Tor nach Osten durchschreiten (2). Wenn du es für nötig hältst, kannst du jedoch auch die Wände nach Geheimgängen absuchen (3)"))
	  (progn
		(textausgabe "Zwar steht das gigantische Tor offen, seine Türen sind weit aufgeschwungen, auch wenn du dir nicht vorstellen kannst, das irgendwer diese Flügel auch nur einen einzigen Millimeter weit bewegen könnte, dennoch kannst du nicht hindurch, den ein riesiges Gitter ist herabgelassen worden - und erlaubt nur den kleinsten Lebewesen ein Durchkommen. Klein - in der Größenordnungen von Katzen, Hunden oder Nagern. Du würdest es hingegen nicht einmal schaffen, deinen Kopf hindurchzuzwängen.")
		(auswahl '(ort-89 ort-212) "Du kannst den Weg zurück nach Norden gehen (1) oder die Wände nach Geheimgängen absuchen (2)"))))


(defun ort-91 ()
  (rotiere-plattform)
  (textausgabe "Der Raum den du betrittst verschlägt dir die Sprache. Seine Wände scheinen von innen heraus zu leuchten, obwohl sie aus Metall zu scheinen sein. In die Wände hinein wurden Bilder geschnitzt, die durch das Leuchten der Wände irgendwie den Eindruck erwecken, als würden sie leben. An der Decke ist ein riesiges Bild dargestellt, dessen Sinn du nicht genau erkennen kannst, denn es ist aus Edelsteinen erstellt und sein Anblickt schmerzt geradezu in deinen Augen, so daß dir die Tränen kommen. In der Westwand ist ein beeindruckender, gigantischer Torrahmen.")
  (if (plusp (ereignis 'gitter-offen ))
	  (progn
		(textausgabe "Das Tor steht offen - doch kann man ganz obem am Türrahmen die feinen Verfärbungen des Gitters im Gestein sehen, das nur darauf zu lauern scheint, auf dich herabzustürzen, wenn du versuchst hindurchzugehen.")
		(auswahl '(ort-90 ort-92 ort-212) "Du kannst den Weg durch das Tor nach Westen nehmen (1) oder den Ausgang nach Osten nehmen (2). Wenn du es für nötig hältst, kannst du jedoch auch die Wände nach Geheimgängen absuchen (3)"))
	  (progn
		(textausgabe "Zwar steht das gigantische Tor offen, seine Türen sind weit aufgeschwungen, auch wenn du dir nicht vorstellen kannst, das irgendwer diese Flügel auch nur einen einzigen Millimeter weit bewegen könnte, dennoch kannst du nicht hindurch, den ein riesiges Gitter ist herabgelassen worden - und erlaubt nur den kleinsten Lebewesen ein Durchkommen. Klein - in der Größenordnungen von Katzen, Hunden oder Nagern. Du würdest es hingegen nicht einmal schaffen, deinen Kopf hindurchzuzwängen.")
		(auswahl '(ort-92 ort-212) "Du kannst den Weg zurück nach Osten gehen (1) oder die Wände nach Geheimgängen absuchen (2)"))))


(defun ort-92 ()
  (rotiere-plattform)
  (textausgabe "Du stehst in einem perfekt aus dem Felsen geschnitzten Tunnelstück, das von Westen nach Osten führt.")
  (auswahl '(ort-91 ort-93 ort-212) "Du kannst dem Tunnel nach Westen folgen (1) oder nach Osten (2) oder die Wände nach Geheimgängen absuchen (3)"))


(defun ort-93 ()
  (rotiere-plattform)
  (textausgabe "Du stehst an einer Abbiegugn des Tunnels, die von Norden nach Westen verläuft.")
  (auswahl '(ort-94 ort-92 ort-212) "Du kannst dem Weg nach Norden folgen (1) oder nach Westen (2) oder die Wände nach Geheimgängen absuchen (3)"))


(defun ort-94 ()
  (rotiere-plattform)
  (textausgabe "Du hast das Ende des aus dem Felsen geschnitzten Tunnels erreicht. Die Wände hier sind mit Motiven eines Drachens dargestellt. Er ist von roter Farbe - und wie es aussieht, hat er einen Hort, den er bewacht. Viele Schriftzeichen bedecken die Wände in langen Reihen. Du weißt nicht warum, aber du kannst dich des unbestimmten Gefühls nicht erwehren, gerade in einer Art Grabkammer zu stehen, auch wenn du kein einziges der Schriftzeichen an der Wand auch nur im entferntesten deuten kannst.")
  ;; Die Chance den richtigen Ausgang zu finden ist 
  (auswahl `(ort-93 (if (> (w6) 4) ort-142 ort-95)) "Dir steht nur der Weg zurück durch den Felstunnel offen (1). Du kannst aber selbstverständlich versuchen, die Wände nach Geheimgängen abzutasten (2)."))


(defun ort-95 ()
  (rotiere-plattform)
  (let ((raum (second *zug*)))
	(unless (member raum '(ort-74 ort-97 ort-98 ort-99 ort-142))
	  (textausgabe "Du entdeckst einen Geheimgang und kriechst in ihn hinein. Nur mühsam, auf Knien und Handflächen, robbst du durch den engen Tunnel. Mermalst drohst du, die Orientierung zu verlieren, doch schließlich hat die Dunkelheit und mit ihr die Orientierungslosikeit ein Ende. Du gelangst in einen hell erleuchteten Raum. Eigentlich, wenn man den geheimen Tunnel nicht bemerkt, ist der Raum eine Art Sackgasse. Aus ihm führt nur ein Gang hinaus.")
	  (auswahl '(ort-134 ort-74) "Du kannst dem Gang hinaus folgen (1) oder dich umdrehen und versuchen, den Weg durch den Geheimgang zurückzufinden (2)")))
  (textausgabe "Du gelangst an das Ende des Ganges. Enttäuscht mußt du feststellen, das hier nur unbehauene Wände und sonst rein gar nichts ist.")
  (auswahl '(ort-134 ort-74) "Willst du unverrichteter Dinge umkehren (1) oder die Wände nach Geheimgängen absuchen (2)?"))


(defun ort-96 ()
  (rotiere-plattform)
  (when (= (person 'arianna) 99)
	(zweisamkeit 1)
	(textausgabe "Als du das erste Mal diesen Ort betreten hast, da war er für dich nur eine Höhle, in der ein Lavabecken stand - und ein seltsamer Metalklumpen, der die eigenartig glatt erschien. Aber jetzt weißt du, was er in Wahrheit darstellt. Das hier ist eine echte Zwergenschmiede, eine Schmiede, in der im heißesten aller Feuer, der Bund für die Ewigkeit besiegelt wird. Alle Zwerge aus Dwellmerdan tummeln sich hier in der Schmiede. Du bahnst dir unter freundschaftlichen Schulterklapsen deinen Weg durch die Meute, nach vorne, wo der Dorfälteste und der Schmied dich bereits erwarten. Und dann wird es leise, etwas, was nur selten bei Zwergen vorkommt - und Arianna wird in Begleitung ihrer Mutter und ihres Vaters zum Amboß geführt. Lansam, ruhig und gemessenen Schrittes nähern sich die drei, während ein Chor aus Sackpfeifen und Blechbläsern die zwergische Hochtzeitsweise anstimmt.~%Und plötzlich stehen Arianna und ihre Eltern nur noch einen Schritt entfernt vor dir. Das Gesicht ihres Vaters ist Rot vor Zorn. Er holt aus - und schlägt dir mit aller Wut ins Gesicht. Du knickst ein, doch als du aufstehst, tust du es im gleich. Mit all der Kraft, die die Liebe zu Arianna dir verleiht, schlägst du zu - und du hast Glück. Du triffst ihren Vater nicht nur, du fällst ihn wie einen Baum. Er knickt nicht einmal in den Beinen ein, sondern kippt unter dem Gröhlen der Zwerge hinten über. Du Mutter und ein paar herbeieilende Dwellmer packen ihn am Kragen und ziehen ihn in die Reihe der Feiernden, die bereits damit begonnen haben, Met aus ihren Gürtelschläuchen zu saufen. Als nächstes legen Arianna und du eure Oberbekleidung ab. Mit nackten Oberkörpern steht ihr voreinander. Zwei Zwerge legen euch mit Zangen die eisernen Bande um die Handgelenke. Der Schmerz ist so überwältigend, daß dir Tränen in die Augen schießen. Und schon hämmert Arianna auf dein Armband ein, verschließt es mit kräftigen Hammerschlägen, während die Zwerge dazu ihr Lied des ewigen Bundes anstimmen. Diesmal klingt es viel volltönender, da noch keiner der Zwerge zu besoffen zum singen ist.~%Nachdem du den Schock des Schmerzes überwunden hast - und Arianna dich noch einmal flüsternd daran erinnert hat, beginnst auch du damit, mit deiner freien Hand und einem Schmiedehammer auf ihr Band einzuhämmer und verschließt es. Hinterher kannst du gar nicht mehr sagen, wie lange es gedauert hat, doch als ihr endlich fertig wart mit dem Hämmern, da leuchteten Runen in den Armbändern für Bruchteile einer Sekunde auf - und besiegelten so für immer euer Bündnis. Jetzt stürmen auch die anderen Zwerge auf euch ein, reichen euch die Schläuche voller Met und saufen mit euch um die Wette."))
  (auswahl '(ort-142 ort-97) "Du kannst den Weg entweder zurück nach Norden gehen (1) oder die Wände nach Geheimgängen absuchen (2)"))


(defun ort-97 ()
	(rotiere-plattform)
	if( raum == 96 || raum == 98 ) (textausgabe "Du bewegst dich für eine lange Zeit durch einen dunklen Gang. Er ist eng und an einigen Stellen mußt du sogar auf allen Vieren weiterkrabbeln. Es ist stockfinster und du verlierst vollkommen die Orientierung. Mehr als nur einmal hast du das Gefühl, in mehr als eine Richtung weitergehen zu können. Schließlich gelingt es dir, einen Ausgang zu finden. Du wirst geblendet von dem Licht und brauchst einige Momente um dich zurechtzufinden, erst dann kannst du erkennen, wo du bist.")
	raum = 97;
	(textausgabe "Du befindest dich in einem Wald aus Pilzen der verschiedensten Größe. Weit entfernt in südlicher Richtung kannst du einen Pilz gigantischer Größe ausmachen, der alle anderen überragt. Helles Licht entströmt den Lamellen unter seinem gigantischen Pilzhut, er scheint die Lichtquelle dieser Höhle zu sein.")
	(auswahl '() "Du kannst dich entweder in westliche Richtung fortbewegen (1) oder nach Süden (2). Wenn du etwas Zeit investierst, findest du vielleicht auch noch einen Weg in eine weitere Richtung (3)", 3, ort148, ort154, (wuerfel(6) > 3) ? ort96 : ort95)
)

(defun ort-98 ()
	// Hier gibt es Drachen!
	static charakter_s drache = { "Drache", 20, 20, 20, 20 );
	(rotiere-plattform)
	if ( raum == 99 || raum == 74 ) (textausgabe "Du findest eine Spalte im Schatten der Wand. Sie ist äußerst schmal, aber es gelingt dir, dich hineinzuquetschen. So kriechst du auf Händen und Füßen weiter - und plötzlich wird dir klar, das nach all den kleinen Windungen, du dich ja gar nicht umdrehen kannst. Zeitweise mußt du sogar flach auf den Boden gepreßt, die Arme vorausgestreckt, weiterkriechen um nicht steckenzubleiben. Die Minuten werden für dich zu kleinen Ewigkeiten, die Sekunden dehnen sich zu Stunden aus. Und endlich - sind deine Arme frei, du kriechst schneller, kletterst aus dem beengenden Gang heraus und richtest dich auf, wobei deine Glieder heftig maulend über die vorhergehende Position reagieren. Du reckst und streckst dich, so gut es eben geht. Es ist immer noch so dunkel wie in dem endlosen Gang um dich herum, aber du spürst trotzdem eine Veränderung. Es fühlt sich an, als wäre die Luft wärmer geworden. Aber vermutlich ist daß nur Einbildung. Tatsächlich war der Kriechgang anstrengend und du bist ganz schön ins Schwitzen gekommen.")
	if ( raum == 162 ) (textausgabe "Während du dich weiter durch den stockdunklen Gang nach Westen tastest, spürst du eine Veränderung in der der Umgebung. Die Luft fühlt sich wärmer an. Sie riecht anders, verbrauchter. Und die Klänge der Hall deiner Schritte, alles klingt, als wäre jetzt mehr Raum da.")
	raum = 98;
	// Zuerst einmal mitteilen, wie's hier aussieht, also - drachentechnisch ...
	if ( drachetot ) (textausgabe "Der Leichnam des Drachen glüht von innen heraus und läßt die Wände um dich herum erstrahlen.")
	else if ( dracheverletzt ) (textausgabe "Eine Atemflamme des Drachens erfaßt die Decke. Phosphoriszierende Steine in den Wänden reagieren auf den blendenden Feuerstrahl, während deine Augen wieder tränen. Der Drache steht da. Sein Gesichtsausdruck hat sich nicht verändert, aber dort, wo dein Pickel sein Auge trag, ist jetzt nur eine blutige Höhle. Du stehst so günstig, das er dich noch nicht gesehen hat. Allerdings bläht er seine Nüstern auf. Er scheint dich riechen zu können.")
	else (textausgabe "In der Dunkelheit erscheint plötzlich ein Auge, dessen pupille geschlitzt ist. Die Iris erscheint in rötlichem Feuer zu strahlen. Das Auge ist groß, sehr groß - größe als deine Handfläche. Und als es sich zur Seite wendet, erscheint ein zweites. Und das ungute Gefühl überkommt dich, das es mit den beiden Augen in der Dunkelheit nichts Gutes hat. Dann erheben sich die Augen, scheinen immer weiter nach oben zu schweben. Du hörst ein Geräusch, als würde ein Elefant sich auf einen heftigen Niesser vorbereiten - und plötzlich schießt eine Lichtsäule gegen die Decke. Es riecht verbrannt. Du mußt deine Augen schützen, fühlst dich geblendet. Tränen laufen aus ihnen heraus. Du glaubst ein Schnauben wahrzunehmen.")
	(textausgabe "Die Wände leuchten in einem fahlen rot, das von den phosphoriszierenden Adern darin hervorgeht.")
	// Ich will ja nicht den Spieler absichtlich töten
	if ( !dracheverletzt && (objekt[adamantpickel] < 1) ) {
		if(objekt[patrone] > 0) (textausgabe "Du schießt einmal beherzt auf den Drachen, bist dir allerdings nicht sicher, das er es wahrgenommen hat. Vermutlich hatte die Patrone noch etwas weniger Wirkung, als ein Watteball, der gegen einen Baum stößt.")
		(textausgabe "Du nutzt die Gelegenheit, als der Drache erneut Luft holt, um die Beine in die Hand zu nehmen und wie von der Tarantel gestochen in den dunklen Gang hineinzulaufen. Mit ein wenig Glück, überlebst du so vielleicht.")
		ort162()
	)
	if ( objekt[adamantpickel] > 0 && tdg(&spieler) ) (textausgabe "Als der Kopf des Drachens nah genug an dich herangekommen ist, holst du mit deiner Spitzhacke aus und rammst sie ihm mit voller Wucht ins Auge. Es spritzt - und sein Kopf ruckt sofort weg von dir. Die scheußlichen Töne, die er jetzt von sich gibt, sind vermutlich sein Wehklagen.")
	else (textausgabe "Als der Koopf des Drachens nah genug an dich herangekommen ist, holst du mit deiner Spitzhacke aus und haust ins Leere. Der Drache hat blitzschnell reagiert und den Kopf zurückgezogen.")
	if ( !drachetot && (ja-oder-nein-p "Willst du den günstigen Moment nutzen, die Beine in die Hand nehmen - und so schnell du nur kannst von hier fliehen?") ) ort162()
	if ( !drachetot && !dracheverletzt ) {
		(textausgabe "Der Drache dreht sich blitzschnell um die eigene Achse. Du siehst noch, wie sein Schwanz heranrast, dann wirst du auch schon von ihm getroffen. Der Schmerz ist fürchterlich. Im weiten Bogen fliegst du rücklinks in die Dunkelheit hinein.")
		// schwerer Treffer, eventuell tödlich
		staerkesteigerung(-5,0)
		if ( spieler.staerke <= 0 ) beenden(FARBE_ROT, EXIT_SUCCESS, "Du hörst es laut krachen, als du gegen die Felswand schmetterst. Alle Knochen tun dir weh. Am liebsten würdest du jetzt stöhen, aber du fühlst dich viel zu kraftlos. Dann siehst du eine Wand aus Feuer, wie sie durch den Tunnel auf dich zuströmt. Deine Augen tränen und alles wird Schwarz, bis auf den beißenden, brennenden Schmerz, der erst nachläßt, als du dein Leben losläßt. Die Begegnung mit dem Drachen hat dein Leben beENDEt.")
	)
	if ( dracheverletzt && !drachetot ) {
		drache.gewandheit = 16;
		drache.staerke = 15;
	)
	if ( !drachetot ) { // Der Drache lebt
		if ( !kampf(&spieler, &drache, 1, false, ort162) ) beenden(FARBE_ROT, EXIT_SUCCESS, "Du hörst es laut krachen, als du gegen die Felswand schmetterst. Alle Knochen tun dir weh. Am liebsten würdest du jetzt stöhen, aber du fühlst dich viel zu kraftlos. Dann siehst du eine Wand aus Feuer, wie sie durch den Tunnel auf dich zuströmt. Deine Augen tränen und alles wird Schwarz, bis auf den beißenden, brennenden Schmerz, der erst nachläßt, als du dein Leben losläßt. Die Begegnung mit dem Drachen hat dein Leben beENDEt.")
		(textausgabe "Ein Ruck geht durch den Körper des Drachen, ein letztes Aufbäumen, dann sinkt er tot zusammen. Sein Körper sieht aus, als würde er glühen. Vielleicht verbrennt er ja innerlich, du weißt es nicht. Das hier war der erste Drache, den du in deinem Leben gesehen hast. Und du dachtest immer, Drachen würden in das Reich der Märchen gehören. Die Legende von der Unverwundbarkeit durch Drachenblut jedenfalls hast du wiederlegt. Dein linker Arm ist nur so besudelt von Drachenblut, trotzdem hast du lange Schnittwunden darauf.~%Plötzlich mußt du lachen, als du bemerkst, das jemand ein Wort auf die Unterseite des Drachens gemalt hat: \"Bauch\"~%Ein ganz klares Zeichen dafür, das du nicht der erste bist, der sich hier in die Höhle des Drachens gewagt hat.")
		// Ich denke, an der Legende ist doch ein Körnchen Wahrheit
		staerkesteigerung(0,1)
	)
	(auswahl '() "Du kannst jetzt dem Tunnel weiter nach Westen folgen (1) oder nach Osten gehen (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort161, ort162, (wuerfel(6) > 3) ? ort99 : ort95)
)

(defun ort-99 ()
	(rotiere-plattform)
	if ( raum == 98 ) (textausgabe "Du findest eine Spalte in der Wand und nutzt die Gunst des Augenblicks, so schnell du kannst auf Händen und Knien hineinzurobben. Von Panik und Angst getrieben kriechst du weiter durch einen endlos lang erscheinenden Stollen. Manchmal musst du dich sogar auf den Boden pressen und dich liegend voranschieben. Deine Angst ist groß, stecken zu bleiben. Dein Herz ist immer noch wie wild am Pochen, als der Kriechgang, durch den du dich seit Minuten gequetscht hast, sein Ende findet. Glücklich darüber, endlich wieder mehr Platz um dich herum zu haben, krabbelst du aus dem Loch heraus und richtest dich auf.")
	raum = 99;
	(textausgabe "Der Stollen fällt hier stärker von Norden nach Süden ab. Ein paar Schienen laufen an der Ostwand entlang. Das Wurzelwerk breitet sich nur sehr spärlich an der Decke aus und entsprechend schlecht ist die natürliche Sicht hier unten. Die Struktur der Wände an der Westwand ist sehr zerklüftet, scharfkantige Strukturen stehen hervor und der Schattenwurf ist großflächig.")
	if ( wuerfel(6) > 3 ) (textausgabe "Dir kommt es so vor, als würdest du Klopfgeräusche aus dem Stollen kommen hören.")
	if ( wuerfel(6) > 3 ) (textausgabe "Es kommt dir so vor, als hätte sich das Gestein des Felsens leicht verändert.")
	if ( wuerfel(6) > 4 ) (textausgabe "Du bist dir nicht sicher, aber trotz des schwachen Lichts hast du das Gefühl, kleine metallische Lichtreflexe auf der Felswand erkennen zu können.")
	(auswahl '() "Du kannst dem Tunnel nach Norden folgen (1) oder nach Süden (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort149, ort158, (wuerfel(6) > 3) ? ort98 : ort95)
)

// -----------
// --- 100 ---
// -----------

(defun ort-100 ()
// Du stimmst Elke zu und ihr verlaßt das Parkhaus
	raum = 100;
	(textausgabe "Ihr verlaßt das Auto. Elke fährt den Wagen die Stepgesstraße hinunter und hält vor der Ampel. Sobald Grün kommt, überquert sie die Kreuzung und fährt den Berliner Platz entlang. Du schaust hinüber zum Kaiserbad, wo du als kleines Kind schwimmen gelernt hast.~%\"Schaltest du mal das Radio an?\" bittet sie dich und du wendest dich der Mittelkonsole zu. Kurz darauf ist das Digitalradio an. \"Mit Radio 90,1 wird das nichts. Der Sender hat wohl keinen Saft.\" kommentierst du den vorausgewälten Sender und probierst einen Senderplatz nach dem anderen. Schließlich empfängt das Radio ein Signal auf der Deutschen Welle.~%\"... wird im Westen der Republik von verheerenden Unwettern berichtet, die auch im Venloer Raum bereits für erhebliche Zerstörung gesorgt haben sollen. Fast die ganze Niederlande und der Niederrhein verfügen über keinerlei Strom mehr. Auch die offiziellen Behördennetzwerke in dem Gebiet können nicht durch die Bundesregierung erreicht werden. Derweil hat der Kanzler eine Mobilmachung der Bundeswehr angeordnet, die eine Versorgung des Gebietes erreichen soll. Aufnahmen von Wettersatelliten zeigen in der Region nichts weiter als eine ausnehmend großen Masse an dichten dunklen Wolken....\"~%Im nächsten Augenblick hatte das Radio auch die Deutsche Welle verloren. Der Wagen rollte derweil die Aachener Straße in Richtung Rheindahlen, Wegberg, Erkelenz entlang. Sie kamen am neuen Bundesliga-Stadion vorbei, doch die Sicht wurde jetzt weiter, offener. Überall im Westen näherte sich eine riesige, bedrohliche Wolkenwand. Selbst im Süden konnte man jetzt sehen, daß sie auch dort waren. Im Osten hingegen kam nun ein kleiner Spalt hellblauen Himmels zum Vorschein. Elke fuhr mit dem Auto auf einen Wendeplatz und hielt den Wagen an.~%\"Was denkst du, wohin sollen wir jetzt fahren?\"")
	(auswahl '() "Wenn du weiter nach Süden willst (1), nach Westen, wo die Wolken noch viel näher und finsterer sind (2), Osten, wo ein Streifen blauer Himmel jetzt sichtbar wird (3), oder zurück nach Mönchengladbach, wo die Turmmonster sind, du dich aber auskennst (4).", 4, ort106, ort107, ort108, ort109)
)

(defun ort-101 ()
// Du hast ein mulmiges Gefühl und versuchtst Elke vom rausfahren abzuhalten
	raum = 101;
	if ( tripodgesehen ) (textausgabe "Du erzählst Elke alles, was du gesehen hast, und beschreibst ihr verbal dieses turmgroße Etwas, das sich wie auf Stelzen zu bewegen schien und von dem aus Lichtbögen die Umgebung zerstörten.~%Elke sieht dich irritiert an und fragt schließlich, wieso du ihr das nicht schon früher gesagt hättest? Schließlich bricht sie in schallendes Gelächter aus. Es vergeht eine Minute, bis sie sich wieder unter Kontrolle hat.~%\"Ich denke, du bist ein echt lieber Kerl, aber auch ein ziemlicher Phantast, hm?\"~%Sie wirft den Motor an. Du glaubst deiner Erinnerung auch nicht wirklich - und irgendwie scheint es sowieso egal zu sein. Im Moment ist einzig und allein am wichtigsten, einmal etwas über die aktuelle Lage in Erfahrung zu bringen. Langsam rollt der Wagen durch das Parkhaus.")
	else (textausgabe "Du erzählst ihr, das du ein ungutes Gefühl hast, ob es nun an dem schlechten Wetter, den Sirenen oder was auch immer liegt, kannst du nicht sagen, aber du bringst klar zum Ausdruck, daß du kein gutes Gefühl dabei hast, in einem Auto auf die Straßen zu fahren, nicht jetzt, nicht bei diesem Wetter, der Situation - und erst recht nicht, wenn es nach deinem Bauchgefühl geht. Aber Elkes Einwände sind besser, immerhind könntet ihr ein wenig erfahren, wieso die ganze Stadt einen Stromausfall erfahren hat. Du zuckst noch einmal mit den Schultern und willigst ein, schnallst dich an und schon geht es aus der Parklücke heraus und ihr rollt durch das dunkle Parkhaus.")
	(textausgabe "Plötzlich gibt es so etwas wie einen kurzen, heftigen Erdstoß. Von einigen wenigen Autos schaltet sich die Diebstahlwarnanlage and und eröffnet eine neue Kakophonie innerhalb des Parkhauses. Jetz wäre dir wirklich nichts lieber, als endlich hier heraus zu sein. Ihr nähert euch der Ausfahrt. Elke schiebt ihre Karte in den alten Scheidt&Bachmann, dann öffnet sich die Schranke. Ihr fahrt langsam an - und verlaßt das Parkhaus. Elke will gerade abfahren, als zum zweiten Mal ein Erdstoß erfolgt.~%\"Gib bitte Gas!\" sagst du zu ihr, mit einem mummeligen Gefühl im Magen. Elke nickt und fährt los.")
	versuchedeinglueck(ort103, ort102)
)

(defun ort-102 ()
// Kein Glück - es hat Elke erwischt
	raum = 102;
	(textausgabe "Mit einem fürchterlichem Geräusch von schneidendem Metall, einer unglaublichen Erschütterung und einem unbeschreiblichem, donnernden Geräusch, wird ein Teil eures Autos zerteten, durchgeteilt, vernichtet. Ein riesiger Metallfuß, wie du ihn von AT-AT aus dem Kino kennst, hat einen Teil des Motorblocks und die Fahrerseite zerquetscht. Du selber bist auch etwas eingequetscht. Dein Herz rast vor Angst und entsetzen. Neben dir, daß weißt du instinktiv, hat gerade Elke ihren Tod gefunden. Obwohl du sie kaum kanntest, tut dir das in der Seele weh, du weinst - versuchst aber gleichzeitig keinen Mucks zu machen - um nicht das gleiche Schicksal zu erleiden. In deiner verzweifelten Lage klappst du den Sitz langsam nach hinten. Du schaffst es, die Befestigung des Rücksitzes zu lösen - und kannst so deinen Rucksack aus dem Kofferraum holen. Du holst zusätzlich noch die Beutel die du erreichen kannst und stopfst dir deinen Rucksack damit voll. Du versuchst nicht zur Seite zu schauen, dir ein Bild von Elke zu machen.")
	(auswahl '() "Willst du dich weiter, so leise wie möglich verhalten (1) oder willst du versuchen, aus dem Auto herauszukommen - und zu fliehen. Immerhin könnte der riesige Fuß ja noch ein zweites Mal auf das Auto herabsausen (2)?", 2, ort104, ort105)
)

(defun ort-103 ()
// Glück gehabt
	raum = 103;
	(textausgabe "Keine zwei Meter von eurem Auto entfernt, knallt ein riesiger Metallfuß mit ohrenbetäubendem Lärm und unter einer kurzen, heftigen Erschütterung, auf den Boden. Ihr werdet beide blass - doch sofort brüllst du Elke an: \"Fahr! Fahr! Fahr!\"~%Sie löst sich aus ihrer Schockstarre und tritt das Gaspedal bis zum Anschlag durch. Mit einem Satz rollt ihr die Stepgesstraße hinab. Ohne auf die Ampelanlage zu achten, schießt ihr über die Kreuzung, am Kaiserbad vorbei in Richtung Korschenbroicher Straße, mit quietschenden Reifen biegt ihr in die Fliethstraße ein, während Elke immer weiter beschleunigt. Und du hast nichts dagegen einzuwenden, denn auch du willst nichts weiter, als weg. Ihr rast am Geroweiher vorbei, als du brüllst, sie soll nach Süden abbiegen. So rast ihr nahezu unaufhalsam über die Kreuzung und dann die Aachener Straße in entlang in Richtung Rheindalen. Für den Moment überlegst du ob ihr auf die Autobahn sollt, aber von dort aus kann man gar nicht mehr abbiegen.~%\"Nicht auf die Autobahn\", sagst du zu ihr, \"da säßen wir in der Falle - und unser Weg wäre vorhersehbar. Als ihr über die Brücke fahrt, die über die Autobahn führt, siehst du, wie klug eure Entscheidung war. Ein weiteres dieser Turmmonster steht mitten auf der Autobahn, einen Berg aus Blechkadavern zu seinen Füßen.~%\"Wohin fahren wir jetzt?\" will sie wissen, während ihr jetzt der Straße weiter in Richtung Wegberg und Erkelenz folgt.~%\"Erst mal weiter in Richtung Süden, an dieser Unwetterwolke und den Metalltürmen vorbei. Da entdeckt ihr weit im Süden, das die Wolke sich auch dort bereits findet. Elke fährt auf den nächsten Wendeplatz und hält den Wagen an. \"Bist du dir sicher, das wir weiter nach Süden fahren wollen?\"")
	(auswahl '() "Wenn du weiter nach Süden willst (1), nach Westen, wo die Wolken noch viel näher und finsterer sind (2), Osten, wo ein Streifen blauer Himmel jetzt sichtbar wird (3), oder zurück nach Mönchengladbach, wo die Turmmonster sind, du dich aber auskennst (4).", 4, ort106, ort107, ort108, ort109)
)

(defun ort-104 ()
// Im Auto bleiben
    beenden(FARBE_ROT, EXIT_SUCCESS, "Du verhältst dich muskmäuschen still. Den Rucksack hast du auf dem Schoß. Du atmest nur gan flach, schluchzt aber auch immer wieder. Du bedauerst deine eigene verzweifelte Lage noch viel mehr, als den Tod der freundlichen Frau, die hier, nur wenige Zentimeter entfernt von dir, unter einem tonnenschweren Stahlfuß zerquetscht wurde. Plötzlich hörst rechts neben dir ein Geräusch. Erschreckt siehst du zur Seite - und starrst in die Mündung eines Automatikgewehres.~%Befriedigt grunzt das Wesen, das gerade seine Waffe in deinem Kopf entladen hat, dann wendet es sich vom Anblick deines Kadavers ab und sucht nach weiteren \"Ausreißern\".")
)

(defun ort-105 ()
// Flucht
	raum = 105;
)

(defun ort-106 ()
// Süden
	raum = 106;
)

(defun ort-107 ()
// Westen
	raum = 107;
)

(defun ort-108 ()
// Osten
	raum = 108;
)

(defun ort-109 ()
// Norden
	raum = 109;
)

(defun ort-110 ()
)

(defun ort-111 ()
)

(defun ort-112 ()
)

(defun ort-113 ()
	// Begegnung mit einem Zufallsgegner der "Militärseite"
	charakter_s gegner[] = {
		{ "Uniformierter", 2, 2, 3, 3 ),
		{ "Besatzer", 6, 6, 3, 3 ),
		{ "aggressiver Uniformträger", 6, 6, 4, 4 ),
		{ "Soldat einer fremden Armee", 5, 5, 4, 4 ),
		{ "gutgläubiger Mitläufer", 6, 6, 5, 5 ),
		{ "Heckenschütze", 8, 8, 4, 4 ),
		{ "gepanzerter Beserker", 6, 6, 8, 8 )
	);
	int zufallsgegner = wuerfel(7)
	bool kampfausgang = false;
	if ( zufallsgegner == 1 ) kampfausgang = kampf(&spieler, &gegner[0], 1, false, NULL)
	else if ( zufallsgegner == 2 ) kampfausgang = kampf(&spieler, &gegner[1], 1, false, NULL)
	else if ( zufallsgegner == 3 ) kampfausgang = kampf(&spieler, &gegner[2], 1, false, NULL)
	else if ( zufallsgegner == 4 ) kampfausgang = kampf(&spieler, &gegner[3], 1, false, NULL)
	else if ( zufallsgegner == 5 ) kampfausgang = kampf(&spieler, &gegner[4], 1, false, NULL)
	else if ( zufallsgegner == 6 ) kampfausgang = kampf(&spieler, &gegner[5], 1, false, NULL)
	else if ( zufallsgegner == 7 ) kampfausgang = kampf(&spieler, &gegner[6], 1, false, NULL)
	if ( kampfausgang ) {
		getoetetegegner += 1;
		if ( wuerfel(6) >= 4 && objekt[pistole] <= 1 && (ja-oder-nein-p "Willst du die Pistole deines Gegners an dich nehmen?") ) objekt[pistole] += 1;
		else if ( objekt[gewehr] <= 0 && (ja-oder-nein-p "Willst du das Gewehr deines Gegners an dich nehmen?") ) objekt[gewehr] += 1;
		objekt[patrone] += wuerfel(8)
	)
	else beenden(FARBE_ROT, EXIT_SUCCESS, "Das war nicht dein bester Kampf. Um ehrlich zu sein, das war dein schlechtester Kampf - und auch dein letzter Kampf. Dein allerletzter Kampf, den du nicht überlebt hast. Mit dir ist es zu ENDE gegangen.")
)

(defun ort-114 ()
)

(defun ort-115 ()
)

(defun ort-116 ()
)

(defun ort-117 ()
)

(defun ort-118 ()
)

(defun ort-119 ()
)

(defun ort-120 ()
)

(defun ort-121 ()
)

(defun ort-122 ()
)

(defun ort-123 ()
)

(defun ort-124 ()
)

(defun ort-125 ()
)

(defun ort-126 ()
)

(defun ort-127 ()
)

(defun ort-128 ()
)

(defun ort-129 ()
)

(defun ort-130 ()
)

(defun ort-131 ()
	(rotiere-plattform)
	if(raum == 57) {
		raum = 131;
		(auswahl '() "Der Gang macht hier einen Knick nach links. Willst du dem Knick folgen (1) oder dich umdrehen und zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3).", 3, ort132, ort57, ort212)
	) else { // raum == 132
		raum = 131;
		(auswahl '() "Der Gang macht hier einen Knick nach rechts. Willst du dem Knick folgen (1) oder dich umdrehen und zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3).", 3, ort57, ort132, ort212)
	)
)

(defun ort-132 ()
	(rotiere-plattform)
	if(raum == 131) {
		raum = 132;
		(auswahl '() "Der Gang macht hier einen Knick nach rechts. Willst du dem Knick folgen (1) oder dich umdrehen und zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3).", 3, ort138, ort131, ort212)
		
	) else { // raum == 138
		raum = 132;
		(auswahl '() "Der Gang macht hier einen Knick nach links. Willst du dem Knick folgen (1) oder dich umdrehen und zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3).", 3, ort131, ort138, ort212)
	)
)

(defun ort-133 ()
	(rotiere-plattform)
	(textausgabe "Der Gang in dem du dich befindest, scheint unendlich lang zu sein.")
	if(raum == 65) {
		raum = 133;
		(auswahl '() "Du kannst dem Gang weiter folgen (1) oder umdrehen und zurückgehen (2). Wenn du willst, kannst du auch die Wände nach Geheimgängen abtasten (3)", 3, ort134, ort65, ort212)
	) else { // raum == 134
		raum = 133;
		(auswahl '() "Du kannst dem Gang weiter folgen (1) oder umdrehen und zurückgehen (2). Wenn du willst, kannst du auch die Wände nach Geheimgängen abtasten (3)", 3, ort65, ort134, ort212)
	)
)

(defun ort-134 ()
	(rotiere-plattform)
	(textausgabe "Du kommst zu einer Stelle, an der der Gang eine Biegung macht.")
	if(raum == 95) {
		raum = 134;
		(auswahl '() "Möchtest du der Abbiegung nach links folgen (1) oder drehst du um und gehst zurück (2)? Wenn du möchtest, kannst du auch die Wände nach Geheimgängen abtasten (3)", 3, ort133, ort95, ort212)
	) else { // raum == 133
		raum = 134;
		(auswahl '() "Möchtest du der Abbiegung nach rechts folgen (1) oder drehst du um und gehst zurück (2)? Wenn du möchtest, kannst du auch die Wände nach Geheimgängen abtasten (3)", 3, ort95, ort133, ort212)
	)
)

(defun ort-135 ()
	(rotiere-plattform)
	if(raum == 62) {
		raum = 135;
		(textausgabe "Der Gang knickt hier nach links ab.")
		(auswahl '() "Du kannst dem Gang weiter folgen (1) oder dich umdrehen und zurückkehren (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort136, ort62, ort212)
	) else {
		raum = 135;
		(textausgabe "Der Gang knickt hier nach rechts ab.")
		(auswahl '() "Du kannst dem Gang weiter folgen (1) oder dich umdrehen und zurückkehren (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort62, ort136, ort212)
	)
)

(defun ort-136 ()
	(rotiere-plattform)
	if(raum == 63) {
		raum = 136;
		(textausgabe "Der Gang knickt hier nach rechts ab.")
		(auswahl '() "Du kannst dem Gang weiter folgen (1) oder dich umdrehen und zurückkehren (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort135, ort63, ort212)
	) else {
		raum = 136;
		(textausgabe "Der Gang knickt hier nach links ab.")
		(auswahl '() "Du kannst dem Gang weiter folgen (1) oder dich umdrehen und zurückkehren (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort63, ort135, ort212)
	)
)

(defun ort-137 ()
	(rotiere-plattform)
	(textausgabe "Du kommst an einen Knick im Tunnel.")
	if(raum == 138) {
		raum = 137;
		(auswahl '() "Willst du dem Gang weiter nach links folgen (1) oder kehrt machen und den Weg zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3).", 3, ort143, ort138, ort212)
	) else {
		raum = 137;
		(auswahl '() "Willst du dem Gang weiter nach rechts folgen (1) oder kehrt machen und den Weg zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3).", 3, ort138, ort143, ort212)
	)
)

(defun ort-138 ()
	(rotiere-plattform)
	if(raum == 132) {
		raum = 137;
		(auswahl '() "Du stößt auf ein Ende des Ganges. Von hier aus kannst du nur noch nach rechts (1) oder nach links (2) weitergehen, sofern du dich nicht lieber schnurstracks umdrehst, um dich nicht weiter zu verirren (3). Wenn du denkst, es könnt hier ein Geheimgang sein, kannst du natürlich auch die Wände absuchen (4)!", 4, ort137, ort139, ort132, ort212)
	) else if(raum == 137) {
		raum = 137;
		(auswahl '() "Du kommst an eine Abzweiung. Du kannst weiter geradeaus gehen (1) oder nach links abbiegen (2). Natürlich kannst du dich auch einfach umdrehen und zurückgehen (3) oder nach Geheimgängen suchen (4), wenn du glaubst, hier könnte einer sein.", 4, ort139, ort132, ort137, ort212)
	) else { // raum == 139
		raum = 137;
		(auswahl '() "Du kommst an eine Abzweigung. Du kannst weiter geradeaus gehen (1) oder nach rechts abbiegen (2). Natürlich kannst du dich auch einfach umdrehen und zurückgehen (3) oder nach Geheimgängen suchen (4), wenn du glaubst, hier könnte einer sein.", 4, ort137, ort132, ort139, ort212)
	)
)

(defun ort-139 ()
	(rotiere-plattform)
	if(raum == 64) {
		raum = 139;
		(auswahl '() "Der Gang macht an dieser Stelle einen Knick nach rechts. Willst du dem Knick folgen (1) oder zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3)", 3, ort138, ort64, ort212)
	) else { // raum == 138
		raum = 139;
		(auswahl '() "Der Gang macht an dieser Stelle einen Knick nach links. Willst du dem Knick folgen (1) oder zurückgehen (2)? Du kannst natürlich auch die Wände nach Geheimgängen absuchen (3)", 3, ort64, ort138, ort212)
	)
)

(defun ort-140 ()
	(rotiere-plattform)
	raum = 140;
	(auswahl '() "Du kannst den Weg zurück nach Westen gehen (1) oder die Wände nach Geheimgängen absuchen (2)", 2, ort66, ort212)

)

(defun ort-141 ()
	(rotiere-plattform)
	raum = 141;
	(auswahl '() "Du kannst den Weg zurück nach Osten gehen (1) oder die Wände nach Geheimgängen absuchen (2)", 2, ort142, ort212)
)

(defun ort-142 ()
	(rotiere-plattform)
	if ( raum == 94 ) (textausgabe "Du kriechst durch die Schwärze eines Tunnels, eine halbe Ewigkeit lang, wie es dir erscheint, bis du schließlich ein violett schimmerndes Licht am Ende deines Tunnels erblicken kannst. So schnell du kannst, krabbelst du darauf zu.")
	raum = 142;
	(textausgabe "Der Raum sieht aus wie ein großes Achteck. Die Wände und der Boden, alles deutet darauf hin, daß du in einem riesigen Amethysten stehst. Das Spiel der Farben ist atemberaubend, und doch benebelt der Anblick auch die Sinne, denn die Tiefenschärfe hat leichte Probleme, die genaue Kontur der Wände wahrzunehmen. Drei Türen befinden sich in diesem Raum. Die eine ist aus weißem Marmor geformt und befindet sich im Westen, die zweite besteht aus schwarzem Onyx und befindet sich im Süden. Im Osten ist eine Türe, die aussieht, als wäre sie aus Gold, doch verrät dir der Versuch an ihr zu kratzen, daß das Material härter ist als der härteste Stahl. Die Türe verfügt über 4 Schlüssellöcher.")
	if ( dwellmer ) (auswahl '() "Du kannst die Höhlen der Jagd verlassen und das Dorf der Dwellmer im Osten betreten (1), den Raum durch die Marmortüre im Westen verlassen (2) oder durch die schwarze Onyxtüre im Süden (3). Wenn dir nach einem Abenteuer ist, kannst du dich auch durch die dunklen Geheimtunnel zwängen (4)", 4, ort165, ort141, ort96, (wuerfel(6) > 3) ? ort94 : ort95)
	if ( ( schluessel9 + schluessel66 + schluessel99 + schluessel111_1 + schluessel111_2 + schluessel125) > 3 ) {
		if ( (ja-oder-nein-p "Du hast genügend Schlüssel bei dir. Möchtest du versuchen die Türe zu öffnen?") ) {
			if ( schluessel9 && schluessel66 && schluessel99 && schluessel125 ) {
				(textausgabe "Du probierst die Schlüssel aus. Sie passen. Ein Schlüssel nach dem anderen läßt sich in eines der Schlüssellöcher nach dem anderen einsetzen und umdrehen. Nachdem der vierte Schlüssel eingeführt worden ist, gibt es ein klickendes Geräusch. Dieses Geräusch ist das letzte, was du wahrnimmst. Du erhältst einen Schlag auf den Kopf, dann wird alles dunkel um dich.")
				// Jetzt geht es weiter -> ins Zwergendorf
				ort175()
			)
			else (textausgabe "Leider hast du nicht die passenden Schlüssel bei dir.")
		)
	)
	else (textausgabe "Du probierst erst gar nicht, die Schlüssel in die Schlösser zu stecken, da du deutlich zu wenige davon hast.")
	(auswahl '() "Du kannst den Raum durch die Marmortüre im Westen verlassen (1) oder durch die schwarze Onyxtüre im Süden (2). Ebenso kannst du die glitzernden und funkelnden Amethystwände nach Geheimtüren absuchen (3)", 3, ort141, ort96, (wuerfel(6) > 3) ? ort94 : ort95)
)

(defun ort-143 ()
	(rotiere-plattform)
	raum = 143;
	(textausgabe "Die Wände dieses Raumes scheinen die Funktionsfähigkeit deines Kompasses zu beeinflussen, die Nadel rotiert wie wild. Der Raum in dem du jetzt stehst, wird an seinem einen Ende durch eine herabstürzende Wand aus Wasser begrenzt, während auf der anderen Seite ein Tunnel in den Felsen führt.")
	(auswahl '() "Du kannst dem Tunnel folgen (1) oder versuchen die Wasserwand zu durchqueren(2). Wenn du glaubst, du könntest einen Geheimgang finden, dürfte hier der ideale Ort zum Suchen sein (3).", 3, ort137, ort80, ort212)
)

(defun ort-144 ()
	(rotiere-plattform)
	raum = 144;
	(textausgabe "Der Stollen fällt langsam von Osten nach Westen ab, er macht an dieser Stelle einen Knick von Osten nach Süden. Ein paar Schienen laufen an der Südwand entlang und biegen dann schließlich ganz nach Süden ab, wo sie an der Ostwand weiterführen. Das Wurzelwerk breitet sich nur sehr spärlich an der Decke aus und entsprechend schlecht ist die natürliche Sicht hier unten.")
	if ( wuerfel(6) > 4 ) (textausgabe "Dir kommt es so vor, als würdest du Klopfgeräusche aus dem Stollen kommen hören.")
	if ( wuerfel(6) > 4 ) (textausgabe "Es kommt dir so vor, als hätte sich das Gestein des Felsens leicht verändert.")
	if ( wuerfel(6) > 5 ) (textausgabe "Du bist dir nicht sicher, aber trotz des schwachen Lichts hast du das Gefühl, kleine metallische Lichtreflexe auf der Felswand erkennen zu können.")
	(auswahl '() "Du kannst nach Osten gehen (1) oder nach Süden (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort145, ort149, ort212)
)

(defun ort-145 ()
	(rotiere-plattform)
	raum = 145;
	(textausgabe "Der Stollen fällt langsam von Osten nach Westen ab. Ein paar Schienen laufen an der Südwand entlang. Das Wurzelwerk breitet sich nur sehr spärlich an der Decke aus und entsprechend schlecht ist die natürliche Sicht hier unten.")
	if ( wuerfel(6) > 5 ) (textausgabe "Dir kommt es so vor, als würdest du Klopfgeräusche aus dem Stollen kommen hören.")
	(auswahl '() "Du kannst dem Tunnel nach Westen folgen (1) oder nach Osten (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort144, ort88, ort212)
)

(defun ort-146 ()
	(rotiere-plattform)
	raum = 146;
	(textausgabe "Du befindest dich in einer riesigen Höhle. Sie ist so riesig, das du nur im Westen eine Wand wahrnehmen kannst. Umgeben bist du von einer Art Wald, aber es ist kein natürlicher Wald, wie du ihn von der Oberfläche her kennst. Es ist ein Funghi-Wald. Pilze, kleine Pilze, wie Champions wachsen hier so zahlreich, das es unmöglich erscheint, nicht auf sie zu treten, aber auch Pilze in der Größe von Mäusen, von Katzen. Doch es sind auch größere Exemplare da, Pilze in der Größe von Schränken, von Bäumen und sogar so hoch, wie ein Hochhaus. Ihre Stengel und Hüte haben die verschiedensten Farben und Formen und du kannst Summen und Surren hören. Der Anblick ist atemberaubend.")
	if ( wuerfel(6) > 3 ) (textausgabe "Trotz dieser gleichzeitig verstörenden und beeindruckenden Schönheit dieses Ortes, wirst du dir schnell einer Sache bewußt: Wenn derart großes Leben hier unten möglich ist, dann wäre es vielleicht an der Zeit, ein wenig mehr Achtsamkeit und Wachsamkeit an den Tag zu legen.")
	(auswahl '() "Du kannst die Pilzhöhle im Westen verlassen und einen Tunnel betreten (1) oder dich weiter in östlicher Richtung durch den Pilzwald bewegen (2) oder versuchen, ob du einen Weg in eine andere Richtung finden kansnt (3)", 3, ort87, ort147, ort210)
)

(defun ort-147 ()
	(rotiere-plattform)
	raum = 147;
	(textausgabe "Du befindest dich in einem Pilzwald. Überall um dich herum wachsen Pilze der verschiedensten Größe. Surrende Geräusche erfüllen die Luft.")
	if ( wuerfel(6) > 4 ) ort210()
	(auswahl '() "Du kannst dich weiter in westlicher Richtung bewegen (1) oder in östlicher Richtung (2). Wenn du etwas Zeit investierst, kannst du vielleicht noch einen anderen Weg entdecken (3)", 3, ort146, ort148, ort210)
)

(defun ort-148 ()
	(rotiere-plattform)
	(textausgabe "Du befindest dich in einem Pilzwald, tief unter der Erde. Um dich herum befindet sich ein bunter Wald aus Pilzen. Hier ist ein kleiner Teich voll klaren Wassers, ein paar Felsen laden zum darauf sitzen ein.")
	if ( raum != 148 ) mahlzeit()
	raum = 148;
	(auswahl '() "Du kannst dich weiter in westlicher Richtung bewegen (1) oder in östlicher (2). Wenn du etwas Zeit investierst, kannst du vielleicht noch einen anderen Weg entdecken (3)", 3, ort147, ort97, ort210)
)

(defun ort-149 ()
	(rotiere-plattform)
	if(raum == 149)
		if(schluessel9 || schluessel66 || schluessel99 || schluessel111_2 || schluessel125 || schluesselbootshaus)
			(textausgabe "Nach mehreren Versuchen gibst du auf. Es sieht nicht danach aus, das du den richtigen Schlüssel bei dir hast.")
		else
			(textausgabe "Die Türe ist verschlossen und äußerst massiv. Ohne Schlüssel wirst du hier nicht weiterkommen.")
	else
		(textausgabe "Der Stollen fällt langsam von Norden nach Süden ab. An dieser Stelle befindet sich ein etwa mannsgroße Türe mit schweren Eisenbeschlägen in der Ostwand. Ein paar Schienen laufen an der Ostwand entlang. Das Wurzelwerk breitet sich nur sehr spärlich an der Decke aus und entsprechend schlecht ist die natürliche Sicht hier unten.")
	raum = 149;
	if ( wuerfel(6) > 3 ) (textausgabe "Dir kommt es so vor, als würdest du Klopfgeräusche aus dem Stollen kommen hören.")
	if ( wuerfel(6) > 5 ) (textausgabe "Es kommt dir so vor, als hätte sich das Gestein des Felsens leicht verändert.")
	if ( wuerfel(6) > 5 ) (textausgabe "Du bist dir nicht sicher, aber trotz des schwachen Lichts hast du das Gefühl, kleine metallische Lichtreflexe auf der Felswand erkennen zu können.")
	if ( wuerfel(6) > 4 ) (textausgabe "Für einen kurzen Moment hattest du das Gefühl, ein Stöhnen von hinter der Türe gehört zu haben.")
	if ( schluessel111_1 ) (auswahl '() "Du kannst dem Tunnel nach Norden folgen (1) oder nach Süden (2) oder versuchen, ob einer deiner Schlüssel die Türe im Osten öffnet (3). Du kannst auch die Wände nach Geheimgängen absuchen (4).", 4, ort144, ort99, ort150, ort212, NULL, NULL)
	else if ( schluessel9 || schluessel66 || schluessel99 || schluessel111_2 || schluessel125 || schluesselbootshaus )
		(auswahl '() "Du kannst dem Tunnel nach Norden folgen (1) oder nach Süden (2) oder versuchen, ob einer deiner Schlüssel die Türe im Osten öffnet (3). Du kannst auch die Wände nach Geheimgängen absuchen (4).", 4, ort144, ort99, ort149, ort212, NULL, NULL)
	else
		(auswahl '() "Du kannst dem Tunnel nach Norden folgen (1) oder nach Süden (2) oder versuchen, ob du die Türe im Osten öffnen kannst (3). Du kannst auch die Wände nach Geheimgängen absuchen (4).", 4, ort144, ort99, ort149, ort212, NULL, NULL)
)

(defun ort-150 ()
	(rotiere-plattform)
	if ( raum == 14 && stollentroll == 150 ) {
		charakter_s troll = { "Stollentroll", 9, 9, 10, 10 );
		if ( !kampf(&spieler, &troll, 1, false, NULL) ) beenden(FARBE_ROT, EXIT_SUCCESS, "Die Fäuste des Stollentrolls hämmern gnadenlos auf deinen Kopf ein. Du spürst das Blut in deine Augen laufen, hörst das Knacken und Krachen deiner berstenden Schädelplatte. Ein heftiger Schmerz durchzuckt dich - dann verschwimmt alles vor deinen Augen, wird blasser, dunkel. Ein Tunnel ... da ist ein Licht am ENDE des Tunnels ...")
		// Dann platzieren wir mal die Zwerge um
		if(minenzwerge == 158) minenzwerge = 160;
	)
	(textausgabe "Der Raum ist dunkel. Der Stollentroll, mit dem du eine wilde Keilerei hattest, liegt tot am Boden und sein Kadaver stinkt wie einst im Mai. Tatsächlich ist der Gestank so stechend, daß dir die Tränen in die Augen steigen. Im Westen befindet sich die offene Türe, durch die du hereingekommen bist, und im Süden glaubst du, einen weiteren Durchgang im Schatten ausmachen zu können.")
	raum = 150;
	(auswahl '() "Du kannst nach Westen gehen (1) oder nach Süden (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort149, ort155, ort212)
)

(defun ort-151 ()
	(rotiere-plattform)
	raum = 151;
	(textausgabe "Der Raum, den du betrittst riecht, als wäre er aus Kuhfladen oder schlimmeren hergestellt. Die Wände erscheinen porös oder schlimmer zu sein. Jeder Schritt auf dem Boden läßt dich in dem braunen Schlick ein wenig einsinken.")
	(auswahl '() "Du kannst dich nach Norden bewegen (1) oder dem stinkigen Weg weiter nach Osten folgen (2). Wenn es dich vor nichts graust, so kannst du auch die Wände nach Geheimgängen absuchen (3)", 3, ort87, ort152, ort212)
)

(defun ort-152 ()
	(rotiere-plattform)
	if ( raum == 79 ) (textausgabe "Du bewegst dich durch einen stockdunklen Gang. Mehr als einmal triffst du auf abzweigende Gänge - und immer entscheidest du dich intuitiv, wo du langgehst. Schließlich wird es etwas heller. Deine Hände greifen durch eine Art von Ranken hindurch, du teilst sie und stehst Augenblicke später in einem hell erleuchteten Raum. Deine Augen tränen etwas und du benötigst einen Moment, um dich an das Licht zu gewöhnen. Offensichtlich war der Gang länger, als du dachtest.")
	raum = 152;
	(textausgabe "Der Raum in dem du dich befindest, riecht eigenartig und überall an den Wänden hängen ranken eines seltsamen rötlichbraunen Grases herunter, so daß die darunterliegenden Wände eher zu erahnen als zu erkennen sind. Nur in westlicher Richtung kannst du einen Ausgang ausmachen, der Raum selber scheint eine Art totes Ende zu sein.")
	(auswahl '() "Du kannst den Weg zurück nach Westen gehen (1) oder die Ranken absuchen, ob sich irgendwo darunter vielleicht noch ein weiterer Weg versteckt (2)", 2, ort151, ort79)
)

(defun ort-153 ()
	(rotiere-plattform)
	raum = 153;
	(textausgabe "Du befindest dich in einem unüberschaubar großen Wald aus Pilzen. Fremde Geräusche dringen von allen Seiten auf dich ein. Südlich von dir befindet sich der gigantische Pilz, der das Zentrum dieses Ortes zu sein scheint.")
	(auswahl '() "Die Wand aus Pilzen läßt es offensichtlich nur zu, das du dich von hier aus in östlicher Richtung (1) oder weiter nach Süden auf den Riesenpilz zu bewegen kannst (2). Wenn du willst, kannst du etwas Zeit investieren und versuchen, ob du vielleicht noch einen weiteren Weg finden kannst (3)", 3, ort154, ort156, ort210)
)

(defun ort-154 ()
	(rotiere-plattform)
	raum = 154;
	(textausgabe "Du befindest dich inmitten eines Meeres aus Pilzen aller Größen und Farben. Die Luft wirkt hier sehr stickig und das Atmen fällt dir etwas schwerer als normal. Schweißperlen bilden sich auf deiner Stirn. Von hier aus führen Wege durch das Pilzdickicht in verschiedene Richtungen.")
	(auswahl '() "Du könntest dich weiter in südlicher (1) oder westlicher (2) Richtung an den Riesenpilz im Zentrum heranwagen oder dich in nördlicher (3) Richtung von ihm entfernen. Wenn du etwas Zeit zu investieren bereit bist, kannst du auch nach weiteren Wegen im Dickicht suchen (4).", 4, ort157, ort153, ort97, ort210)
)

(defun ort-155 ()
	(rotiere-plattform)
	if ( raum == 159 ) {
		(textausgabe "Der Raum liegt im Dunkeln. Du glaubst vor dir im Schatten eine Bewegung auszumachen. Ein Brüllen ist zu hören - und plötzlich, stürzt vor dir die Decke ein! Du nimmst die Beine in die Hand und rennst wie von der Tarantel gestochen zur Türe, durch die du gerade erst hereingekommen bist, hinaus.")
		raum = 155;
		ort159()
	) else {
		(textausgabe "Der Raum liegt im Dunkeln. Du kannst über dir im Gestein ein schweres Knacken und Knirschen hören. Eine Gänsehaut jagt dir über den Rücken. Wenn du etwas nicht möchtest, dann an diesem Ort unter Stein begraben zu werden. Hastig drehst du dich um und rennst zurück nach Norden.")
		raum = 155;
		ort150()
	)
)

(defun ort-156 ()
	(rotiere-plattform)
	if(raum == 156) {
		(textausgabe "Trotz deiner intensiven Suche, kannst du keinerlei weitere Wege entdecken.")
		mahlzeit()
	)
	raum = 156;
	(textausgabe "Du befindest dich jetzt im Licht des Riesenpilzes. Je näher du dem Stamm kommst, um so gewaltiger empfindest du ihn. Die Ausmaße sind so riesig, das er an der Oberfläche bestimmt jeden einzelnen der Wolkenkratzer im durch Betonstahl verseuchten Herzen Frankfurts überragen dürfte. Der freie Raum vor dem Riesenstamm ist nicht sehr groß, es wirkt, als würden sich alle anderen Pilze in diesem für deine Verhältnisse gewöhnungsbedürftigen Wald, sich auf diesen Pilz zubewegen.")
	if ( wuerfel(6) > 4 ) (textausgabe "Eine frische Brise kommt auf und es riecht jetzt salzig in der Luft. Du glaubst sogar, das Schlagen von Wellen an das Ufer hören zu können.")
	(auswahl '() "Du kannst dich von hier aus in südlicher Richtung (1) oder in nördlicher Richtung (2) bewegen. Wenn du etwas Zeit investierst, kannst du auch den dichten Pilzwald nach weiteren Wegen durchsuchen (3)", 3, ort163, ort153, ort210)
)

(defun ort-157 ()
	(rotiere-plattform)
	raum = 157;
	(textausgabe "Du machst einen weiten Bogen um den Zentralpilz herum. Weit im Osten glaubst du, eine Wand sehen zu können, aber aufgrund des Dunstes in der Luft könnte es auch eine reine Wunschprojektion deines Geistes sein. Der Pilzstamm liegt von dir aus weit im Westen. Die Pilze um dich herum verbreiten einen üblen Gestank, trotzdem kannst du atmen.")
	(auswahl '() "Du kannst dich von hier aus weiter in südlicher Richtung (1) oder in nördlicher Richtung (2) bewegen. Wenn du etwas Zeit investierst, vermagst du vielleicht noch weitere Wecke in diesem erdrückenden Pilzwald auszumachen (3)", 3, ort164, ort154, ort210)
)

(defun ort-158 ()
	(rotiere-plattform)
	raum = 158;
	charakter_s zwerg[] = {
		{ "Zwerg mit Spitzhacke", 7, 7, 9, 9 ),
		{ "Zwerg mit Schaufel", 5, 5, 6, 6),
		{ "Zwerg mit Hammer", 8, 8, 7, 7)
	);
	(textausgabe "Schon als du dich dem Raum näherst, bemerkst du die Veränderung der Helligkeit. Als du ihn schließlich betrittst, ist er taghell erleuchtet. Eine Art von Käfern krabbeln hier auf den Wänden entlang, ihre Panzer schimmern in hellem weißlichen Licht.")
	if ( minenzwerge == 158 ) {
		if ( schluessel111_1 && (wuerfel(6) > 4) ) {
			(textausgabe "Die Zwerge beäugen dich äußerst mißtrauisch.~%\"Du weißt nicht zufällig, wo mein Schlüssel abgeblieben ist, Fremder, hm?\" fragt er feindselig, während ein anderer Zwerg um dich herumgeht und versucht, deinen Rucksack zu öffnen.~%\"Wie ich's mir gedacht habe!\" brüllt er triumphierend, \"Der Mensch ist ein lausiger Dieb!\"~%Du siehst, wie sie ihre Schaufeln und Spitzhacken in der Haltung verlagern. Jetzt sind es keine Werkzeuge mehr, sondern Waffen.")
			if ( !kampf(&spieler, zwerg, 1, false, ort99) ) beenden(FARBE_ROT, EXIT_SUCCESS, "Drei Zwerge gegen sich aufzubringen war wirklich eine dumme Idee. Du sinkst mit einer riesigen Schädelwunde am Hinterkopf zusammen. Du siehst nichts mehr, hörst aber das Näherkommen ihrer Schritte. In Gedanken bist du auf dem Mönchengladbacher Hauptfriedhof. Du kniest nieder am Grab deiner Großeltern und entspannst dich. DU erträgst den Schmerz der Schläge - und dann verebbt der Schmerz ganz. Du hörst nichts mehr. Du siehst nichts mehr. Du gleitest hinab in die Schwärze. Dein ENDE ist gekommen.")
			// Aus ist's mit den Zwergen
			minenzwerge = 0;
		)
		if ( !schluessel111_1 ) {
			(textausgabe "Du kannst sehen, das an der Hose eines der Zwerge ein Schlüssel an einem Lederbändchen herabbaumelt.")
			if ( (ja-oder-nein-p "Möchtest du versuchen, dem Zwerg den Schlüssel heimlich und in einem unbeobachteten Moment zu stibitzen?") ) {
				if ( tdg(&spieler) ) {
					schluessel111_1 = true;
					(textausgabe "Gerade, als keiner der Zwerge in deine Richtung schaut, hältst du für einen Moment die Luft an und greifst zu. Es ist ganz leicht, den Knoten zu lösen - und schon hältst du den Schlüssel in der Hand. Mit einer sanft gleitenden Bewegung läßt du ihn in deiner Tasche verschwinden.")
				) else {
					if (!kampf(&spieler, zwerg, 1, false, ort99) ) beenden(FARBE_ROT, EXIT_SUCCESS, "Drei Zwerge gegen sich aufzubringen war wirklich eine dumme Idee. Du sinkst mit einer riesigen Schädelwunde am Hinterkopf zusammen. Du siehst nichts mehr, hörst aber das Näherkommen ihrer Schritte. In Gedanken bist du auf dem Mönchengladbacher Hauptfriedhof. Du kniest nieder am Grab deiner Großeltern und entspannst dich. DU erträgst den Schmerz der Schläge - und dann verebbt der Schmerz ganz. Du hörst nichts mehr. Du siehst nichts mehr. Du gleitest hinab in die Schwärze. Dein ENDE ist gekommen.")
					// Aus ist's mit den Zwergen
					minenzwerge = 0;
				)
			)
			if ( minenzwerge == 158 ) (textausgabe "Die Zwerge sind viel zu abgelenkt, um dich tatsächlich wahrzunehmen. Du siehst, wie sie den Felsen scheinbar streicheln und mit ihren Spitzhacken dann gezielt auf Stellen schlagen. Der Fels teilt sich und legt darunterliegende Gesteinsadern frei. Der Vorgang wirkt viel natürlicher und bewußter, als die plumpe Art der Menschen, wahllos auf den Fels einzuprügeln und ihn Kubikmeterweise auszuplündern. Sie entnehmen immer nur Teile der Gesteine aus der Ader, so als wollten sie verhindern, die Ader ausbluten zu lassen. Die so erwirtschafteten Steine legen sie beinahe zärtlich in die Lore, die hier auf den Schienen steht.")
		)
	)
	(auswahl '() "Du kannst dem Tunnel nach Norden folgen (1) oder nach Osten (2) oder die Wände nach Geheimgängen absuchen (3)", 3, ort99, ort159, ort212)
)

(defun ort-159 ()
	(rotiere-plattform)
	// Sind die Minenzwerge im angrenzenden Raum?
	if ( raum == 155 && ( minenzwerge == 158 || minenzwerge == 160 ) ) {
		minenzwerge = 159;
		charakter_s zwerg[3] = { { "Zwerg mit Spitzhacke", 7, 7, 9, 9 ), { "Zwerg mit Schaufel", 5, 5, 6, 6), { "Zwerg mit Hammer", 8, 8, 7, 7) );
		(textausgabe "Die Zwerge sind herangestürmt und beäugen dich äußerst mißtrauisch.~%\"Du weißt nicht zufällig, wo mein Schlüssel abgeblieben ist, Fremder, hm?\" fragt einer von ihnen feindselig, während ein anderer Zwerg um dich herumgeht und versucht, deinen Rucksack zu öffnen.~%\"Wie ich's mir gedacht habe!\" brüllt er triumphierend, \"Der Mensch ist ein lausiger Dieb!\"~%Du siehst, wie sie ihre Schaufeln und Spitzhacken in der Haltung verlagern. Jetzt sind es keine Werkzeuge mehr, sondern Waffen.")
		if(!kampf(&spieler, zwerg, 1, false, ort158))
            beenden(FARBE_ROT, EXIT_SUCCESS, "Drei Zwerge gegen sich aufzubringen war wirklich eine dumme Idee. Du sinkst mit einer riesigen Schädelwunde am Hinterkopf zusammen. Du siehst nichts mehr, hörst aber das Näherkommen ihrer Schritte. In Gedanken bist du auf dem Mönchengladbacher Hauptfriedhof. Du kniest nieder am Grab deiner Großeltern und entspannst dich. DU erträgst den Schmerz der Schläge - und dann verebbt der Schmerz ganz. Du hörst nichts mehr. Du siehst nichts mehr. Du gleitest hinab in die Schwärze. Dein ENDE ist gekommen.")
		// Aus ist's mit den Zwergen
		minenzwerge = 0;
	)
	if ( raum == 159 )
		// Haben wir den passenden Schlüssel?
		if(schluessel9 || schluessel66 || schluessel99 || schluessel111_1 || schluessel125 || schluesselbootshaus)
			(textausgabe "Nach mehreren Versuchen gibst du auf. Es sieht nicht danach aus, das du den richtigen Schlüssel bei dir hast.")
		else
			(textausgabe "Die Türe ist verschlossen und äußerst massiv. Ohne Schlüssel wirst du hier nicht weiterkommen.")
	else
		(textausgabe "Der Stollen fällt hier stärker von Osten nach Westen ab. Das Wurzelwerk breitet sich nur sehr spärlich an der Decke aus und entsprechend schlecht ist die natürliche Sicht hier unten. Die Struktur der Wände an der Südwand ist teilweise zerklüftet, scharfkantige Strukturen stehen hervor und der Schattenwurf ist großflächig.")
	raum = 159;
	(if (> (w6) 3) (textausgabe "Dir kommt es so vor, als würdest du Klopfgeräusche aus dem Stollen kommen hören.")
	(if (> (w6) 3) (textausgabe "Es kommt dir so vor, als hätte sich das Gestein des Felsens leicht verändert.")
	(if (> (w6) 4) (textausgabe "Du bist dir nicht sicher, aber trotz des schwachen Lichts hast du das Gefühl, kleine metallische Lichtreflexe auf der Felswand erkennen zu können.")
	if(schluessel111_2)
		(auswahl '() "Du kannst dem Tunnel nach Westen folgen (1) oder nach Osten (2) oder versuchen, ob einer deiner Schlüssel die Türe in der Nordwand öffnet (3). Du kannst auch die Wände nach Geheimgängen absuchen (4).", 4, ort158, ort160, ort155, ort212, NULL, NULL)
	else if(schluessel9 || schluessel66 || schluessel99 || schluessel111_1 || schluessel125 || schluesselbootshaus)
		(auswahl '() "Du kannst dem Tunnel nach Westen folgen (1) oder nach Osten (2) oder versuchen, ob einer deiner Schlüssel die Türe in der Nordwand öffnet (3). Du kannst auch die Wände nach Geheimgängen absuchen (4).", 4, ort158, ort160, ort159, ort212, NULL, NULL)
	else
		(auswahl '() "Du kannst dem Tunnel nach Westen folgen (1) oder nach Osten (2) oder die Wände nach Geheimgängen absuchen (3)", 4, ort158, ort160, ort212)
)

(defun ort-160 ()
	(rotiere-plattform)
	raum = 160;
	(textausgabe "Der Tunnel endet in einem großen Raum, der sehr stark nach Brandwein und altem Stinkekäse riecht. Er ist vollgestellt mit Schaufeln, Spitzhacken und allerlei anderem Gerümpel.")
	// Hat man den Schlüssel noch nicht, könnte man ihn sich nehmen
	if ( !schluessel111_2 && (ja-oder-nein-p "An einem Haken neben der Eingangstüre hängt ein Kupferschlüssel. Deutlich kannst du darauf die Schriftzeichen \"111\" ausmachen. Möchtest du den Schlüssel an dich nehmen?") ) schluessel111_2 = true;
	// Das ist Pech. Ausser dem Schlüssel sind auch noch die Zwerge da ...
	if ( minenzwerge == 160 && (schluessel111_1 || schluessel111_2) ) {
		charakter_s zwerg[3] = { { "Zwerg mit Spitzhacke", 7, 7, 9, 9 ), { "Zwerg mit Schaufel", 5, 5, 6, 6), { "Zwerg mit Hammer", 8, 8, 7, 7) );
		(textausgabe "Erst spät bemerkst du die Zwerge, die zwischen den Gerätschaften im Schatten stehen.~%\"Du weißt nicht zufällig, wo mein Schlüssel abgeblieben ist, Fremder, hm?\" fragt einer von ihnen feindselig, während ein anderer Zwerg um dich herumgeht und versucht, deinen Rucksack zu öffnen.~%\"Wie ich's mir gedacht habe!\" brüllt er triumphierend, \"Der Mensch ist ein lausiger Dieb!\"~%Du siehst, wie sie ihre Schaufeln und Spitzhacken in der Haltung verlagern. Jetzt sind es keine Werkzeuge mehr, sondern Waffen.")
		if ( !kampf(&spieler, zwerg, 1, false, ort159) ) beenden(FARBE_ROT, EXIT_SUCCESS, "Drei Zwerge gegen sich aufzubringen war wirklich eine dumme Idee. Du sinkst mit einer riesigen Schädelwunde am Hinterkopf zusammen. Du siehst nichts mehr, hörst aber das Näherkommen ihrer Schritte. In Gedanken bist du auf dem Mönchengladbacher Hauptfriedhof. Du kniest nieder am Grab deiner Großeltern und entspannst dich. DU erträgst den Schmerz der Schläge - und dann verebbt der Schmerz ganz. Du hörst nichts mehr. Du siehst nichts mehr. Du gleitest hinab in die Schwärze. Dein ENDE ist gekommen.")
		// Aus ist's mit den Zwergen
		minenzwerge = 0;
	)
	// Der Mechanismus für den Durchgang zwischen Raum 90 und Raum 91
	if ( wuerfel(6) > 3 && gitteroffen == false && (ja-oder-nein-p "In einem leeren Brandweinfaß bemerkst du einen Hebel. Als du daran ziehst merkst du, daß er nicht zufällig darin ist. Es scheint eine Art Tarnung für den Hebel zu sein. Möchtest du kräftig an dem Hebel ziehen?") ) gitteroffen = true;
	(auswahl '() "Du kannst den Tunnel zurück nach Westen gehen (1) oder die Wände nach Geheimgängen absuchen (2)", 2, ort159, ort212)
)

(defun ort-161 ()
	(rotiere-plattform)
	if ( raum != 161 ) (textausgabe "Du stehst in einer von leuchtenden Adern durchzogenen Höhle, die voller Gold und Juwelen ist. Ein Drachenhort, ganz so, wie man ihn aus den Märchen und Erzählungen der Großväter kennt. Das kleine Kind in dir jauchzt fröhlich vor Vergnügen an dem Anblick - und das Herz des Obdachlosen würde sich am liebsten alles, was es hier sieht, einstecken und zu Geld machen, wenn, ja wenn Geld noch irgendeine Bedeutung hätte. Schmerzlich wirst du dir bewußt, daß du in der Tat ein Flüchtling bist, ein Flüchtling, der vor einer unbekannten Besatzungsmacht flieht, die seine Heimatstadt mit der Hilfe seltsamer Maschinenwesen in eine Landschaft aus rauchenden Trümmern und Asche verwandelt hat.~%Du beginnst dem Berg aus Edelmetallen zu durchsuchen, nach etwas sinnvollem, etwas nützlichem. Eine Waffe wäre vielleicht interessant, aber alles, was du hier findest, sind scheinbar wertvolle Gesteinsbrocken, Juwelen, aber nichts, was dir wirklich weiter helfen würde.")
	raum = 161;
	if ( wuerfel(6) > 4 ) (textausgabe "In all den Kostbarkeiten, stößt du auf einen Zettel. Du entfaltest ihn und liest nur ein einziges Wort: \"Handschuh\"")
	if ( wuerfel(6) > 4 ) (textausgabe "Während du dich am Fuße des Horts entlangbewegst, bemerkst du ein mit Kreide an die Wand geschriebenes Wort: \"Bett\"")
	if ( wuerfel(6) > 4 ) (textausgabe "In einer Ecke hat jemand das Wort \"Brezel\" auf den Boden geritzt.")
	if ( wuerfel(6) > 4 ) (textausgabe "Am Ausgang des Horts hat jemand das Wort \"Zeit\" in Augenhöhe an die Wand geschrieben.")
	(auswahl '() "Du kannst den Hort verlassen und zurück nach Osten gehen (1) oder die Wände absuchen, ob nicht ein Geheimgang an diesen Ort führt (2)", 2, ort98, ort212)
)

(defun ort-162 ()
	(rotiere-plattform)
	raum = 162;
	(textausgabe "Du befindest dich in einem breiten Tunnel. Im Osten siehst du ein Licht, vermutlich ist es der Ausgang aus diesem Tunnel, während es in westlicher Richtung nur tiefer in die Schwärze hineinzugehen scheint.")
	(auswahl '() "Du kannst dem Tunnel nach Westen folgen (1) oder nach Osten in Richtung auf das Licht zu gehen (2). Wenn du magst, kannst du auch die Wände nach Geheimgängen absuchen (3)", 3, ort98, ort163, ort212)
)

(defun ort-163 ()
	(rotiere-plattform)
	if ( raum == 163 ) (textausgabe "Du gehst an den endlosen Reihen der Stämme entlang, hast jedoch Angst den einzigen Weg, den du durch den Pilzwald kennst, aus den Augen zu verlieren, so drehst du um und näherst dich wieder der Felswand im Westen.")
	raum = 163;
	(textausgabe "Du befindest dich an einem großen, weiten Strand, an dessen Ufer die Wellen eines Meeres branden. Weit nördlich von hier aus befindet sich ein riesiger Pilz und ein Pilzwald zieht sich am Strand entlang, soweit dein Auge reicht. Nicht weit im Westen von dir ist die Felswand die sich weit nach oben streckt. Das Meer erstreckt sich bis zum Horizont, du kannst seine Ausmaße nicht einmal erahnen. Salzig duftende, frische Brisen streichen dir durch das Gesicht. In der Westwand befindet sich ein Tunnel.")
	(auswahl '() "Du kannst dich nach Westen am Strand lang bewegen und den Tunnel im Berg betreten (1) oder du kannst dich in Richtung des Riesenpilzes nach Norden weiterbewegen (2). Wenn du willst, kannst du den Strand auch nach weiteren Wegen absuchen (3)", 3, ort162, ort156, ort210)
)

(defun ort-164 ()
	(rotiere-plattform)
	if ( (raum == 164) && schluessel9 ) {
		int wurf = wuerfel(6)
		if ( wurf == 1 ) {
			(textausgabe "\"Du schon wieder?\" lächelt der kleine Gnom dich an, dann schließt er die Augen und ignoriert dich.")
			if ( wuerfel(6) > 4 ) (textausgabe "\"Der Tempel birgt ein Geheimnis!\" lächelt der kleine Blauhäutige und wendet sich ab.")
		) else if ( wurf == 2 ) (textausgabe "\"Hat dir mein kleines Spielchen gefallen?\" fragt der Gnom und lächelt dich desinteressiert an.")
		else if ( wurf == 3 ) {
			(textausgabe "Der kleine Gnom reagiert nicht auf dich, egal wie oft du ihn ansprichst.")
			if ( wuerfel(6) > 4 ) (textausgabe "\"Laß ab von Drachen!\" lächelt der kleine Gnom und ignoriert dich und deine Fragen auch weiterhin.")
		) else if ( wurf == 4 ) (textausgabe "\"Mich nennen alle nur Papa!\" grinst der kleine Gnom, zieht an seiner Pfeife und bläst ein Rauchwölkchen aus. Es ist ein sehr kunstvolles Rauchwölkchen, das davonschwebt. Dir kommt es so vor, als würde sie eine Kutsche mit 4 Pferden davor darstellen.")
		else if ( wurf == 5 ) {
			(textausgabe "\"Wie ist das Wetter da oben?\" fragt der kleine Blaue verschmitzt.")
			if ( wuerfel(6) > 4 ) (textausgabe "\"Manchmal bleibt der Drache Sieger!\" murmelt das Männlein und bläst einen atemberaubend großen Rauchkringel in die Luft.")
		) else {
			(textausgabe "\"Hast du nicht noch etwas anderes zu tun?\" fragt das kleine Männlein verschmitzt und ignoriert all deine Worte.")
			if ( wuerfel(6) > 4 ) (textausgabe "\"Hab' Angst vor Drachen!\" sagt das Männlein weise und wendet sich von dir ab.")
		)
	)
	if ( raum == 157 ) (textausgabe "Als du aus dem Dickicht der Pilzstämme trittst, siehst du ein seltsames Männlein, das gemütlich eine Pfeife rauchend im Schatten eines Pilzhauses sitzt. Das Männlein trägt rotes Beinkleid und eine entsprechende Zipfelmütze, und hat eine bläuliche Haut. Ein weißer Rauschebart ziert sein freundliches Gesicht mit der großen, knubbeligen Nase.")
	raum = 164;
	if ( !schluessel9 && (ja-oder-nein-p "Das kleine Männlein sagt: \"Ein Rätsel habe ich für dich, löst du es, dann lohnt es sich! Möchtest du, daß ich dir mein Rätsel stelle (j/n)?\"") ) {
		(textausgabe "Der kleine Gnom lächelt dich gutmütig an.~%\"Ich habe mir eine Zahl zwischen 1 und 1000 ausgedacht. Du hast 9 Versuche, die Zahl zu erraten.\"")
		for(int i=1; i <= 9; ++i) {
            vordergrundfarbe(FARBE_GELB)
			(textausgabe "\"Was denkst du, wie lautet meine Zahl?\"")
            vordergrundfarbe(FARBE_MAGENTA)
            char eingabe[21];
            texteingabe(eingabe, 20)
            vordergrundfarbe(FARBE_WEISS)
			int geraten = atoi(eingabe)
			int zufallszahl = wuerfel(1000)
			if ( zufallszahl == geraten ) {
				schluessel9 = true;
				(textausgabe "\"Du bist sehr klug!\" lobt dich der kleine Gnome und gibt dir einen kleinen, blauen Schlüssel, der wie die Zahl 9 oder 6 geformt ist.")
				break;
			)
			if ( zufallszahl < geraten ) (textausgabe "\"Deine Zahl ist zu groß!\" lächelt der kleine Gnom.")
			if ( zufallszahl > geraten ) (textausgabe "\"Deine Zahl ist zu klein!\" schmunzelt der rauschebärtige Gnom.")
		)
	)
	(textausgabe "Der kleine Gnom lehnt sich zurück und zieht schmauchend an seiner Pfeife.")
	(auswahl '() "Du kannst den Weg zurück in nördlicher Richtung nehmen (1) oder suchen, ob du noch einen weiteren Ausgang finden kannst (2). Vielleicht möchtest du ja auch noch einen Schwatz mit dem kleinen Gnom halten (3)?", 3, ort157, ort210, ort164)
)

// ============================================
// Dwellmerdan - das Dorf der Dwellmer (Zwerge)
// ============================================
(defun ort-165 ()
	// Der Eingang zum Zwergendorf
	(if (> (w6) 4) (textausgabe "Du befindest dich an der großen Eingangsschleuse des Zwergendorfes. Die Schleuse ist so groß, das man in ihr bequem einen Zug mit fünf Waggons parken könnte - und hätte trotzdem noch genügend Platz.")
	(textausgabe "Du kannst den Wächtern in der Schleusensteuerung von Dwellmerdan ein Zeichen geben, ob du das Dorf der Dwellmer betreten möchtest, oder ob du hinaus in die Höhlen der Jagd willst.~%Zum Betreten der Dwellmersiedlung mußt du \"Melone\" in den großen Kupfertrichter in der Mitte der Schleuse sagen, um hingegen die Höhlen der Jagd zu betreten heißt das Losungswort \"Fellohne\".")
	raum = 165;
	(auswahl '() "Sagst du \"Melone\" (1) oder \"Fellohne\" (2)? in den großen Kupfertrichter?", 2, ort166, ort142)
)

(defun ort-166 ()
	// Der große Platz
	raum = 166;
	(textausgabe "Du befindest dich auf dem \"Großen Platz\", obwohl die Bezeichnung \"der einzige Platz des Dorfes\" genauso treffend wäre - und verglichen mit der Höhle des Riesenpilzes ist der große Platz und das in die Felsen gehauene Dorf der Dwellmer nun wirklich nicht der Rede wert. Wäre es das erste gewesen, was du nach deiner Rutschpartie in die unterirdische Welt gesehen hättest, wärst du vermutlich vor Ergriffenheit erzittert, aber in den letzten Tagen hast du einfach schon zuviel erlebt, zuviel gesehen und auch zuviele Schmerzen und Verletzungen erlitten. Der Platz ist über und über mit Mustern versehen, er erinnert ein wenig an die Zengärten Japans, wobei hier jedoch weniger das Bild der Leere, als vielmehr die Erzählkunst der Dwellmer in den Vordergrund gerückt wird.")
	(if (> (w6) 4) (textausgabe "Vom Großen Platz aus kannst du all die Zentralorte des Dorfes erreichen, wie etwa das Haus Haus der Feier, wo die Dwellmer es gerne ordentlich krachen lassen, oder der Arena, wo die Zwerge sich schon gegenseitig eines auf die Fünf geben.")
	(auswahl '() "Möchtest du das Dorf der Dwellmer verlassen und die Höhlen der Jagd betreten (1), ins Haus der Feier (2), ins Haus der Maschinen (3), in das Haus der Helden der Waffen (4), in das Haus der herzhaften Düfte (5), in die Arena des kochenden Blutes (6), in das Haus der Einkehr (7), in das Haus der Ältesten (8), in das Haus der Alchemiker (9) oder in das Haus der Ruhe (10). Du kannst auch einfach nur auf dem Platz herumstehen und die Zeit totschlagen (11).", 11, ort142, ort167, ort168, ort169, ort170, ort171, ort172, ort173, ort174, ort175, ort166)
)

(defun ort-167 ()
	// Das Haus der Feier
)

(defun ort-168 ()
	// Das Haus der Maschinen
)

(defun ort-169 ()
	// Das Haus des Helden der Waffen = Held = Bewahrer
)

(defun ort-170 ()
	// Das Haus der herzhaften Düfte
)

(defun ort-171 ()
	// Die Arena des kochenden Blutes
)

(defun ort-172 ()
	// Das Haus der Einkehr
)

(defun ort-173 ()
	// Das Haus des Ältesten
)

(defun ort-174 ()
	// Das Haus der Alchemiker
)

(defun ort-175 ()
	// Das Haus der Ruhe
	int n;
	if(raum == 142) { // Du erwachst nach dem Niederschlag
		(textausgabe "Als du deine Augen aufschlägst, hast du das Gefühl, dein ganzer Kopf wäre in Watte gepackt. Das Bild will sich nicht scharf stellen vor deinen Augen, ja, du hast sogar das Gefühl, als würde das Bild in verschiedene Richtungen fallen, eines für das rechte und eines für das linke Auge. Du versuchst etwas zu murmeln und nickst wieder ein.~%Beim nächsten Mal, als du die Augen öffnest, ist das Bild klarer. Vor dir siehst du eine Gestalt. Offensichtlich ist ihr Kopf nah vor deinem, aber obwohl sie den Mund bewegt, hörst du kein Wort - und auch das Gefühl, den Kopf in Zuckerwatte gebettet zu haben ist noch da. Du beschließt zu antworten, schließt die Augen um etwas Kraft zu sammeln - und bist schon wieder eingeschlafen.~%Nachdem du die Augen wieder aufgeschlagen hast, stellst du fest, daß dein Kopf sich nicht mehr wie in Watte gebettet anfühlt. Dafür spürst du jetzt einen scharfen Schmerz, der sich durch deine ganze Wirbelsäule erstreckt, als du auch nur den Versuch machst, den Kopf einen Millimeter weit zu drehen. Offensichtlich liegst du in einem Bett, einem sehr angenehmen Bett. Es fühlt sich bequem an - und trotzdem scheint es deinen Körper zu umschließen, zu fixieren. In deinen Augenwinkeln erkennst du zwei Gestalten, die sich miteinander zu unterhalten scheinen. Du strengst dich an - und kannst Worte verstehen, Worte, deren Klang dir falsch erscheint. Die Rhytmik ist irgendwie atonal - und dennoch kannst du der Unterhaltung folgen.~%\"Siehst du, siehst du! Das Kerlchen hat die Augen offen. Ich wette 5 zu 1, das er in den nächsten 2 Minuten wieder wegdöst!\" ertönt eine keckernde Stimme, während eine etwas tiefere ihr antwortet: \"Die Wette hast du schon jetzt verloren!\"~%Eines der beiden Gesichter kommt jetzt näher, füllt einen Großteil deines Gesichtsfeldes aus.~%\"Bist du wach?\" Die Stimme die du hörst ist die tiefere. Das Gesicht ist haarig, nein, behaart. Die Person hat ein irgendwie grobschlächtiges und doch freundliches bärtiges Gesicht.~%Du öffnest deinen Mund, willst antworten, aber dein Mund ist zu trocken - und so entströmt nur ein Krächzen deinen Stimmbändern. Sehr schnell ist jetzt auch die zweite Person bei dir, sie hat eine Ampulle in der Hand, aus der eine Art Glasstrohhalm hervorschaut. Sie zielt mit dem offenen Stück in deinen Mund und spritzt Wasser hinein. Das Schlucken tut weh, trotzdem bist du dankbar, bringt das Nass doch eine angenehme Kühle auf deine Stimmbänder. Das zweite Wesen hat keinen Rauschebart wie das erste, nur einen sanften Bartflaum auf den Wangen.~%\"Trink!\" flüstert dir das Wesen jetzt mit einer angenehmeren, höhren Tonlage zu. Dir gelingt es, den Mund um den Strohhalm zu schließen und daran zu saugen. Das Brennen im Hals wird mit jedem Mal weniger - und ein wohliges Gefühl breitet sich in dir aus. Du beschließt dich zu bedanken und lallst: \"Dampfe!\"~%Dann sackt das Bild vor deinen Augen weg.~%Du weißt nicht, wieviel Zeit vergangen ist, als du das nächste Mal aufwachst, aber diesmal fühlst du dich quietschlebendig. Du richtest dich auf uns sitzt im Bett. Das Wesen, augenscheinlich eine Art von Zwerg, wie diejenigen, die du schon in den Minen gesehen hast, hört auf mit ihrer Arbeit an einem Schreibtisch und steht auf, dann kommt es zu dir hinüber.~%\"Du siehst besser aus!\" sagt es zu dir.~%\"Ich fühle mich auch schon viel besser!\" antwortest du mit klarer Stimme.~%\"Das höre ich gerne\" antwortet das Wesen und tritt näher. \"Und jetzt streck doch bitte einmal deine Zunge heraus.\"~%Keine 5 Minuten später hast du eine vollständig medizinische Untersuchung hinter dir, die erste seit Jahren! Und daß das Wesen ein Arzt ist, daran hast du jetzt auch keinen Zweifel mehr, auch wenn der fehlende weiße Kittel und kein Stethoskop um den Hals doch etwas verwirrend wirken.~%\"Würdest du das bitte trinken?\" fragt dich das Wesen und reicht dir eine kleine graue Kristallflasche mit einer bräunlich aussehenden Brühe darin.")
		if((ja-oder-nein-p "Willst du die Kristallflasche annehmen und der Bitte des Arztes nachkommen, sie zu trinken?")) {
			n = wuerfel(6)
			staerkesteigerung(n, n)
			n = wuerfel(6)
			gewandheitssteigerung(n, n)
			n = wuerfel(6)
			glueckssteigerung(n, n)
		)
		else
			(textausgabe "\"Wie du meinst!\" entgegnet das Wesen, und stellt die kleine Flasche in einen Schrank zurück.")
		(textausgabe "\"Mein Name ist Arianna.\", stellt sich das Wesen vor, \"Ich bin die Heilkundige dieses Dorfs.\"~%Heilkundige? Du hättest schwören können, einen Mann vor dir zu haben. Deine Verwunderung versuchst du zu verbergen, schluckst einmal kurz und nennst ihr deinen Namen. Im Laufe der nächsten halben Stunde erklärt dir Arianna, was passiert ist in der Zeit, während du geschlafen hast - und vor allem, WARUM du geschlafen hast. Und das kam so:~%Die fünf Schlüssel, die du zusammengesucht hast, sind so etwas wie ein Notbehelf, falls die Magie des Tores einmal versagen sollte. Mit Hilfe dieser 5 Schlüssel kann man das Tor manuell öffnen, man muß nur im nächsten Augenblick zur Seite springen, da es auch eine Falle aktiviert, um ungebetene Gäste abzuhalten - und eben jene Falle hast du aktiviert. Dein Metabolismus war glücklicherweise kräftig genug, daß du die Falle überlebt hast, wenn auch zu dem Preis, daß du querschnittgelähmt warst, ein Problem, das für die Heilkundige dieses Ortes jedoch nur eine Lapalie ist. Wie es scheint haben Dwellmer, denn so nennen sich die Zwerge selber, bei ihren Tätigkeiten selber öfter einmal den Hang dazu, ihren Körper unnötig zu überlasten - und die Heilkünste Ariannas zu benötigen.~%Jetzt, wo du wach bist, kannst du dich einkleiden, neu einkleiden. Eine andere Dwellmerfrau war scheinbar so nett - und hat dir neue Kleidung geschneidert. Deine alte war etwas mitgenommen, durch die Jahre, aber auch durch die Erlebnisse der letzten Tage. Die neue Kleidung fühlt sich sehr leicht an und paßt wie angegossen, wobei ihr Stoff eine Farbe irgendwo zwischen Perl- und Cremefarben darstellt.~%Als du fertig angekleidet bist, betrachtest du dich im Spiegel. Dein Bart ist etwas gewachsen, und deine Haare sind lang, aber im Nacken zu einem Zopf zusammengeflochten, ganz wie es hier die Mode bei den Herren zu sein scheint.")
	)
	if(raum != 175)
		(textausgabe "Du befindest dich im \"Haus der Ruhe\", einer Art Haus der Erholung im Dorf der Dwellmer. Hier geht man hin, wenn das eigene Wohlbefinden angegriffen ist - oder man einfach nur Ruhe vor den Pflichten des Alltags oder den anderen Dwellmern benötigt.")
	if(raum == 175) {
		(if (> (w6) 5) {
			(textausgabe "Du suchst einen der Ruheräume auf, wo du dich entkleidest und die Sauna betrittst. Als du lässig an der Wand lehnst, betritt Arianna den Raum, nur ein Handtuch lässig über die Schulter geworfen. Sie begrüßt dich und fragt, ob sie sich dem Saunagang anschließen darf.")
		       if((arianna >= 40) || (ja-oder-nein-p "Bist du dazu bereit, den Saunagang mit Arianna gemeinsam zu machen(j/n)?")) {
				       (textausgabe "Du hast nichts dagegen und ihr verbringt die nächste Stunde mit einer lockeren Plauderei und noch mehr Schwitzen. Nach dem Saunagang erhältst du von ihr sogar noch eine Ganzkörpermassage. Du fühlst dich so wohl, wie noch nie in deinem Leben.")
				       zweisamkeit(1)
			) else {
				(textausgabe "Du verneinst, da du dich zu erschöpft fühlst. Sie nickt verstehend und verläßt den Saunaraum. Es ist ja nicht so, als wäre dies die einzige Sauna im Haus der Ruhe.")
				zweisamkeit(-1) 
			)
		) else 
			(textausgabe "Du suchst einen der Ruheräume auf, wo du zuerst einen Saunagang machst und dich anschließend auf einem Massagesessel ausruhst. Als du einige Stunden später wieder aufwachst, fühlst du dich vollkommen erfrischt.")
		spieler.staerke = spieler.staerke_start;
		spieler.gewandheit = spieler.gewandheit_start;
		spieler.glueck = spieler.glueck_start;
	)
	raum = 175;
	(auswahl '() "Du kannst hier eine Verschnaufpause einlegen (1) oder das Haus der Ruhe verlassen (2).", 2, ort175, ort166)
)

(defun ort-176 ()
)

(defun ort-177 ()
)

(defun ort-178 ()
)

(defun ort-179 ()
)

(defun ort-180 ()
)

(defun ort-181 ()
)

(defun ort-182 ()
)

(defun ort-183 ()
)

(defun ort-184 ()
)

(defun ort-185 ()
)

(defun ort-186 ()
)

(defun ort-187 ()
)

(defun ort-188 ()
)

(defun ort-189 ()
)

(defun ort-190 ()
)

(defun ort-191 ()
)

(defun ort-192 ()
)

(defun ort-193 ()
)

(defun ort-194 ()
)

(defun ort-195 ()
)

(defun ort-196 ()
)

(defun ort-197 ()
)

(defun ort-198 ()
)

(defun ort-199 ()
)

// -----------
// --- 200 ---
// -----------

(defun ort-200 ()
)

(defun ort-201 ()
)

(defun ort-202 ()
)

(defun ort-203 ()
)

(defun ort-204 ()
)

(defun ort-205 ()
)

(defun ort-206 ()
)

(defun ort-207 ()
)

(defun ort-208 ()
)

(defun ort-209 ()
)

(defun ort-210 ()
	// Die Untersuchung der Pilze in der Pilzwaldhöhle ruft diese Funktion auf.
	int wurf = wuerfel(6)
	if ( wurf == 1 ) (textausgabe "Du berührst einen großen Pilzstamm. Unter der Wärme deiner Hand fühlt es sich an, als würde sein Stamm anfangen zu pulsieren.")
	else if ( wurf == 2 ) (textausgabe "Der Pilzstamm fühlt sich warm an, während du mit deiner Hand darüber gehst.")
	else if ( wurf == 3 ) (textausgabe "Du glaubst zu hören, das hinter einer undurchdringlichen Wand aus Pilzen das Geräusch eines fließenden Gewässers erklingt.")
	else if ( wurf == 4 ) (textausgabe "Während du den Pilzhut eines mannshohen Pilzes genauer untersuchst, kannst du dich des Gefühls nicht erwehren, das etwas dich beobachtet.")
	else if ( wurf == 5 ) (textausgabe "Als du den Stamm eines baumhohen Pilzes abklopfst, kommt es dir so vor, als wäre klänge er hohl. Möglicherweise ist der Stamm des Pilzes ja hohl?")
	else (textausgabe "Du kannst absolut nichts ungewöhnliches wahrnehmen.")
	wurf = wuerfel(4)
	if ( wurf == 1 ) (textausgabe "Vielleicht ist es nur Einbildung, aber du hattest gerade das Gefühl, einen Windstoß im Nacken zu spüren.")
	else if ( wurf == 2 ) (textausgabe "Für einen Moment glaubtest du, ein knackendes Geräusch gehört zu haben.")
	else if ( wurf == 3 ) (textausgabe "Es kommt dir so vor, als wäre es still hier, sehr still, ja, unnatürlich still. Die Stille erscheint dir zu konsequent, als das sie natürlich erscheint.")
	else (textausgabe "Ein ungutes Gefühl breitet sich in deiner Magengrube aus.")
	// 20% Wahrscheinlichkeit einer Zufallsbegegnung.
	if(wuerfel(10) > 6) ort211()
	else raumptr[raum]() // weiter geht's im Spiel in Raum [raum]
)

(defun ort-211 ()
	// Begegnung mit einem Zufallsgegner der oberen Hohlwelt
	charakter_s gegner[] = {
		{ "gefiederte Schnecke", 6, 6, 3, 3 ),
		{ "Riesenborkenkäfer", 6, 6, 3, 3 ),
		{ "gigantische Pilzlaus", 3, 3, 4, 4 ),
		{ "wandernder Riesensteinpilz", 5, 5, 4, 4 ),
		{ "Sporenkrabbe", 6, 6, 9, 9 ),
		{ "fleischfressender Pilzaal", 7, 7, 4, 4 ),
		{ "zu groß geratenes Frettchen", 9, 9, 8, 8 )
	);
	bool kampfausgang = false;	
	int zufallsgegner = wuerfel(7)
	if ( zufallsgegner == 1 ) {
		(textausgabe "Um einen Pilzstamm herum kommt eine Schnecke gekrochen, eine Schnecke, die den Hut eines Pilzes als Haus auf dem Rücken trägt. Plötzlich richtet sie sich auf. Ihr Rücken hat ein Gefieder, das sie ausbreitet, während hier Mund sich weit öffnet, rasiermesserscharfe Zähne zeigt - und ihre Augen sich dir zuwenden.")
		kampfausgang = kampf(&spieler, &gegner[0], 1, false, NULL)
	) else if ( zufallsgegner == 2 ) {
		(textausgabe "Ein Borkenkäfer, in der größe eines Ponys kommt herabgeschwebt und läßt sich vor dir auf dem Boden nieder, augenscheinlich um zu - fressen?")
		(if (> (w6)4) (textausgabe "Als du dein Gewicht verlagerst, trittst du auf einen kleinen Pilz der jämmerlich zu schreien anfängt. Damit hast du nun die ungeteilte Aufmerksamkeit des Borkenkäfers erlangt.")
		kampfausgang = kampf(&spieler, &gegner[1], 1, false, NULL)
	) else if ( zufallsgegner == 3 ) {
		(textausgabe "Am Stamm eines größeren Pilzes kommt eine Laus heruntergekrabbelt. Ihr Kopf bewegt sich, als würde sie etwas riechen.")
		(if (> (w6) 4) (textausgabe "Und wenn man die Richtung ihrer Bewegung weiter verfolgt, weiß man auch, was sie riecht: Dich!")
		kampfausgang = kampf(&spieler, &gegner[2], 1, false, NULL)
	) else if ( zufallsgegner == 4 ) {
		(if (> (w6) 4) (textausgabe "Du hast einen Riesensteinpilz erschreckt. Er dreht sich um und kommt schweren Schrittes auf dich zu.")
		kampfausgang = kampf(&spieler, &gegner[3], 1, false, NULL)
	) else if ( zufallsgegner == 5 ) {
		(if (> (w6) 4) (textausgabe "Du hast gerade einer Sporenkrabbe auf die Schere getreten.")
		else (textausgabe "Du hast gerade einer Sporenkrabbe auf ein Bein getreten.")
		kampfausgang = kampf(&spieler, &gegner[4], 1, false, NULL)
	) else if ( zufallsgegner == 6 ) {
		(if (> (w6) 3) (textausgabe "Die Vibration deines Klopfens an einen übergroßen Pilzstamm hat eine Pilzaal hervorgelockt. Die großen, scharfen Zähne in seinem weit aufgerissenen Maul lassen darauf schließen, daß es sich bei ihm um einen Fleischfresser handelt. Und seinem Blick nach zu urteilen handelt es sich bei dir um ein Mittagessen in seinen Augen.")
		else (textausgabe "Die Vibration deines Klopfens an einen übergroßen Pilzstamm hat einen wütenden Pilzaal hervorgelockt. Bei deinem Anblick reißt er sein Maul weit auf und präsentiert dir eine paar hübsche Reihen rasiermesserscharfe Zähne, die dich sehr an das Maul eines großen weißen Hais erinnern. Du könntest schwören, gerade in deinem Kopf eine Stimme vernommen zu haben, die sagte: \"Hallo Abendessen!\"")
		kampfausgang = kampf(&spieler, &gegner[5], 1, false, NULL)
	) else {
		(textausgabe "Wie aus dem Nichts heraus, stürmt plötzlich ein ziemlich großes Frettchen auf dich zu.")
		kampfausgang = kampf(&spieler, &gegner[6], 1, false, NULL)
	)
	if( kampfausgang ) {
		getoetetegegner += 1;
		raumptr[raum]() // weiter geht's im Spiel in Raum [raum]
	)
	else beenden(FARBE_ROT, EXIT_SUCCESS, "Das war nicht dein bester Kampf. Um ehrlich zu sein, das war dein schlechtester Kampf - und auch dein letzter Kampf. Dein allerletzter Kampf, den du nicht überlebt hast. Mit dir ist es zu ENDE gegangen.")
)

(defun ort-212 ()
	// Die Suche nach Geheimgängen im ersten Level der Hohlwelt ruft diese Funktion auf.
	int wurf = wuerfel(6)
	if ( wurf == 1 ) (textausgabe "Du hämmerst die Felsen ab, aber von Hohlräumen ist nichts zu hören.")
	else if ( wurf == 2 ) (textausgabe "Die Wand fühlt sich warm an, während du mit deiner Hand darüber gehst.")
	else if ( wurf == 3 ) (textausgabe "Du könntest schwören, hinter der Wand das Geräusch fließenden Wassers hören zu können.")
	else if ( wurf == 4 ) (textausgabe "Während du die Wände abtastest, hast du das Gefühl, beobachtet zu werden.")
	else if ( wurf == 5 ) (textausgabe "Es kommt dir so vor, als würdest sich dein Klopfen etwas hohl anhören. Vielleicht befindet sich ja ein Hohlraum hinter der Wand?")
	else (textausgabe "Du kannst absolut nichts ungewöhnliches wahrnehmen.")
	wurf = wuerfel(4)
	if ( wurf == 1 ) (textausgabe "Vielleicht ist es nur Einbildung, aber du glaubst gerade ganz deutlich, einen Luftzug gespürt zu haben.")
	else if ( wurf == 2 ) (textausgabe "Für einen Moment glaubtest du, ein Geräusch gehört zu haben.")
	else if ( wurf == 3 ) (textausgabe "Es kommt dir so vor, als wäre es unnatürlich still hier unten.")
	else if ( wurf == 4 ) (textausgabe "Ein ungutes Gefühl breitet sich in deinem Magen aus.")
	// 20% Wahrscheinlichkeit einer Zufallsbegegnung.
	if(wuerfel(10) > 8) ort213()
	else raumptr[raum]() // weiter geht's im Spiel in Raum [raum]
)

(defun ort-213 ()
	// Begegnung mit einem Zufallsgegner der oberen Hohlwelt
	charakter_s gegner[] = {
		{ "wandernder Pilz", 2, 2, 3, 3 ),
		{ "Riesenraupe", 6, 6, 3, 3 ),
		{ "Feuerschmetterling", 6, 6, 4, 4 ),
		{ "verschreckte Fledermaus", 5, 5, 4, 4 ),
		{ "Geröllnatter", 6, 6, 5, 5 ),
		{ "Felsegel", 8, 8, 4, 4 ),
		{ "Felsenkrebs", 6, 6, 8, 8 )
		);
	bool kampfausgang = false;
	int zufallsgegner = wuerfel(7)
	if ( zufallsgegner == 1 ) {
		(textausgabe "Du hast einen wandernden Riesenpilz angelockt.")
		kampfausgang = kampf(&spieler, &gegner[0], 1, false, NULL)
	) else if ( zufallsgegner == 2 ) {
		(textausgabe "Aus einem Loch in der Wand kommt eine Riesenraupe herangekrochen.")
		(if (> (w6)4) (textausgabe "Offenbar hat dein ständiges Geklopfe und Geschabe sie aufgeschreckt.")
		kampfausgang = kampf(&spieler, &gegner[1], 1, false, NULL)
	) else if ( zufallsgegner == 3 ) {
		(textausgabe "Ein riesiger roter Schmetterling kommt auf dich zugeschwebt. Das illusorische Farbspiel auf seinen Flügel läßt sie aussehen, als wären sie aus Feuer.")
		kampfausgang = kampf(&spieler, &gegner[2], 1, false, NULL)
	) else if ( zufallsgegner == 4 ) {
		(if (> (w6) 4) (textausgabe "Du hast eine Fledermaus aufgeschreckt")
		kampfausgang = kampf(&spieler, &gegner[3], 1, false, NULL)
	) else if ( zufallsgegner == 5 ) {
		(if (> (w6) 4) (textausgabe "Du hast gerade einer Felsennatter auf den Kopf gehauen.")
		else (textausgabe "Die Vibration deines Klopfens hat eine Geröllnatter herbeigelockt.")
		kampfausgang = kampf(&spieler, &gegner[4], 1, false, NULL)
	) else if ( zufallsgegner == 6 ) {
		(if (> (w6) 3) (textausgabe "Ein Felsegel schmiegt sich zärtlich um dein Bein.")
		else (textausgabe "Wie aus dem Nichts heraus, windet sich plötzlich ein Felsegel um deine Hand.")
		kampfausgang = kampf(&spieler, &gegner[5], 1, false, NULL)
	) else {
		(textausgabe "Der Fels, auf den du gerade klopfst, war ein Felsenkrebs.")
		kampfausgang = kampf(&spieler, &gegner[6], 1, false, NULL)
	)
	if ( kampfausgang ) {
		getoetetegegner += 1;
		raumptr[raum]() // weiter geht's im Spiel in Raum [raum]
	) else        beenden(FARBE_ROT, EXIT_SUCCESS, "Das war nicht dein bester Kampf. Um ehrlich zu sein, das war dein schlechtester Kampf - und auch dein letzter Kampf. Dein allerletzter Kampf, den du nicht überlebt hast. Mit dir ist es zu ENDE gegangen.")
)

(defun ort-214 ()
)

(defun ort-215 ()
)

(defun ort-216 ()
)

(defun ort-217 ()
)

(defun ort-218 ()
)

(defun ort-219 ()
)

(defun ort-220 ()
)

(defun ort-221 ()
)

(defun ort-222 ()
)

(defun ort-223 ()
)

(defun ort-224 ()
)

(defun ort-225 ()
)

(defun ort-226 ()
)

(defun ort-227 ()
)

(defun ort-228 ()
)

(defun ort-229 ()
)

(defun ort-230 ()
)

(defun ort-231 ()
)

(defun ort-232 ()
)

(defun ort-233 ()
)

(defun ort-234 ()
)

(defun ort-235 ()
)

(defun ort-236 ()
)

(defun ort-237 ()
)

(defun ort-238 ()
)

(defun ort-239 ()
)

(defun ort-240 ()
)

(defun ort-241 ()
)

(defun ort-242 ()
)

(defun ort-243 ()
)

(defun ort-244 ()
)

(defun ort-245 ()
)

(defun ort-246 ()
)

(defun ort-247 ()
)

(defun ort-248 ()
)

(defun ort-249 ()
)

(defun ort-250 ()
)

(defun ort-251 ()
)

(defun ort-252 ()
)

(defun ort-253 ()
)

(defun ort-254 ()
)

(defun ort-255 ()
)

(defun ort-256 ()
)

(defun ort-257 ()
)

(defun ort-258 ()
)

(defun ort-259 ()
)

(defun ort-260 ()
)

(defun ort-261 ()
)

(defun ort-262 ()
)

(defun ort-263 ()
)

(defun ort-264 ()
)

(defun ort-265 ()
)

(defun ort-266 ()
)

(defun ort-267 ()
)

(defun ort-268 ()
)

(defun ort-269 ()
)

(defun ort-270 ()
)

(defun ort-271 ()
)

(defun ort-272 ()
)

(defun ort-273 ()
)

(defun ort-274 ()
)

(defun ort-275 ()
)

(defun ort-276 ()
)

(defun ort-277 ()
)

(defun ort-278 ()
)

(defun ort-279 ()
)

(defun ort-280 ()
)

(defun ort-281 ()
)

(defun ort-282 ()
)

(defun ort-283 ()
)

(defun ort-284 ()
)

(defun ort-285 ()
)

(defun ort-286 ()
)

(defun ort-287 ()
)

(defun ort-288 ()
)

(defun ort-289 ()
)

(defun ort-290 ()
)

(defun ort-291 ()
)

(defun ort-292 ()
)

(defun ort-293 ()
)

(defun ort-294 ()
)

(defun ort-295 ()
)

(defun ort-296 ()
)

(defun ort-297 ()
)

(defun ort-298 ()
)

(defun ort-299 ()
)

// -----------
// --- 300 ---
// -----------

(defun ort-300 ()
)

(defun ort-301 ()
)

(defun ort-302 ()
)

(defun ort-303 ()
)

(defun ort-304 ()
)

(defun ort-305 ()
)

(defun ort-306 ()
)

(defun ort-307 ()
)

(defun ort-308 ()
)

(defun ort-309 ()
)

(defun ort-310 ()
)

(defun ort-311 ()
)

(defun ort-312 ()
)

(defun ort-313 ()
)

(defun ort-314 ()
)

(defun ort-315 ()
)

(defun ort-316 ()
)

(defun ort-317 ()
)

(defun ort-318 ()
)

(defun ort-319 ()
)

(defun ort-320 ()
)

(defun ort-321 ()
)

(defun ort-322 ()
)

(defun ort-323 ()
)

(defun ort-324 ()
)

(defun ort-325 ()
)

(defun ort-326 ()
)

(defun ort-327 ()
)

(defun ort-328 ()
)

(defun ort-329 ()
)

(defun ort-330 ()
)

(defun ort-331 ()
)

(defun ort-332 ()
)

(defun ort-333 ()
)

(defun ort-334 ()
)

(defun ort-335 ()
)

(defun ort-336 ()
)

(defun ort-337 ()
)

(defun ort-338 ()
)

(defun ort-339 ()
)

(defun ort-340 ()
)

(defun ort-341 ()
)

(defun ort-342 ()
)

(defun ort-343 ()
)

(defun ort-344 ()
)

(defun ort-345 ()
)

(defun ort-346 ()
)

(defun ort-347 ()
)

(defun ort-348 ()
)

(defun ort-349 ()
)

(defun ort-350 ()
)

(defun ort-351 ()
)

(defun ort-352 ()
)

(defun ort-353 ()
)

(defun ort-354 ()
)

(defun ort-355 ()
)

(defun ort-356 ()
)

(defun ort-357 ()
)

(defun ort-358 ()
)

(defun ort-359 ()
)

(defun ort-360 ()
)

(defun ort-361 ()
)

(defun ort-362 ()
)

(defun ort-363 ()
)

(defun ort-364 ()
)

(defun ort-365 ()
)

(defun ort-366 ()
)

(defun ort-367 ()
)

(defun ort-368 ()
)

(defun ort-369 ()
)

(defun ort-370 ()
)

(defun ort-371 ()
)

(defun ort-372 ()
)

(defun ort-373 ()
)

(defun ort-374 ()
)

(defun ort-375 ()
)

(defun ort-376 ()
)

(defun ort-377 ()
)

(defun ort-378 ()
)

(defun ort-379 ()
)

(defun ort-380 ()
)

(defun ort-381 ()
)

(defun ort-382 ()
)

(defun ort-383 ()
)

(defun ort-384 ()
)

(defun ort-385 ()
)

(defun ort-386 ()
)

(defun ort-387 ()
)

(defun ort-388 ()
)

(defun ort-389 ()
)

(defun ort-390 ()
)

(defun ort-391 ()
)

(defun ort-392 ()
)

(defun ort-393 ()
)

(defun ort-394 ()
)

(defun ort-395 ()
)

(defun ort-396 ()
)

(defun ort-397 ()
)

(defun ort-398 ()
)

(defun ort-399 ()
)

(defun ort-400 ()
)

