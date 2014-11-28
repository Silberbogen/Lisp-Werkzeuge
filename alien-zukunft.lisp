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


(defun nur-ziffern (text)
  "Entfernt die Nicht-Ziffern eines Textes."
  (remove-if #'(lambda (string) (not (digit-char-p string)))
			 text))


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
	 (auswahl 'ort-74 ort-68 ort-82 ort-67) ("Du kannst dem grünlich erleuchteten Tunnel folgen (1), dem rötlich erleuchteten Tunnel (2), dem gelblich erleuchteten Ausgang (3) oder stehenbleiben und warten (4)"))
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


(defun ort-100 ()
  'ende)

(defun ort-101 ()
  'ende)

(defun ort-113 ()
  )
