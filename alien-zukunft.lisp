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



(defparameter *spieler* (make-hash-table)
  "Die Charakterwerte des Spielers")



;;; ----------------------------
;;;  Allgemeine Hilfsfunktionen
;;; ----------------------------



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



(defun trennsymbolep (c) (position c " ,.;/"))



(defun string-aufteilen (string &key (trennzeichenp #'trennsymbolep))
  (loop :for beg = (position-if-not trennzeichenp string)
	 :then (position-if-not trennzeichenp string :start (1+ end))
	 :for end = (and beg (position-if trennzeichenp string :start beg))
	 :when beg :collect (subseq string beg end)
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



(defun kampf (gegner &optional treffer-verboten flucht)
  (flet ((kampfrunde (gegner)
		   (let ((g1 (+ (spieler 'gewandheit) (w6 2) (spieler 'angriffsbonus)))
				 (g2 (+ (third gegner) (w6 2))))
			 (when (ereignis 'unsichtbar)
			   (incf g1 2))
			 (when (= g1 g2)
			   (return-from kampfrunde 't)) ; Unentschieden, nicht getroffen
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
  (do (ende
	   (lst (list #'ort-1)))
	  (ende
	   (textausgabe "Ich hoffe sehr, es hat dir Spaß gemacht!"))
	(push (funcall (first lst)) lst)
	(when (eql (first lst) 'ende)
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
  (case (zahlen-auswahl 2 "Wirst du auf die winkende Gestalt zugehen (1) oder ziehst du es vor, dich lieber scheu und mit knurrendem Magen aus dem Staub zu machen (2)?")
	(1 #'ort-2)
	(2 #'ort-3)))



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
  (case (zahlen-auswahl 3 "Ignorierst du das Geheul, das Donnern und die Blitze und wirst  erst einmal in Ruhe Frühstücken (1) oder würdest du lieber Elke fragen, ob sie dich rauslassen kann, damit du einmal nach dem Rechten sehen kannst (2)? Vielleicht würdest du sie ja auch lieber fragen, ob sie über einen anderen Empfänger verfügt, der nicht an das Stromnetz angeschlossen werden muss (3)?")
	(1 #'ort-4)
	(2 #'ort-5)
	(3 #'ort-6)))



(defun ort-3 ()
  (textausgabe "Du bist ein äußerst mißtrauischer Mensch und vertraust der unbekannten, winkenden Gestalt nicht. Zu deinem Glück fängt es an zu regnen, eine hervorragende Möglichkeit um sich zu verpissen. Leider ist es aber ein echter Platzregen, daher solltest du möglichst schnell ein Dach über den Kopf bekommen. Du rennst ein Stück weit die Hindenburgstraße hinab und biegst in den Lichthof ein, wo du dich während des Regens unterstellts.")
  (textausgabe "Kaum bist du eine Minute drin, als plötzlich überall die Sirenen anfangen zu heulen. Draußen hörst du ein lautes Donnergrollen. Auch ist es viel dunkler geworden, seit die dichten grauen Wolken sich mit ihrem Regen auf Gladbach stürzen. Plötzlich pfeift ein Schuß an dir vorbei und schlägt in die Fensterscheibe ein Stück weit vor dir ein.")
  (textausgabe "Schneller, als du selbst es für möglich gehalten hast, hechtest du in den nächsten Eingang und siehst zurück. Du bist nicht alleine. Von oben herab kommt eine Gestalt gelaufen. Aus deinem Rucksack holst du das Taschenmesser heraus und klappst es auf. Fast ist es so, als wärst du unbewaffnet, aber eben nur fast. Dein Gegner kommt weiter auf dich zugeeilt.")
  (when (eql (ort-13) 'ende)
	(return-from ort-3 'ende))
  (textausgabe "Es war zwar Notwehr, dennoch hockst du jetzt hier neben einer Leiche. Dein Gefühl sagt dir, daß es wohl das Beste wäre, dich so schnell wie möglich aus dem Staub zu machen, Unwetter hin oder her.")
  (case (zahlen-auswahl 2 "Möchtest du den Lichthof nach Norden in Richtung Kaiserstraße verlassen (1), oder nach Süden in Richtung Hindernburgstraße (2)?")
	(1 #'ort-50)
	(2 #'ort-51)))



(defun ort-4 ()
  (textausgabe "Du setzt dich zu Elke an den Tisch. Das Wetter, den Donner und den fehlenden Strom ignorieren, lernt ihr euch langsam besser kennen, während die Anzahl der Backwaren auf dem Tablett deutlich schrumpfen und der heiße Kaffee aus der Thermoskanne sich wohlig in deinem Inneren verteilt. Der Ort, die Zeit, die Situation, alles könnte man als idyllisch bezeichnen, wenn, ja wenn nicht auf einmal dieses seltsame Geräusch eine absterbenden Sirene gewsen wäre. Es war kein Abschwellen, wie man es kenn, sondern klang eher, wie ein entenartiger Aufschrei.")
  (textausgabe "Und dann nimmst du plötzlich wahr, das mit dem Verstummen dieser Sirene, die Masse an Sirenen, die noch klingen, weniger hoch ist, als zuvor. Aber was viel wichtiger ist, einige gehen immer noch. Langsam wirst du dir der Situation bewußt, die da draußen herrscht - und du beschließt, nachsehen zu gehen, was da los ist.")
  #'ort-5)



(defun ort-5 ()
  (textausgabe "Du schlägst vor, daß du dich draußen umsiehst und zurückkommst, sobald du weißt, was los ist. Elke begleitet dich zur Ladentüre und läßt dich raus. Der Regen prasselt von oben herab und immer wieder donnert es. Du winkst ihr kurz zu und rennst so gut es geht an den Hauswänden entlang die Hindenburgstraße hinauf, trotzdem bist du nach weniger als einer Minute bis auf die Unterhose durchnäßt. Als du am ehemaligen Heinemann vorbeikommst und durch die kurze Passage läuftst, bemerkst du an der Straßenecke zum Sankt Vith hinunter einen brennenden Polizeiwagen. Ein mulmiges Gefühl geht dir durch den Magen. Eigentlich wolltest du ja in das Haus, das früher deinem Großvater gehört hat - und von dem aus man eine Übersicht über die ganze Stadt hat. Trotzdem ergreift dich gerade die Angst.")
  (case (zahlen-auswahl 3 "Vielleicht wäre es ja besser, die Straße wieder hinunter zulaufen - und sich im Geschäft bei Elke zu verstecken (1)? Du könntest auch auf den Polizeiwagen zulaufen, vielleicht kannst du jemandem helfen, der im Wagen eingeklemmt ist (2)? Natürlich kannst du auch deinem ursprünglichen Plan weiter verfolgen,, das Haus deines Großvaters zu erreichen (3)!")
	(1 #'ort-7)
	(2 #'ort-8)
	(3 #'ort-9)))



(defun ort-6 ()
  (textausgabe "Elke schüttelt den Kopf, und verneint deine Frage.")
  (textausgabe "\"Tut mir leid, so etwas ist nicht hier im Geschäft. Das einzige Radio, das mir einfallen würde, wäre mein Autoradio.\" entgegnet sie, nimmt sich ein warmes Brötchen vom Tablett und kaut daran herum.")
  (case (zahlen-auswahl 3 "Willst du sie wirklich überreden, mit dir zu ihrem Auto zu gehen (1), oder würdest du jetzt nicht viel lieber frühstücken (2)? Möchtest du hingegen unbedingt draußen nachsehen was los ist, könntest du Elke auch bitten, dich rauszulassen (3).")
	(1 #'ort-17)
	(2 #'ort-4)
	(3 #'ort-5)))



(defun ort-7 ()
  (textausgabe "Vollkommen durchnäßt kommst du wieder am Geschäft an. Drinnen ist alles dunkel. Du klopfst mehrfach, aber nichts rührt sich.")
  (case (zahlen-auswahl 3 "Willst du es weiter mit klopfen und rufen probieren (1), oder willst du versuchen, ob du die Türe öffnen kannst (2), oder aber möchtest du dir einen anderen Weg suchen (3)?")
	(1 #'ort-14)
	(2 #'ort-15)
	(3 #'ort-16)))



(defun ort-8 ()
  (if (plusp (inventar 'gewehr))
	  (progn
		(textausgabe "Du läufst erneut über den Platz um dir den Polizeiwagen noch einmal anzusehen. Das Feuer ist erloschen. Du überlegst dir gerade, ob du eine noch gründlichere Durchsuchung des Wagens vornehmen sollst, als du ein Geräusch aus der Richtung der Kirche hören kannst. Ein Geschoss schlägt in die Karosserie des Wagens ein.")
		(case (zahlen-auswahl 2 "Willst du dich dem Kampf stellen (1) oder versuchen zu fliehen (2)?")
		  (1 #'ort-10)
		  (2 #'ort-11)))
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
		  (case (zahlen-auswahl 2 "Willst du jetzt weiter zum Haus deines Großvaters (1) oder zurück zum Geschäft (2)?")
			(1 #'ort-9)
			(2 #'ort-7))))))



(defun ort-9 ()
  (if (ereignis 'dreistelzer)
	  (progn
		(textausgabe "Der Kampf ist nicht spurlos am Haus deines Großvaters vorbeigezogen. Die Fenster sind zersplittert, die Haustüre liegt am Boden, die Wände rußgeschwärzt.")
		(case (zahlen-auswahl 2 "Willst du in das Haus deines Großvaters hinein (1) oder versuchen, um die Ecke zu laufen und den Kapuzinerplatz zu erreichen (2)?")
		  (1 #'ort-21)
		  (2 #'ort-28)))
	  (if (ereignis 'dreistelzer-gesehen)
		  (progn
			(textausgabe "Du läufst auf das Haus deines Großvaters zu und in den Eingang zwischen Gaststätte und Kleidungsgeschäft. Für einen Moment mußt du vor der alten Holztüre mit der Glasscheibe halten. Mit Hilfe deines Taschenmessers daudert es nur ein paar Sekunden, dann ist die Türe öffnet und du schlüpfst hinein.")
			#'ort-21)
		  (progn
			(textausgabe "Du läufst an der Häuserwand des Alten Marktes entlang und gelangst schließlich zum Haus deines Großvaters. Du machst dich mit Hilfe deines Taschenmessers kurz am Schloß der alten Holztüre zu schaffen, sie geht fast sofort aus. Den Trick hast du schon als Kind gemacht, wenn du mal wieder deinen Schlüssel zu Hause vergessen hattest - und er klappt immer noch wunderbar. Du hastest die Türe hinauf. Immer noch donnert es draußen, so laut, wie du es schon lange bei keinem Gewitter mehr gehört hast. Auf jeder Etage drückst du den Lichtschalter, aber keiner schaltet das Licht an. Auch hier ist vollkommener Stromausfall. Kurz bevor du die oberste Etage erreichst, hören die Sirenen auf zu heulen, was für dich nur zum Vorteil sein kann, steht doch noch eine alte motorbetriebene E57. Du obersten Treppen sind am kürzesten und am verwinkelsten. Links die Waschküche läßt du liegen, da kannst du nichs sehen. Du stürmst nach rechts, den kurzen Gang entgang und reißt die hintereste Türe auf der rechten Seite auf. Du stürmst auf das Dachfenster an der Südseite zu. Überall siehst du dunkelgraue Wolkenberge sich auftürmen. Statt bis nach Düsseldorf kannst du nicht einmal bis an die Westgrenze Mönchengladbachs sehen. Du wendest den Blick ab und läufst zu einem der Fenster an der Nordseite. Hier bietet sich die ein gleiches Bild. Die Wolken sind so dicht, daß du nicht einmal den Gladbacher Wasserturm in dieser Brühe sehen kannst. Bleiben noch die Fenster an der Südseite. Bereits als du dich ihnen näherst, erkennst du, daß du hier ein weiteres Sichtfeld haben wirst. Du reißt das Fenster auf um besser sehen zu können. Von oben peitschen dicke Regentropen herab, aber das aufgeklappte Fenster schützt dich weitestgehend. Die Wolkenwand ist hier einige Kilometer entfernt. Da plötzlich wird es hell in der Wolkenwand. Wie gebannt starrst du dahin. War das ein Blitz? Da wieder. Wieder ein Blitz. Wieder in der Wolkenwand. Das ist jetzt aber sehr ungewöhnlich. Minutenlang starrst du auf die Wolkenwand - und auch an zwei oder drei anderen Stellen erblickst du immer wieder kurze Blitze - mitten in der Wolkenwand, aber fast auf Bodenhöhe. Ein mulmiges Gefühl breitet sich in deinem Magen aus. Gerade, als du das Fenster schließen willst, kommt etwas aus der Wolke. Etwas riesiges. Ein Objekt, wie ein Turm, aber es bewegt sich, wie ein Stelzenläufer. Und ein Lichtbogen, wie ein Blitz spannt sich von ihm aus, dann brennt etwas zu seinen - Beinen? - während es sich weiter in Richtung der Gladbacher Innenstadt voranschiebt.")
			(textausgabe "Deine Nackenhärchen haben sich aufgerichtet. Du weißt zwar nicht genau, was das ist, aber es bringt Zerstörung, soviel ist sicher. Hastig schließt du das Fenster. Du rennst aus dem Dachstuhl heraus zurück in den Flur und eilst die Treppen hinab, bis du unten an den der Haustüre ankommst. Was wirst du tun?")
	(ereignis 'dreistelzer-gesehen 't)
	(case (zahlen-auswahl 3 "Du läufst hinaus und zu dem Polizeiwagen (1), du läufst die Kellertreppe hinab und suchst dort Schutz vor dem was kommt (2) oder du läufst zurück zu Elke, der Frau, die dich täglich mit Backwaren versorgt hat und erzählst ihr, was du gesehen hast (3)?")
	  (1 #'ort-8)
	  (2 #'ort-20)
	  (3 #'ort-7))))))



(defun ort-10 ()
  (when (eql (ort-13) 'ende)
	(return-from ort-10 'ende))
  (textausgabe "Du bist nicht stolz darauf, einen anderen Menschen getötet zu haben, aber du warst ja nicht der Angreifer. Trotzdem fühlst du dich schäbig. Etwas in dir hat sich verändert, das kannst du spüren. Noch immer prasselt der Regen auf dich, so als wäre nichts gewesen. Und hinter den Wolkenbergen, da bist du dir sicher, scheint immer noch die Sonne.")
  (case (zahlen-auswahl 2 "Willst du jetzt weiter zum Haus deines Großvaters (1) oder zurück zum Geschäft (2)?")
	(1 #'ort-9)
	(2 #'ort-7)))



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
  (case (zahlen-auswahl 3 "Willst du es weiter mit klopfen und rufen probieren (1), oder willst du versuchen, ob du die Türe öffnen kannst (2), oder willst du dir einen anderen Weg suchen (3)?")
	(1 #'ort-14)
	(2 #'ort-15)
	(3 #'ort-16)))



(defun ort-15 ()
  'ende)

(defun ort-16 ()
  'ende)

(defun ort-17 ()
  'ende)

(defun ort-20 ()
  'ende)

(defun ort-21 ()
  'ende)

(defun ort-28 ()
  'ende)

(defun ort-50 ()
  'ende)

(defun ort-51 ()
  'ende)
