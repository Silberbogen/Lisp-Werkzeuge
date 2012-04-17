;-*-lisp-*-
;-*- coding: utf-8 -*-

;;; Dateiname: ssp.lisp
;;; Beschreibung: Eine kleine Umsetzung des Spiels Schere-Stein-Papier in seiner 2005er Variante
;;; Author: Sascha Biermanns (saschakb), <skkd dot h4k1n9 at yahoo dot de>

;;; Lizenz: ISC

;;; Copyright (C) 2012, Sascha Biermanns

;;; Permission to use, copy, modify, and/or distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; -----------------------------------------------------------------
;;; Hinweis:
;;; Das Laden dieser Datei in Common Lisp erfolgt per:
;;; (load "ssp")
;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;; (compile-file "ssp")
;;; -----------------------------------------------------------------



(defun aussage (objekt1 verb objekt2)
  "(aussage objekt1 verb objekt2)
AUSSAGE dient dazu einen kurzen Satz, bestehend aus 2 Objekten und einer Handlung zurückzuliefern.
Beispiel: (aussage 'schere 'schneidet 'papier)
 SCHERE SCHNEIDET PAPIER"
  (format t "~& ~A ~A ~A.~%" objekt1 verb objekt2))



(defun prompt-read (eingabeaufforderung)
  "(promt-read eingabeaufforderung)
PROMPT-READ stellt eine gewünschte Eingabeaufforderung dar, um im Anschluß daran eine Eingabe einzulesen und zurückzugeben. Beendet wird die Eingabe durch drücken der Eingabetaste.
Beispiel: (prompt-read \"Wie lautet dein Name\")
Wie lautet dein Name? Sascha
\"Sascha\""
  (format *query-io* "~&~a? " eingabeaufforderung)
  (force-output *query-io*)
  (read-line *query-io*))



(defun schere-stein-papier-echse-spock ()
  "(schere-stein-papier-echse-spock)
SCHERE-STEIN-PAPIER-ECHSE-SPOCK dient dazu, eine gepflegte Runde Schere-Stein-Papier-Echse-Spock zu spielen, wie es von Sam Kass und Karen Bryla erfunden und 2005 in der Times veröffentlicht wurde. Es bildet eine Erweiterung gegenüber dem ursprünglichen \"Schere-Stein-Papier\" und ist gleichwertig zum alten deutschen \"Schere-Stein-Papier-Brunnen-Kurbel\", aber dank der TV-Sendung \"The Big Bang Theory\" wurde es schlagartig im Jahre 2008 weltweit bekannt - und erfreut sich unter Computerenthusiasten, Fernsehzuschauern und Star-Trek-Fans einer gewissen Beliebtheit. Da die Spielregeln umfangreicher als die Ursprungsregeln sind, hier einfach einmal die vollständigen Spielregeln:
* Schere schneidet Papier
* Papier bedeckt Stein
* Stein zerschmettert Echse
* Echse vergiftet Spock
* Spock zertrümmert Schere
* Schere köpft Echse
* Echse frisst Papier
* Papier widerlegt Spock
* Spock verdampft Stein
* Stein zerschmettert Schere
Beispiel: (schere-stein-papier-echse-spock)
Bitte triff eine Auswahl, was du ausspielst:
1. Schere
2. Stein
3. Papier
4. Echse
5. Spock
Wie lautet deine Wahl? 2
 STEIN ZERSCHMETTERT ECHSE."
  (format t "~&Bitte triff eine Auswahl, was du ausspielst:~%1. Schere~%2. Stein~%3. Papier~%4. Echse~%5. Spock~%")
  (let* ((auswahl (prompt-read "Wie lautet deine Wahl"))
	(zahl (parse-integer auswahl :junk-allowed t)))
    (cond ((not (null zahl))
	   (spiele-sspes (wähle-zahl zahl)))
	  ((equal auswahl "")
	   (format t "~&Du hast keine Auswahl getroffen. Du mußt entweder den Begriff, oder die davor stehende Nummer eingeben, um eine Auswahl zu treffen."))
	  ((and (setf auswahl (wähle-begriff auswahl)))
		(spiele-sspes auswahl))
	  (t
	   (format t "~&Du hast keine gültige Auswahl getroffen. So können wir leider nicht miteinander spielen.~%")))))
	 
    
	  


(defun spiele-sspes (&optional objekt1 objekt2)
  "(spiele-sspes &optional objekt1 objekt2)
SPIELE-SSPES dient dazu, eine gepflegte Runde Schere-Stein-Papier-Echse-Spock zu spielen, wie es von Sam Kass und Karen Bryla erfunden und 2005 in der Times veröffentlicht wurde. Es bildet eine Erweiterung gegenüber dem ursprünglichen \"Schere-Stein-Papier\" und ist gleichwertig zum alten deutschen \"Schere-Stein-Papier-Brunnen-Kurbel\", aber dank der TV-Sendung \"The Big Bang Theory\" wurde es schlagartig im Jahre 2008 weltweit bekannt - und erfreut sich unter Computerenthusiasten, Fernsehzuschauern und Star-Trek-Fans einer gewissen Beliebtheit. Da die Spielregeln umfangreicher als die Ursprungsregeln sind, hier einfach einmal die vollständigen Spielregeln:
* Schere schneidet Papier
* Papier bedeckt Stein
* Stein zerschmettert Echse
* Echse vergiftet Spock
* Spock zertrümmert Schere
* Schere köpft Echse
* Echse frisst Papier
* Papier widerlegt Spock
* Spock verdampft Stein
* Stein zerschmettert Schere
Beispiel: (spiele-sspes 'schere)
 SCHERE KÖPFT ECHSE."
  (let ((gültige-objekte '(schere stein papier echse spock))) ; eine Liste aller gültigen Objekte
    (when (null objekt1) ; hat objekt1 noch keinen Inhalt?
      (setf objekt1 (zufallsauswahl))) ; dann befüllen wir es zufällig
    (when (null objekt2) ; hat objekt2 noch keinen Inhalt?
      (setf objekt2 (zufallsauswahl))) ; dann befüllen wir es zufällig
    (cond ((or (not (member objekt1 gültige-objekte)) ; ist objekt1 ein gültiges Objekt? 
	       (not (member objekt2 gültige-objekte))) ; ist objekt2 ein gültiges Objekt?
	   (format t "~&Das Spiel heisst Stein Papier Schere Echse Spock, hier noch einmal alles über das Spiel:~%")
	   (princ (documentation 'spiele-sspes 'function)))
	  ((and (equal objekt1 'schere) (equal objekt2 'papier))
	   (aussage objekt1 'schneidet objekt2))
	  ((and (equal objekt1 'papier) (equal objekt2 'stein))
	   (aussage objekt1 'bedeckt objekt2))
	  ((and (equal objekt1 'stein) (or (equal objekt2 'echse) (equal objekt2 'schere)))
	   (aussage objekt1 'zerschmettert objekt2))
	  ((and (equal objekt1 'echse) (equal objekt2 'spock))
	   (aussage objekt1 'vergiftet objekt2))
	  ((and (equal objekt1 'spock) (equal objekt2 'schere))
	   (aussage objekt1 'zertrümmert objekt2))
	  ((and (equal objekt1 'schere) (equal objekt2 'echse))
	   (aussage objekt1 'köpft objekt2))
	  ((and (equal objekt1 'echse) (equal objekt2 'papier))
	   (aussage objekt1 'frisst objekt2))
	  ((and (equal objekt1 'papier) (equal objekt2 'spock))
	   (aussage objekt1 'widerlegt objekt2))
	  ((and (equal objekt1 'spock) (equal objekt2 'stein))
	   (aussage objekt1 'verdampft objekt2))
	  ((equal objekt1 objekt2)
	   (format t "~&~A trifft auf ~A ist immer ein Unentschieden.~%" objekt1 objekt2))
	  (t (spiele-sspes objekt2 objekt1))))) ; Hm, probieren wir es doch mit der umgekehrt Objektfolge



(defun wähle-begriff (begriff)
  "(wähle-begriff begriff)
WÄHLE-BEGRIFF dient dazu, aus einem Begriff zu ermitteln, ob er ein Ausdruck der vorgegebenen Liste '(schere stein papier echse spock) ist.
Beispiel: (wähle-begriff \"Schere\")
Schere"
  (let ((symbol (read-from-string begriff)))
    (first (member symbol '(schere stein papier echse spock)))))



(defun wähle-zahl (zahl)
  "(wähle-zahl zahl)
WÄHLE-ZAHL dient dazu, zu ermitteln ob eine der 5 gesuchten Zahlen vorhanden ist und aufgrund dieser den ihr zugeordneten Begriff zurückzuliefern. Ist dies nicht der Fall, wird NIL zurückgegeben.
Beispiel: (wähle-zahl 4)
ECHSE"
  (cond ((= zahl 1) 'schere)
	((= zahl 2) 'stein)
	((= zahl 3) 'papier)
	((= zahl 4) 'echse)
	((= zahl 5) 'spock)
	(t nil)))



(defun zufallsauswahl ()
  "(zufallsauswahl)
ZUFALLSAUSWAHL liefert bei seinem Aufruf eine von 5 Möglichkeiten des Papiers Schere-Stein-Papier-Echse-Spock zurück.
Beispiel: (zufallsauswahl)
 SCHERE"
  (let ((auswahl (1+ (random 5))))
    (wähle-zahl auswahl)))



;;; --------------------------------------------------------------------------------
;;; Um das Spiel automatisch beim Laden zu spielen, müssen einfach nur das Semikolon
;;; in der übernächsten Zeile entfernt werden.
;;; --------------------------------------------------------------------------------
;(schere-stein-papier-echse-spock)
