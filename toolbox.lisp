;-*-lisp-*-
;-*- coding: utf-8 -*-

;;;; Dateiname: toolbox.lisp
;;;; Beschreibung: Ein kleiner Werkzeugkasten für den Umgang mit Lisp - und um Programme "einzudeutschen"
;;;; Author: Sascha Biermanns, <skkd.h4k1n9@yahoo.de>

;;;; Lizenz: ISC

;;;; Copyright (C) 2011,2012 Sascha Biermanns

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

;;;; Hinweis:
;;;; Das Laden dieser Datei in Common Lisp erfolgt per:
;;;; (load "toolbox")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "toolbox")


;;; ===============================
;;; Abschnitt 1 - Macros in Deutsch
;;; ===============================

(defmacro assoziiere (objekt a-liste &rest schlüssel)
  "(assoziiere objekt a-liste &key :test :test-not :key)
ASSOZIIERE ist die deutsche Fassung der Funktion ASSOC.
ASSOZIIERE durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert dem von Objekt entspricht, oder aber NIL. 
ASSOZIIERE verfügt über zwei Kind-Funktionen: ASSOZIIERE-WENN und ASSOZIIERE-WENN-NICHT.
Beispiel: (assoziiere 'b '((a hubbel) (b dubbel) (c gubbel))) => (B DUBBEL)"
  `(assoc ,objekt ,a-liste ,@schlüssel))

(defmacro assoziiere-wenn (prädikat a-liste &rest schlüssel)
  "(assoziiere-wenn prädikat a-liste &key :key)
ASSOZIIERE-WENN ist die deutsche Fassung der Funktion ASSOC-IF.
ASSOZIIERE-WENN durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert der PRÄDIKATPRÜFUNG entspricht, oder aber NIL. 
ASSOZIIERE-WENN verfügt über eine Eltern-Funktion, ASSOZIIERE, und eine Bruderfunktion, ASSOZIIERE-WENN-NICHT.
Beispiel: (assoziiere-wenn #'oddp '((2 hubbel) (3 dubbel) (5 gubbel))) => (3 DUBBEL)"
  `(assoc-if ,prädikat ,a-liste ,@schlüssel))

(defmacro assoziiere-wenn-nicht (prädikat a-liste &rest schlüssel)
  "(assoziiere-wenn-nicht prädikat a-liste &key :key)
ASSOZIIERE-WENN-NICHT ist die deutsche Fassung der Funktion ASSOC-IF-NOT.
ASSOZIIERE-WENN-NICHT durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert nicht der PRÄDIKATPRÜFUNG entspricht, oder aber NIL. 
ASSOZIIERE-WENN-NICHT verfügt über eine Eltern-Funktion, ASSOZIIERE, und eine Bruderfunktion, ASSOZIIERE-WENN.
Beispiel: (assoziiere-wenn-nicht #'oddp '((2 hubbel) (3 dubbel) (5 gubbel))) => (2 HUBBEL)"
  `(assoc-if-not ,prädikat ,a-liste ,@schlüssel))

(defmacro austauschen (neu alt baum &rest schlüssel)
  "(austauschen neu alt baum &key :test :test-not :key)
AUSTAUSCHEN ist die deutsche Fassung von SUBST.
AUSTAUSCHEN erstellt eine Kopie des Baumes, wobei ein Austausch von Alt gegen Neu erfolgt. Der Austausch kann zusätzlich durch die Schlüsselworte beeinflusst werden. Der Orginalbaum bleibt unverändert, der neue Baum kann jedoch Teile des Originalbaums erhalten, er muss also keine unabhängige Kopie sein.
AUSTAUSCHEN verfügt über eine Kind-Funktionen, AUSTAUSCHEN-WENN.
Von beiden Funktionen gibt es zusätzlich eine zerstörerische Version, die den Ursprungsbaum zerstören kann, NAUSTAUSCHEN und NAUSTAUSCHEN-WENN.
Beispiel: (austauschen 'tempest 'hurricane '(shakespeare wrote (the hurricane))) =>  (SHAKESPEARE WROTE (THE TEMPEST))"
  `(subst ,neu ,alt ,baum ,@schlüssel))

(defmacro austauschen-wenn (neu prädikat baum &rest schlüssel)
  "(austauschen-wenn neu prädikat baum &key :key)
AUSTAUSCHEN-WENN ist die deutsche Fassung von SUBST-IF.
AUSTAUSCHEN-WENN erstellt eine Kopie des Baumes, wobei ein Austausch erfolgt, wenn ein Blatt oder Ast das Prädikat erfüllt. Der Austausch erfolgt gegen Neu, wobei dieser zusätzlich durch das Schlüsselwort beeinflusst werden kann. Der Orginalbaum bleibt unverändert, der neue Baum kann jedoch Teile des Originalbaums erhalten, er muss also keine unabhängige Kopie sein.
AUSTAUSCHEN-WENN verfügt über eine Elternfunktion, AUSTAUSCHEN.
Von beiden Funktionen gibt es zusätzlich eine zerstörerische Version, die den Ursprungsbaum zerstören kann, NAUSTAUSCHEN und NAUSTAUSCHEN-WENN.
Beispiel: (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
 (austauschen-wenn 5 #'listp tree1) =>  5"
  `(subst-if ,neu ,prädikat ,baum ,@schlüssel))

(defmacro austauschliste (a-liste baum &rest schlüssel)
  "(austauschliste a-liste baum &key :test :test-not :key)
AUSTAUSCHLISTE ist die deutsche Fassung von SUBLIS.
AUSTAUSCHLISTE macht Ersetzungen an einem Baum, so wie es AUSTAUSCHEN auch vornimmt, jedoch werden viele Ersetzungen auf einmal anhand einer Assoziations-Liste erstellt. AUSTAUSCHLISTE berücksichtig bei der Suche alle Blätter und Zweige des Baumes. Die Suche kann zusätzlich durch Schlüsselbegriffe und Testmöglichkeiten beeinflusst werden.
Beispiel: (austauschliste '((rosen . veilchen) (rot . blau)) '(rosen sind rot)) => VEILCHEN SIND BLAU."
  `(sublis ,a-liste ,baum ,@schlüssel))

(defmacro baum-gleich (x y &rest schlüssel)
  "(baum-gleich x y &schlüssel :test :test-not)
BAUM-GLEICH ist die deutsche Fassung von TREE-EQUAL.
BAUM-GLEICH ist ein Prädikat, das wahr zurück liefert, wenn x und y isomorphe Bäume sind, mit identischen Zweigen und Blättern, wenn also die Überprüfung jeden einzelnen Atoms die Überprüfung (vorausgewählt ist eql) besteht. Durch Verwendung von :test #'equal können auch anders strukturierte Objekte, wie beispielsweise Zeichenketten überprüft werden.
Beispiel: (baum-gleich '((a) (b c)) '((a) (b c))) =>T"
  `(tree-equal ,x ,y ,@schlüssel))

(defmacro differenzmenge (liste1 liste2 &rest schlüssel)
  "(differenzmenge liste1 liste2 &key :test :test-not :key)
DIFFERENZMENGE ist die deutsche Fassung der Funktion SET-DIFFERENCE.
DIFFERENZMENGE ermittelt die Differenzmenge von Liste1 zu Liste2. Die zurückgegebene Liste enthält also alle Elemente aus Liste1, die nicht in Liste2 vorhanden sind. Der Ausgang kann mit Hilfe der verschiedenen Schlüssel beeinflusst werden.
DIFFERENZMENGE ist eine nicht-destruktive Funktion. Sie verfügt über eine destruktive Bruderfunktion, NSET-DIFFERENCE, die bei ihrer Anwendung vermutlich Liste1 zerstören wird.
Beispiel: (differenzmenge '(a b c d) '(c d e f)) => (B A)"
  `(set-difference ,liste1 ,liste2 ,@schlüssel))

(defmacro dokumentation (symbol dokumentations-art)
  "DOKUMENTATION ist die deutsche Fassung der Funktion DOCUMENTATION.
DOKUMENTATION dient dazu, die Documentation zu einem Symbol der Art Dokumentations-Art anzuzeigen.
Mit Hilfe von SETF kann allerdings auch die bereits vorhandene Dokumentation neu gesetzt werden.
Beispiel: (dokumentation 'documentation 'function).
 (defparameter *test* 4) => *TEST*
 (setf (dokumentation '*test* 'variable) \"Nur eine Testvariable.\") => \"Nur eine Testvariable.\"
 (dokumentation '*test* 'variable) => \"Nur eine Testvariable.\""
  `(documentation ,symbol ,dokumentations-art))

(defmacro dokumentation-austauschen (objekt nutzung zeichenkette)
  "(dokumentation-austauschen objekt nutzung zeichenkette)
DOKUMENTATION-AUSTAUSCHEN dient dazu, die Dokumentationszeichenkette auszutauschen."
  `(setf (documentation ,objekt ,nutzung) ,zeichenkette))

(defmacro entferne (item &rest liste)
  "(entferne item liste)
ENTFERNE entspricht der deutschen Fassung für remove.
Es wird jegliches Item aus einer bestehenden Liste entfernt.
Beispiel: (entferne 1000 '(1 2 50 1000 70000 1000)) => (1 2 50 70000)"
  `(remove ,item ,@liste))

(defmacro entferne-wenn (objekt liste &rest schlüssel)
  "(entferne-wenn test reihenfolge &key :from-end :start :end :count :key)
ENTFERNE-WENN ist die deutsche Fassung von REMOVE-IF.
ENTFERNE-WENN liefert eine Reihenfolge zurück, die weitestgehend der übergebenen Reihenfolge entspricht, jedoch wurden jene zwischen :start und :end herausgenommen, die dem test entsprachen. Das :count Argument, wenn angegeben, sorgt dafür das nicht mehr Argumente als gewünscht entfernt werden. Ausschließlich wenn das :count Argument angegeben wurde. In diesem Fall arbeitet die Funktion vom Ende aus rückwerts die Liste ab.
ENTFERNE-WENN ist nicht destruktiv, die Ursprungsliste wird nicht verändert.
ENTFERNE-WENN ist ein Abkömmling von ENTFERNE und hat ENTFERNE-WENN-NICHT als Bruderfunktion an seiner Seite.
Beispiele: (entferne-wenn #'oddp '(1 2 4 1 3 4 5)) => (2 4 4)
 (entferne-wenn #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t) => (1 2 4 1 3 5)"
  `(remove-if ,objekt ,liste ,@schlüssel))

(defmacro entferne-wenn-nicht (objekt liste &rest schlüssel)
  "(entferne-wenn-nicht test reihenfolge &key :from-end :start :end :count :key)
ENTFERNE-WENN-NICHT ist die deutsche Fassung von REMOVE-IF-NOT.
ENTFERNE-WENN-NICHT liefert eine Reihenfolge zurück, die weitestgehend der übergebenen Reihenfolge entspricht, jedoch wurden jene zwischen :start und :end herausgenommen, die dem test nicht entsprachen. Das :count Argument, wenn angegeben, sorgt dafür das nicht mehr Argumente als gewünscht entfernt werden. Ausschließlich wenn das :count Argument angegeben wurde. In diesem Fall arbeitet die Funktion vom Ende aus rückwerts die Liste ab.
ENTFERNE-WENN-NICHT ist nicht destruktiv, die Ursprungsliste wird nicht verändert.
ENTFERNE-WENN-NICHT ist ein Abkömmling von ENTFERNE und hat ENTFERNE-WENN als Bruderfunktion an seiner Seite.
Beispiel: (entferne-wenn-nicht #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
=>  (1 2 3 4 5 6 8)"
  `(remove-if-not ,objekt ,liste ,@schlüssel))

(defmacro entferne-duplikate (sequenz &rest schlüssel)
  "(entferne-duplikate reihenfolge &key :from-end :test :start :end :key)
ENTFERNE-DUPLIKATE ist die deutsche Fassung von REMOVE-DUPLICATES.
ENTFERNE-DUPLIKATE überprüft die Reihenfolge paarweise, und wenn zwei daraus übereinstimmend sind, wird das erste entfernt, es sei denn FROM-END ist wahr, dann wird das letztere entfernt. Andere Schlüsselworte können ebenfalls einen Einfluss auf die Überprüfung auswirken. Das Ergebnis wird zurückgeliefert.
Die Funktion ist destruktiv, die Original-Reihenfolge wird vermutlich zerstört werden.
Das Schlüsselwort :test-not gilt als veraltet.
Beispiel: (entferne-duplikate '(a b c b d d e)) => (a d b c e)"
  `(remove-duplicates ,sequenz ,@schlüssel))

(defmacro ggt (&rest liste)
  "(ggt zahlen)
GGT ist die deutsche Fassung von gcd.
GGT steht für GRÖßTER GEMEINSAMER TEILER und berechnet aus einer Liste von Zahlen die größte Zahl, durch die alle Zahlen der Liste sind.
Beispiel: (ggt 24 48 72 120) => 24"
  `(gcd ,@liste))

(defmacro kgv (&rest liste)
  "(kgv zahlen)
KGV ist die deutsche Fassung von lcm.
KGV stet für das KLEINSTE GEMEINSAME VIELFACHE und berechnet aus einer Liste von Zahlen die kleinste Zahl, die ein gemeinsames Vielfaches aller Zahlen darstellt.
Beispiel: (kgv 24 48 72 120) => 720"
  `(lcm ,@liste))

(defmacro länge (&rest reihenfolge)
  "(länge reihenfolge)
LÄNGE ist die deutsche Fassung von length.
Es wird die Länge einer Reihenfolge zurückgegeben.
Beispiel: (länge '(a b c)) => 3"
  `(length ,@reihenfolge))

(defmacro letztes (liste &optional (n 1))
  "(letztes liste &optional (n 1))
LETZTES ist die deutsche Fassung von last. 
LETZTES gibt die Verkettung der letzten N CONS-Werte zurück, nicht die letzen N Elemente.
Ist die Liste eine leere Liste wird NIL zurückgegeben.
Ist N Null, so wird das Atom zurückgeliefert, das die gesamte Liste beendet.
Beispiel:  (letztes '(a b c d Gummibaum)) => (GUMMIBAUM)"
       `(last ,liste ,n))

(defmacro listen-länge (&rest liste)
  "(listen-länge liste)
LISTEN-LÄNGE ist die deutsche Fassung von list-length.
LISTEN-LÄNGE gibt die Länge einer Liste als Ganzzahl zurück. Hierbei verhält sie sich anders als LÄNGE, wenn die übergebene Liste zirkular ist. LÄNGE würde hierbei versagen, während LISTEN-LÄNGE ein NIL zurückgibt.
Beispiel: (listen-länge '(a b c)) => 3"
  `(list-length ,@liste))

(defmacro mitglied (objekt &rest liste-und-schlüssel)
  "(mitglied objekt liste schlüssel)
MITGLIED ist die deutsche Fassung von member.
Es wird geprüft ob ein Objekt ein Mitglied in einer Liste ist. Wenn ja, wir die Liste ab dem gefunden bjekt zurückgegeben, ansonsten NIL. Hierbei werden die verwendeten Schlüssel :test :test-not oder :key berücksichtigt.
Beispiel: (mitlied 'boston '(la new york boston hamburg)) => (BOSTON HAMBURG)"
  `(member ,objekt ,@liste-und-schlüssel))

(defmacro naustauschen (neu alt baum &rest schlüssel)
  "(naustauschen neu alt baum &key :test :test-not :key)
NAUSTAUSCHEN ist die deutsche Fassung von NSUBST.
NAUSTAUSCHEN erstellt eine Kopie des Baumes, wobei ein Austausch von Alt gegen Neu erfolgt. Der Austausch kann zusätzlich durch die Schlüsselworte beeinflusst werden. Der Orginalbaum wird vermutlich zerstört, der neue Baum kann jedoch Teile des Originalbaums erhalten.
NAUSTAUSCHEN verfügt über eine Kind-Funktionen, NAUSTAUSCHEN-WENN.
Von beiden Funktionen gibt es zusätzlich eine nicht zerstörerische Version, die den Ursprungsbaum nicht zerstört, AUSTAUSCHEN und AUSTAUSCHEN-WENN.
Beispiel: (naustauschen 'tempest 'hurricane '(shakespeare wrote (the hurricane))) =>  (SHAKESPEARE WROTE (THE TEMPEST))"
  `(nsubst ,neu ,alt ,baum ,@schlüssel))

(defmacro naustauschen-wenn (neu prädikat baum &rest schlüssel)
  "(austauschen-wenn neu prädikat baum &key :key)
NAUSTAUSCHEN-WENN ist die deutsche Fassung von NSUBST-IF.
NAUSTAUSCHEN-WENN erstellt eine Kopie des Baumes, wobei ein Austausch erfolgt, wenn ein Blatt oder Ast das Prädikat erfüllt. Der Austausch erfolgt gegen Neu, wobei dieser zusätzlich durch das Schlüsselwort beeinflusst werden kann. Der Orginalbaum wird vermutlich zerstört, der neue Baum kann jedoch Teile des Originalbaums erhalten.
NAUSTAUSCHEN-WENN verfügt über eine Elternfunktion, NAUSTAUSCHEN.
Von beiden Funktionen gibt es zusätzlich eine nicht zerstörerische Version, die den Ursprungsbaum nicht zerstört, AUSTAUSCHEN und AUSTAUSCHEN-WENN.
Beispiel: (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
 (naustauschen-wenn 5 #'listp tree1) =>  5"
  `(nsubst-if ,neu ,prädikat ,baum ,@schlüssel))

(defmacro rassoziiere (objekt a-liste &rest schlüssel)
  "(rassoziiere objekt a-liste &key :test :test-not :key)
RASSOZIIERE durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CDR-Wert dem von Objekt entspricht, oder aber NIL. 
RASSOZIIERE verfügt über zwei Kind-Funktionen: RASSOC-IF und RASSOC-IF-NOT.
RASSOZIIERE benötigt A-Listen, deren Objekte Paare aus gepunkteten Listen sind.
Beispiel: (rassoziiere 'b '((a hubbel) (b dubbel) (c gubbel))) => NIL
 (rassoziiere 'dubbel '((a . hubbel) (b . dubbel) (c . gubbel))) => (B . DUBBEL)"
  `(rassoc ,objekt ,a-liste ,@schlüssel))

(defmacro rassoziiere-wenn (prädikat a-liste &rest schlüssel)
  "(rassoc-if prädikat a-liste &key :key)
RASSOZIIERE-WENN durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CDR-Wert dem Prädikat entspricht, oder aber NIL. 
RASSOZIIERE-WENN verfügt über eine Eltern-Funktion, RASSOC, und eine Bruderfunktion, RASSOC-IF-NOT.
RASSOZIIERE-WENN benötigt A-Listen, deren Objekte Paare aus gepunkteten Listen sind.
Beispiel: (rassoziiere-wenn 'b '((a hubbel) (b dubbel) (c gubbel))) => NIL
 (rassoziiere-wenn #'oddp '((hubbel . 2) (dubbel . 3) (gubbel . 4))) => (DUBBEL . 3)"
  `(rassoc-if ,prädikat ,a-liste ,@schlüssel))

(defmacro rassoziiere-wenn-nicht (prädikat a-liste &rest schlüssel)
  "(rassoziiere-wenn-nicht prädikat a-liste &key :key)
RASSOZIIERE-WENN-NICHT durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CDR-Wert nicht dem Prädikat entspricht, oder aber NIL.
RASSOZIIERE-WENN-NICHT verfügt über eine Eltern-Funktion, RASSOC, und eine Bruderfunktion, RASSOC-IF.
RASSOZIIERE-WENN-NICHT benötigt A-Listen, deren Objekte Paare aus gepunkteten Listen sind.
Beispiel: (rassoziiere-wenn-nicht 'b '((a hubbel) (b dubbel) (c gubbel))) => NIL
 (rassoziiere-wenn-nicht #'oddp '((hubbel . 2) (dubbel . 3) (gubbel . 4))) => (HUBBEL . 2)"
  `(rassoc-if-not ,prädikat ,a-liste ,@schlüssel))

(defmacro rückwärts (&rest reihenfolge)
  "(rückwärts reihenfolge)
RÜCKWÄRTS ist die deutsche Fassung von reverse.
Sie nimmt eine Reihenfolge und gibt diese in umgekehrter Reihenfolge zurück. Hierbei wird ausschließlich die oberste Elementeben umgekehrt.
Beispiel: (rückwärts '(a b c d)) => (D C B A)"
  `(reverse ,@reihenfolge))

(defmacro schnittmenge (liste1 liste2 &rest schlüssel)
  "(schnittmenge liste1 liste2 &key :test :test-not :key)
SCHNITTMENGE ist die deutsche Fassung von INTERSECTION.
SCHNITTMENGE fasst alle Elemente, die in beiden Listen, liste1 und liste2 vorhanden sind und, wenn angegeben :test oder :test-not, das Testkriterium bestehen, zu einer neuen Liste, also einer Schnittmenge (engl. intersection) zusammen. Die Reihenfolge der Elemente in der neuen Liste ist implementationsabhängig und gilt als nicht vorhersehbar. Wird eine bestimmte Reihenfolge benötigt, so muß das Ergebnis entsprechend noch mit Hilfer der SORT-Funktion sortiert werden.
Beispiel: (schnittmenge '(a b c d) '(f a d s l)) => (A D)
 (schnittmenge '((a) b c d) '(f (a) d s l) :test #'equal) => (D (A))"
  `(intersection ,liste1 ,liste2 ,@schlüssel))

(defmacro untermenge-p (liste1 liste2 &rest schlüssel)
  "(untermenge-p liste1 liste2 &key :test :test-not :key)
UNTERMENGE-P ist die deutsche Fassung des Prädikats SUBSETP.
UNTERMENGE-P ist ein Prädikat, daß das Ergebnis zurückliefert, ob Liste1 eine Untermenge von Liste2 ist. Gegebenenfalls kann das Ergebnis mit Hilfe der Schlüsselwörter beeinflusst werden.
Beispiel: (untermenge-p '(rot grün) '(rot grün blau gelb)) => T"
  `(subsetp ,liste1 ,liste2 ,@schlüssel))

(defmacro vereinigungsmenge (liste1 liste2 &rest schlüssel)
  "(VEREINIGUNGSMENGE liste1 liste2 &key :test :test-not :key)
VEREINIGUNGSMENGE ist die deutsche Fassung der Funktion UNION.
VEREINIGUNGSMENGE liefert die Vereinigungsmenge aus Liste1 und Liste2 zurück. Sind Schlüssel wie :test oder :test-not angegeben, werden die entsprechenden Prüfverfahren im Vorfeld herangezogen.
Die Reihenfolge der Elemente in der Ergebnisliste sind Implementationsabhängig und gelten als nicht vorhersagbar. Wird eine bestimmte Reihenfolge im Ergebnis erwartet, so muß dieses per SORT-Funktion erzeugt werden.
Beispiel: (vereinigungsmenge '(rot grün blau) '(gelb magenta blau)) => (ROT GRÜN GELB MAGENTA BLAU)
 (vereinigungsmenge '((x 5) (y 6)) '((z 2) (x 4)) :key #'first) => ((x 4) (z 2) (y 6))"  
  `(union ,liste1 ,liste2 ,@schlüssel))

(defmacro zufallszahl (zahl &optional (status nil status-supplied-p))
  "(zufallszahl zahl &optional status)
ZUFALLSZAHL ist die deutsche Fassung von RANDOM. Sie nimmt eine Zahl entgegen und liefert eine Zahl zurück, die kleiner ist als zahl, jedoch größer oder gleich 0.
Beispiel: (zufallszahl 6) => 2"
  (if (eql status-supplied-p t)
      `(random ,zahl ,status)
      `(random ,zahl)))

;;; ---------------------------------------------------------------------
;;; Abschnitt 2 - Listenmacros für car/cdr/nth und seine Erben in Deutsch
;;; ---------------------------------------------------------------------

(defmacro nter (n liste)
  "(nter liste)
NTER ist die deutsche Fassung von NTH.
NTER liefert den nten Wert einer Liste zurück. Zu beachten ist, das die Zählung von n bei 0 beginnt, nicht bei 1.
Beispiel: (nter 5 '(a b c d e f g)) ==> F"
  `(nth ,n ,liste))

; Deutsche Fassungen für die car/cdr Erben

(defmacro erster (liste)
  "(erster liste)
ERSTER ist die deutsche Fassung für FIRST, respektive CAR.
Es wird der erste Wert einer übergebenen Liste zurückgegeben.
Beispiel: (erster '(a b c d e f g h i j k l m n)) => A"
  `(first ,liste))

(defmacro zweiter (liste)
  "(zweiter liste)
ZWEITER ist die deutsche Fassung für SECOND, respektive CADR.
Es wird der zweite Wert einer übergebenen Liste zurückgegeben.
Beispiel: (zweiter '(a b c d e f g h i j k l m n)) => B"
  `(second ,liste))

(defmacro dritter (liste)
  "(dritter liste)
DRITTER ist die deutsche Fassung für THIRD, respektive CADDR.
Es wird der dritte Wert einer übergebenen Liste zurückgegeben.
Beispiel: (dritter '(a b c d e f g h i j k l m n)) => C"
  `(third ,liste))

(defmacro vierter (liste)
  "(vierter liste)
VIERTER ist die deutsche Fassung für FOURTH, respektive CADDDR.
Es wird der vierte Wert einer übergebenen Liste zurückgegeben.
Beispiel: (vierter 'a b c d e f g h i j k l m n)) => D"
 `(fourth ,liste))

(defmacro fünfter (liste)
  "(fünfter liste)
FÜNFTER ist die deutsche Fassung für FIFTH.
Es wird der fünfte Wert einer übergebenen Liste zurückgegeben.
Beispiel: (fünfter 'a b c d e f g h i j k l m n)) => E"
  `(fifth ,liste))

(defmacro sechster (liste)
  "(sechster liste)
SECHSTER ist die deutsche Fassung für SIXTH.
Es wird der sechste Wert einer übergebenen Liste zurückgegeben.
Beispiel: (sechster 'a b c d e f g h i j k l m n)) => F"
  `(nth 5 ,liste))

(defmacro siebter (liste)
  "(siebter liste)
SIEBTER ist die deutsche Fassung für SEVENTH.
Es wird der siebte Wert einer übergebenen Liste zurückgegeben.
Beispiel: (siebter 'a b c d e f g h i j k l m n)) => G"
  `(nth 6 ,liste))

(defmacro achter (liste)
  "(achter liste)
ACHTER ist die deutsche Fassung für EIGTH.
Es wird der achte Wert einer übergebenen Liste zurückgegeben.
Beispiel: (achter 'a b c d e f g h i j k l m n)) => H"
  `(nth 7 ,liste))

(defmacro neunter (liste)
  "(neunter liste)
NEUNTER ist die deutsche Fassung für NINETH.
Es wird der neunte Wert einer übergebenen Liste zurückgegeben.
Beispiel: (neunter 'a b c d e f g h i j k l m n)) => I"
  `(nth 8 ,liste))

(defmacro zehnter (liste)
  "(zehnter liste)
ZEHNTER ist die deutsche Fassung für TENTH.
Es wird der zehnte Wert einer übergebenen Liste zurückgegeben.
Beispiel: (zehnter 'a b c d e f g h i j k l m n)) => J"
  `(nth 9 ,liste))

(defmacro elfter (liste)
  "(elfter liste)
ELFTER liefert den elften Wert einer übergebenen Liste zurückgegeben.
Beispiel: (elfter 'a b c d e f g h i j k l m n)) => K"
  `(nth 10 ,liste))

(defmacro zwölfter (liste)
  "(zwölfter liste)
ZWÖLFTER liefert den zwölften Wert einer übergebenen Liste zurückgegeben.
Beispiel: (zwölfter 'a b c d e f g h i j k l m n)) => L"
 `(nth 11 ,liste))

; ---------------------

;;; =====================================================================

;;; -----------------------
;;; Abschnitt 3 - Prädikate
;;; -----------------------

(defun echte-teilmenge-p (a b)
  "(echte-teilmenge-p liste1 liste2)
ECHTE-TEILMENGE-P überprüft, ob Liste1 ein wirklicher Subset von Liste2 ist. Das bedeutet, das Liste1 ausschließlich Elemente aus Liste 2 enthält, nicht aber alle Elemente der Liste 2. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiele: (echte-teilmenge-p '(rot grün) '(grün blau rot gelb)) => T
 (echte-teilmenge-p '(rot schwarz) '(grün blau gelb)) => NIL"
	   (when (and (subsetp a b) (not (subsetp b a)))
	     t))

(defun gleichwertige-elemente (a b)
  "(gleichwertige-elemente liste1 liste2)
GLEICHWERTIGE-ELEMENTE überprüft, ob Liste1 und Liste2 über dieselben Elemente verfügen. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiel: (gleichwertige-elemente '(rot blau grün) '(grün rot blau)) => "T
	   (when (and (subsetp a b) (subsetp b a))
	     t))

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

(defun palindrom-p (sequenz) "(palindrom-p sequenz)
Palindromp testet, ob eine übergebene Sequenz, eine übergebene Zeichenkette oder ein übergebenes Symbol ein Palindrom darstellt.
Beispiele: (palindrom-p '(1 2 3 4 3 2 1)) => T
 (palindrom-p 'otto) => T
 (palindrom-p 'otta) => NIL
 (palindrom-p \"Otto\") => T"
       (cond ((null sequenz) nil)
	     ((stringp sequenz) (string= sequenz (reverse sequenz)))
	     ((symbolp sequenz) (string= (symbol-name sequenz) (reverse (symbol-name sequenz))))
	     ((listp sequenz) (equal sequenz (reverse sequenz)))
	     (t nil)))

(defun set-equal-p (a b)
  "(set-equal-p liste1 liste2)
SET-EQUAL-P überprüft, ob Liste1 und Liste2 über dieselben Elemente verfügen. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiel: (set-equal-p '(red blue green) '(green red blue)) => "T
	   (cond ((and (not (set-difference a b)) (not (set-difference b a))) t)
		 (t nil)))

(defun vorher-p (a b liste)
  "(vorher-p a b liste)
VORHER-P ist ein Prädikat und prüft, ob sich a vor b in der Liste befindet. Als Ergebnis wird die Liste ab b oder nil zurückgeben.
Beispiel: (vorher-p 'alpha 'beta '(alpha beta gamma delta)) => (BETA GAMMA DELTA)"
  (member b (member a liste)))


;;; ===========================================
;;; Abschnitt 4 - Es ist einfach nur Mathematik
;;; ===========================================

(defun durchschnitt (&rest liste)
  "(durchschnitt liste)
DURCHSCHNITT ermöglicht es, den Durchschnitt einer Reihe von Zahlen zu berechnen.
Beispiel: (durchschnitt 2 3 4) => 3"
  (if (null liste)
      nil
      (/ (reduce #'+ liste) (length liste))))

(defun faktor (n)
  "(faktor zahl)
FAKTOR berechnet den Faktor einer Zahl.
Ein Faktor von 6 wird zum Beispiel errechnet, indem man die Werte von 1 bis 6 miteinander malnimmt, also 1 * 2 * 3 * 4 * 5 * 6. Faktoren haben die unangenehme Eigenschaft, das sie sehr schnell sehr groß werden können.
Beispiel: (faktor 20) =>  2432902008176640000"
  (if (eql n 0) 
    1 
    (* n (faktor (1- n)))))

(defun faktor-festlegen (wert)
  "(faktor-festlegen wert)
FAKTOR-FESTLEGEN dient dazu, einen Rechenfaktor für die Maßeinheit zurückzugeben.
Beispiel: (faktor-festlegen 'mm) => 1/1000"
  (case wert
    ((yoctometer) 10e-24)
    ((zeptometer) 10e-21)
    ((am attometer) 10e-18)
    ((femtometer fm) 10e-15)
    ((picometer pm) 1/1000000000000) ; 10e-12
    ((Ångström Å) 1/10000000000) ; 10e-10
    ((nanometer nm) 1/1000000000) ; 10e-9
    ((mikrometer mm2 µm quadratmillimeter) 1/1000000) ; 10e-6
    ((cm2 quadratzentimeter) 1/10000) ; 10e-4
    ((mm tausendstel) 1/1000) ; 10e-3
    ((cm dm2 hundertstel quadratdezimeter zentimeter) 1/100) ; 10e-2
    ((inch zoll) 0.0254)
    ((dm dezimeter zehntel) 1/10) ; 10e-1
    ((foot fuß) 0.3048) ; 12 inches
    ((gerte schritt yard yd) 0.9144) ; 3 feet
    ((bit g gramm m m2 meter qm quadratmeter) 1)
    ((fathom fth) 1.8288) ; 6 feet
    ((byte octet oktett) 8)
    ((square-foot) 10.7639)
    ((dutzend) 12)
    ((shackle shot) 27.432) ; 15 fathom
    ((a ar hundert) 100) ; 10e2
    ((gros gröthen gruessa tylt) 144)
    ((pfund) 500)
    ((kb kg kilobyte kilogramm kilometer myriameter tausend) 1000) ; 10e3
    ((kib kibibyte) 1024) ; 2e10
    ((square-inch) 1550.0031)
    ((meile mile) 1609.344) ; 5280 feet
    ((großes-gros großgros maß) 1728)
    ((seemeile) 1852)
    ((international-nautical-mile) 1852.01)
    ((acre) 4046.8564)
    ((league nautical-league sea-league) 5559.552) ; 3 admirality sea miles
    ((ha hektar zehntausend) 10000) ; 10e4
    ((zentner ztr) 50000)
    ((dezitonne dezitonnen doppelzentner dt dz) 100000) ; 10e5
    ((km2 mb megabyte megameter million million mg quadratkilometer t tonne tonnen) 1000000) ; 10e6
    ((mib mebibyte) 1048576) ; 2e20
    ((square-mile) 2589988.1103)
    ((gb gigabyte gigameter gm kilotonne kilotonnen kt milliarde) 1000000000) ; 10e9
    ((gib gibibyte) 1073741824) ; 2e30
    ((billion megatonne mt tb terabyte terameter tm) 1000000000000) ; 10e12
    ((tebibyte tib) 1099511627776) ; 2e40
    ((billiarde pb petabyte petameter) 10e15)
    ((pebibyte pib) 1125899906842624) ; 2e50
    ((eb exabyte em exameter) 10e18)
    ((exbibyte eib) 1152921504606846976) ; 2e60
    ((zb zettabyte zettameter) 10e21)
    ((zebibyte zib) 1180591620717411303424) ; 2e70
    ((yb yottabyte yottameter) 10e24)
    ((yobibyte yib) 1208925819614629174706176) ; 2e80
    (otherwise nil)))

(defun faktorisiere (n)
  "(faktorisiere n)
Gibt eine Liste der Faktoren der Zahl N zurück.
Beispiel: (faktorisiere 1000) => (2 2 2 5 5 5)"  
  (when (and (integerp n) (> n 1))
    (loop with max-d = (isqrt n)
	  for d = 2 then (if (evenp d) (+ d 1) (+ d 2)) do
	  (cond ((> d max-d) (return (list n))) ; n ist eine Primzahl
		((zerop (rem n d)) (return (cons d (faktorisiere (truncate n d)))))))))

(defun fibonacci-reihe (n) "Bildet die Fibonacci-Reihe zur n. Zahl; Beispiel: (fibonacci-reihe 20) => 6765"
  (if (< n 2)
      n
      (+ (fibonacci-reihe (- n 2)) (fibonacci-reihe (- n 1)))))

;;; Kilometer-pro-Tankfüllung
(defun km-pro-tankfüllung (anfang ende spritmenge)
  (values (/ (- ende anfang) spritmenge) 'km/l))

(defun lottoziehung (ziehungen gesamt &optional (zurücklegen nil))
  "(lottoziehung ziehungen gesamt)
LOTTOZIEHUNG gibt eine anzahl an ziehungen Objekten zurück die aus einer Menge gesamt gezogen werden. Ist das optionale Argument zurücklegen NIL, so werden die Objekte entnommen und können nicht wiederverwendet werden.
Beispiel: (lottoziehung 6 49) => (37 44 13 41 4 3)"
  (let ((zahlen nil))
    (do ((i 1 (1+ i)))
	((or (and (null zurücklegen) (= (length zahlen) ziehungen)) ; ohne zurücklegen
	     (and (not (null zurücklegen)) (> i ziehungen))) ; mit zurücklegen
	 zahlen)
      (if (null zurücklegen)
	  (setq zahlen (adjoin (würfelwurf gesamt) zahlen)) ; ohne zurücklegen
	  (push (würfelwurf gesamt) zahlen))))) ; mit zurücklegen

(defun münzwurf ()
  "Münzwurf bildet den Wurf einer Münze nach. Es ist möglich, daß die Münze auf der Kante stehen bleibt! Beispiel: (münzwurf) => ZAHL"
       (let ((wurf (random 101)))
	 (cond ((< wurf 50) 'kopf)
	       ((> wurf 50) 'zahl)
	       (t 'kante))))

(defun preis-änderung (alter-preis neuer-preis)
  "(preis-änderung alter-preis neuer-preis)
PREIS-ÄNDERUNG berechnet die Veränderung in Prozentpunkten zwischen dem alten und dem neuen Preis, wobei der alte Preis mit 100% gleichgesetzt wird. Die Funktion wird aufgerufen in der Form: (preis-änderung alter-preis neuer-preis). Beispiel: (preis-änderung 1.00 1.10)
10.000002
 (PREISÄNDERUNG UM 10.000002 %)"
  (let* ((differenz (- neuer-preis alter-preis))
	 (verhältnis (/ differenz alter-preis))
	 (prozentpunkte (* verhältnis 100.0)))
    (values prozentpunkte (list 'Preisänderung 'um prozentpunkte '%))))

;;; Pythagoras
(defun pythagoras (a b)
  "(pythagoras a b)
PYTHAGORAS berechnet aufgrund der Wurzel aus a² und b² c, nach dem Satz a²+b² = c²
Beispiel: (pythagoras 3 4) => 5.0"
  (sqrt (+ (* a a) (* b b))))

(defun quadratzahl (x)
  "(quadratzahl zahl)
QUADRATZAHL berechnet die Quadratzahl einer gegebenen Zahl.
Beispiel: (quadratzahl 1024) => 1048576)"
       (* x x))

(defun sieb-des-eratosthenes (maximum)
  (let ((composites (make-array (1+ maximum) :element-type 'bit
                                             :initial-element 0)))
    (loop for candidate from 2 to maximum
          when (zerop (bit composites candidate))
            collect candidate
            and do (loop for composite from (expt candidate 2) to maximum by candidate
                         do (setf (bit composites composite) 1)))))

(defun temperatur (wert &optional (größeneinheit 'celsius))
  "(temperatur wert &optional größeneinheit)
TEMPERATUR wandelt den angegebenen Temperaturwert in eine Liste der Werte aller drei Maßsysteme um."
  (let ((kelvin (case größeneinheit
		  ((celsius c) (+ wert 273.15))
		  ((fahrenheit f) (* (+ wert 459.67) 5/9))
		  ((kelvin k) wert)
		  (otherwise nil))))
    (when kelvin
      (values (list kelvin 'kelvin) (list (- kelvin 273.15) 'celsius) (list (- (* kelvin 1.8) 459.67) 'fahrenheit)))))

(defun umwandeln (wert ausgangsgröße ergebnisgröße)
  "(umwandeln wert größe1 größe2)
UMWANDELN dient dazu, eine Zahl von einer Maßeinheit in eine andere umzurechnen.
Beispiel: (umwandeln 10 'cm 'mm) => 100 MM"
  (if (eql ausgangsgröße ergebnisgröße)
      (values wert ergebnisgröße)
      (let ((faktor1 (faktor-festlegen ausgangsgröße))
	    (faktor2 (faktor-festlegen ergebnisgröße)))
	(values (/ (* wert faktor1) faktor2)
		ergebnisgröße)))) 

(defun würfelwurf (&optional (seiten 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (1+ (random seiten)))



;;; ================================
;;; Abschnitt 5 - Listenverarbeitung
;;; ================================

(defun alle-permutationen (liste)
  "Alle Permutationen einer Liste erzeugen; Beispiel: (alle-permutationen (list 'a 'b 'c 'd 'e))"
  (if (null liste) '(())
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (alle-permutationen (remove x liste :count 1)))) liste)))

(defun entferne-letztes (liste)
  "(entferne-letztes liste)
ENTFERNE-LETZES entfernt das letzte Top-Level-Element einer Liste.
Beispiel: (letztes-weg '(a b c d)) => (A B C)"
;ehemals  (rückwärts (rest (rückwärts liste))))
  (if (null liste)
      nil				 
      (when (rest liste)
	  (cons (first liste) (entferne-letztes (rest liste))))))

(defun ersetze (ausdruck1 ausdruck2 liste)
  "Ersetze Ausdruck1 durch Ausdruck2 in der Liste; Beispiel: (ersetze 'hallo 'holla '(Hallo Welt!)) => (HOLLA WELT!)"
   (cond ((endp liste) nil)
         ((equal (first liste) ausdruck1) 
          (cons ausdruck2
                 (ersetze ausdruck1 ausdruck2 (rest liste))))
         (t 
          (cons (first liste)
                 (ersetze ausdruck1 ausdruck2 (rest liste))))))

(defun ersetze-letztes (item liste)
  "Ersetzt das letzte Element einer Liste durch item; Beispiel: (ersetze-letztes 'delta '(alpha beta gamma)) => (ALPHA BETA DELTA)"
       (rückwärts (cons item (rest (rückwärts liste)))))

(defun hinten-einfügen (item liste)
  "(hinten-einfügen item liste)
HINTEN-EINFÜGEN ist ein umgekehrtes cons, das Item wird an das Ende der Liste gesetzt, nicht an den Anfang.
Beispiel: (hinten-einfügen 'Alpha '(Beta Gamma)) => (BETA GAMMA ALPHA)
Hinweis: (SNOC item liste) ist eine weitaus schnellere Fassung dieser Funktion." 
  (rückwärts (cons item (rückwärts liste))))

(defun königliches-wir (liste)
  "(königliches-wir liste)
KÖNIGLICHES-WIR tauscht in einer Liste alle vorkommen von I oder ICH gegen WE oder WIR aus, ganz so sie Liz 2 es von sich gibt."
  (sublis '((i . we) (ich . wir)) liste))

(defun nvertausche-anfang-und-ende (x)
  "(nvertausche-anfang-und-ende liste)
NVERTAUSCHE-ANFANG-UND-ENDE ist eine Funktion, die das erste und letzte CONS-Element einer Liste gegeneinander austauscht. Die Funktion ist destruktiv, die Ursprungsliste wird abgeändert!
Es gibt auch eine nicht-destruktive, dafür aber langsamere Variante, VERTAUSCHE-ANFANG-UND-ENDE.
Beispiele: (nvertausche-anfang-und-ende '(a b c d)) => (D B C A)
 (defparameter *test* '(a b c d e)) => *TEST*
 (time (nvertausche-anfang-und-ende *test*)) => (E B C D A)
 *test* => (E B C D A)"
  (if (atom x)
      nil
      (let ((erstes (first x))
	    (letztes (first (last x))))
	(setf (first x) letztes
	      (first (last x)) erstes)
	x)))

(defun rotiere-nach-links (x)
  "(rotiere-nach-links liste)
ROTIERE-NACH-LINKS schiebt bildlich gesprochen das linke Element aus der Liste links heraus und fügt es rechts an.
Beispiel: (rotiere-nach-links '(a b c d)) => (B C D A)"
  (snoc (first x) (rest x)))

(defun rotiere-nach-rechts (x)
  "(rotiere-nach-rechts liste)
ROTIERE-NACH-RECHTS schiebt bildlich gesprochen das rechte Element aus der Liste heraus und fügt es links an.
Beispiel: (rotiere-nach-links '(a b c d)) => (D A B C)"
  (append (last x) (reverse (rest (reverse x)))))

(defun snoc (cdr-wert car-wert)
  "(snoc cdr-wert car-wert)
SNOC ist quasi die Umkehrung der CONS-Funktion, der cdr-Wert wird an das Ende der durch den car-Wert dargestellten Liste gesetzt. Ist der cdr-wert keine Liste, so erhält man eine punktierte Liste.
Beispiele: (snoc 'd '(a b c)) => (A B C . D)
 (snoc '(4 5) '(1 2 3)) => (1 2 3 4 5)
 (snoc '(b) 'a) => (a b)"
  (if (atom car-wert)
    (append (list car-wert) cdr-wert)
    (append car-wert cdr-wert)))

(defun tausche (i j liste)
  "(tausche i j liste)
TAUSCHE ermöglicht es, zwei Elemente einer Liste auszutauschen, ohne daß die Ursprungsliste hierbei zerstört wird.
Beispiel: (setq liste '(a b c d e)) => (A B C D E)
 (tausche 'b 'd liste) => (A D C B E)
liste => (A B C D E)"
  (let ((eliste (copy-tree liste)))
    (rotatef (first (member i eliste)) (first (member j eliste)))
    eliste))

(defun vertausche-anfang-und-ende (x)
  "(vertausche-anfang-und-ende liste)
VERTAUSCHE-ANFANG-UND-ENDE ist eine Funktion, die das erste und letzte CONS-Element einer Liste gegeneinander austauscht. Die Funktion ist nicht destruktiv, die Ursprungsliste bleibt erhalten.
Es gibt auch eine destruktive, dafür aber schnellere, Variante dieser Funktion, NVERTAUSCHE-ANFANG-UND-ENDE.
Beispiele: (vertausche-anfang-und-ende '(a b c d)) => (D B C A)
 (defparameter *test* '(a b c d e)) => *TEST*
 (time (vertausche-anfang-und-ende *test*)) => (E B C D A)
 *test* => (A B C D E)"
  (if (or (atom x) (endp x))
      nil
      (append (rotiere-nach-rechts (rest x)) (list (first x)))))

(defun zuletzt (a)
  "(zuletzt item)
ZULETZT gibt das letzte Element von item zurück.
Beispiel:  (zuletzt '((a b c) (d) (e (f (g h))))) => H
 (zuletzt 'a) => A"
       (cond ((atom a) a)
	     ((null a) nil)
	     ((null (rest a)) (if (consp (first a))
				  (zuletzt (rest (first a)))
				  (first a)))
	     (t (zuletzt (rest a)))))



;;; ==========================
;;; Abschnitt 6 - Ein-/Ausgabe
;;; ==========================

(defun hole-zahl (string)
  "(hole-zahl string)
HOLE-ZAHL gibt die Zeichenkette String aus und erzwingt die Eingabe einer Zahl."
  (format t "~A" string)
  (let ((zahl (read)))
    (if (not (numberp zahl))
	(hole-zahl string)
	zahl)))

(defun sicheres-lesen-aus-string (string &rest read-from-string-args)
  "(sicheres-lesen-aus-string string &rest read-from-string-args)
SICHERES-LESEN-AUS-STRING ermöglicht die Nutzung der Funktion read-from-string, wobei sie fehlertoleranter ist."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string string read-from-string-args))))



;;; =============================
;;; Abschnitt 7 - Lernalgorithmen
;;; =============================
;;; Sie dienen um Techniken / Strukturen zu vermitteln

(defun enthält-artikel-p (liste)
  "(enthält-artikel-p liste)
ENTHÄLT-ARTIKEL-P prüft eine übergebene Liste, ob sie einen Artikel aus der Menge der bekannten Artikel enhält.
Beispiel: (enthält-artikel-p '(der mann)) => (DER)"
  (schnittmenge '(der des dem den die das ein eine einer eines einem einen) liste))

(defun mein-entferne-wenn (funktion liste)
  "(mein-entferne-wenn funktion liste)
MEIN-ENTFERNE-WENN entfernt ein Element, wenn die angewandte Funktion auf dieses Element den Wert T zurückliefert.
Beispiel: (mein-entferne-wenn #'oddp '(1 2 3 4 5 6)) => (2 4 6)"
  (unless (null liste)
    (if (funcall funktion (first liste)) ; Übereinstimmung beim nächsten first?
	(mein-entferne-wenn funktion (rest liste)) ; ja, daher Neuaufruf mit dem Rest der Liste
	(cons (first liste) (mein-entferne-wenn funktion (rest liste)))))) ;; das first erhalten - und mit dem Rest der liste fortfahren

(defun mein-subst (neu alt baum)
  (if (eql alt baum)
      neu		   ; Tauscht aus, wenn ein Treffer erfolgt ist
      (if (atom baum)
	  baum ; Da baum ein Atom ist, wurde das letzte Element erreicht, das Atom wird zurückgegeben
	  (cons (mein-subst neu alt (first baum)) ;; bearbeitetet das aktuelle Element ab (Atom oder Liste)
		(mein-subst neu alt (rest baum)))))) ;; und fährt mit dem Rest fort



;;; ====================================================================
;;; Abschnitt 8 - Deutsche Übersetzungen der Dokumentationszeichenketten
;;; ====================================================================

(dokumentation-austauschen 'append 'function "(append &rest liste{n})
APPEND nimmt Listen entgegen und verkettet deren Elemente zu einer neuen Liste. Hierbei zeigt jeweils der CDR-Pointer des Vorgängers auf seinen Nachfolger in der Kette.
Übergibt man keinerlei Argumente an APPEND, so wird NIL zurückgegeben.
Ist das letzte Element keine Liste, so erhält man eine punktierte LISTE als Ergebnis.
Ist ein anderes als das letzte Argument keine Liste, so ist dies ein Fehler.
APPEND ist eine nicht-destruktive Funktion.
Beispiele: (append '(1) '(2) '(3)) => (1 2 3)
 (append) => NIL
 (append '((a) (b)) '((c) (d e))) => ((A) (B) (C) (D E))
 (setf wer '(nur die guten)) => (NUR DIE GUTEN)
 (append wer '(sterben jung)) => (NUR DIE GUTEN STERBEN JUNG)
 (append '(w x y) 'z) => (W X Y . Z)
 (append 'z) => Z")

(dokumentation-austauschen 'apply 'function "(apply funktion argument &rest mehr-argumente)
APPLY wendet eine Funktion auf ein oder mehrere Argumente in der Art und Weise wie LIST* es ihr gleicht tut. Das bedeutet, das bedeutet, das aus allen Argumenten ausser dem letzten eine Liste gemacht wird, die vorangesetzt wird an das letzte Argument, das seinerseits bereits eine Liste sein muss.
Beispiel: (apply #'+ '1 '2 '(3 4)) => 10")

(dokumentation-austauschen 'assoc 'function "(assoc objekt a-liste &key :test :test-not :key)
ASSOC durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert dem von Objekt entspricht, oder aber NIL. 
ASSOC verfügt über zwei Kind-Funktionen: ASSOC-IF und ASSOC-IF-NOT.
Beispiel: (assoc 'b '((a hubbel) (b dubbel) (c gubbel))) => (B DUBBEL)")

(dokumentation-austauschen 'assoc-if 'function "(assoc-if prädikat a-liste &key :key)
ASSOC-IF durchsucht die A-Liste nach einer Assoziation anhand des Prädikats. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert die Prüfung von Prädikat erfolgreich besteht, oder aber NIL. 
ASSOC-IF verfügt über eine Eltern-Funktion, ASSOC und eine Bruder-Funktionen, ASSOC-IF-NOT.
Beispiel: (assoc-if #'oddp '((2 hubbel) (3 dubbel) (5 gubbel))) => (3 DUBBEL)")

(dokumentation-austauschen 'assoc-if-not 'function "(assoc-if-not prädikat a-liste &key :key)
ASSOC-IF-NOT durchsucht die A-Liste nach einer Assoziation anhand des Prädikats. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert die Prüfung von Prädikat nicht besteht, oder aber NIL. 
ASSOC-IF-NOT verfügt über eine Eltern-Funktion, ASSOC und eine Bruder-Funktionen, ASSOC-IF.
Beispiel: (assoc-if-not #'oddp '((2 hubbel) (3 dubbel) (5 gubbel))) => (2 HUBBEL)")

(dokumentation-austauschen 'car 'function "(car liste)
Die Bedeutung des Mnemonics von CAR ist \"Contents of the Adress Register\" bei einer IBM 704.
CAR gibt das erste Objekt eines CONStructs / einer Liste oder NIL zurück. Die Verwendung von CAR geht in die Ursprünge von Lisp zurück, einfacher zu lesen ist stattdessen die gleichwertige Verwendung des Befehls FIRST.
Beispiel: (car '(a b)) => A")

(dokumentation-austauschen 'cdr 'function "(cdr liste)
Die Bedeutung des Mnemonics von CDR ist \"Contents of the Decrement Register\" bei einer IBM 704.
CDR gibt eine Liste der Objekte nach dem ersten Objekt einer Liste oder NIL zurück.
Die Verwendung von REST anstelle von CDR kann einen Quelltext sehr viel lesbarer machen, beide Funktionen sind Synonyme.
Beispiel: (cdr '(a b c)) => (B C)")

(dokumentation-austauschen 'cons 'function "(cons objekt1 objekt2)
Die Bedeutung des Mnemonics von CONS ist die Abkürzung von CONStruct.
CONS erzeugt eine neues Konstrukt, in CL auch CONS genannt, das sich zu einer Liste aus Objekt1 und Objekt2 zusammensetzt.
CONS erzeugt genau eine neue CONS-Zelle. Es wird oftmals eingesetzt, um ein neues Element an den Anfang einer Liste zu setzen.
Hinweis: Mit der selbgeschriebenen Funktion SNOC kann ein Element an das Ende einer Liste gesetzt werden.
Beispiel: (cons 'a '(b)) => (A B)
 (cons 'a 'b) => (A . B)")

(dokumentation-austauschen 'intersection 'function "(intersection list1 list2 &key :test :test-not :key)
INTERSECTION fasst alle Elemente, die in beiden Listen, list1 und list2 vorhanden sind und,  wenn angegeben :test oder :test-not, das Testkriterium bestehen, zu einer neuen Liste, also einer Schnittmenge (engl. intersection) zusammen. Die Reihenfolge der Elemente in der neuen Liste ist implementationsabhängig und gilt als nicht vorhersehbar. Wird eine bestimmte Reihenfolge benötigt, so muß das Ergebnis entsprechend noch mit Hilfer der SORT-Funktion sortiert werden.
INTERSECTION ist eine nicht-destruktive Funktion, hat aber auch eine Geschwisterfunktion, NINTERSECTION, die unter Umständen liste1 zerstören würde - und daher einen destruktiven Seiteneffekt hat.
Beispiel: (intersection '(a b c d) '(f a d s l)) => (A D)")

(dokumentation-austauschen 'last 'function "(last liste &optional (n 1))
LAST gibt die Verkettung der letzten N CONS-Werte zurück, nicht die letzen N Elemente.
Ist die Liste eine leere Liste wird NIL zurückgegeben.
Ist N Null, so wird das Atom zurückgeliefert, das die gesamte Liste beendet.
Beispiele:  (last nil) =>  NIL
 (last '(1 2 3)) =>  (3)
 (last '(1 2 . 3)) =>  (2 . 3)
 (setq x (list 'a 'b 'c 'd)) =>  (A B C D)
 (last x) =>  (D)
 (last '(a b c))   =>  (C)
 (last '(a b c) 0) =>  ()
 (last '(a b c) 1) =>  (C)
 (last '(a b c) 2) =>  (B C)
 (last '(a b c) 3) =>  (A B C)
 (last '(a b c) 4) =>  (A B C)
 (last '(a . b) 0) =>  B
 (last '(a . b) 1) =>  (A . B)
 (last '(a . b) 2) =>  (A . B)")

(dokumentation-austauschen 'list 'function "(list & rest argumente)
LIST konstruiert eine Liste aus den der Funktion übergebenen Argumenten, wobei diese notfalls zuerst evaluiert werden - und gibt die so entstandene Liste zurück.
LIST bildet Listen aus einer Serie von CONS-Zellen und beendet die neue Liste mit NIL. Das CAR jeder Zelle deutet auf das übergebene Argument hin.
Beispiele: (list 'a 'b) => (A B)
 (list 1 2 'ö (car '(a . b)) (+ 7 -3)) => (1 2 Ö A 4)")

(dokumentation-austauschen 'member 'function "(member objekt liste &key :test :test-not :key)
MEMBER überprüft anhand von :test (voreingestellt ist hier EQL) oder :test-not, ob ein Objekt ein Element der Liste ist. Das voreingestellte EQL prüft hierbei nur die oberste Hierarchie-Ebene.
Ist die Suche erfolgreich, wird eine Unterliste, beginnend ab dem gefundenen Objekt bis zum Ende der Liste zurückgegeben, was auch als T betrachtet wird. Ist die Suche erfolglos, wird NIL zurückgeliefert.
MEMBER hat zwei Abkömmlinge, MEMBER-IF und MEMBER-IF-NOT.
Beispiele: (member 'nerd '(a b c d)) => NIL
 (member 'b '(a b c d)) => (B C D)")

(dokumentation-austauschen 'member-if 'function "(member-if prädikat liste &key :key)
MEMBER überprüft anhand von Prädikat, ob ein zutreffendes Objekt in der Liste vorkommt oder nicht.
Ist die Suche erfolgreich, wird eine Unterliste, beginnend ab dem gefundenen Prädikattreffer bis zum Ende der Liste zurückgegeben, was auch als T betrachtet wird. Ist die Suche erfolglos, wird NIL zurückgeliefert.
MEMBER hat eine Eltern-Funktion: MEMBER und eine Geschwister-Funktion: MEMBER-IF-NOT.
Beispiel: (member-if  #'oddp '(2 3 4)) => (3 4)")

(dokumentation-austauschen 'member-if-not 'function "(member-if-not prädikat liste &key :key)
MEMBER überprüft anhand von Prädikat, ob ein zutreffendes Objekt in der Liste nicht vorkommt.
Ist die Suche erfolgreich, wird eine Unterliste, beginnend ab dem gefundenen Prädikattreffer bis zum Ende der Liste zurückgegeben, was auch als T betrachtet wird. Ist die Suche erfolglos, wird NIL zurückgeliefert.
MEMBER hat eine Eltern-Funktion: MEMBER und eine Geschwister-Funktion: MEMBER-IF.
Beispiel: (member-if-not  #'oddp '(1 2 3 4)) => (2 3 4)")

(dokumentation-austauschen 'nintersection 'function "(nintersection list1 list2 &key :test :test-not :key)
NINTERSECTION fasst alle Elemente, die in beiden Listen, list1 und list2 vorhanden sind und,  wenn angegeben :test oder :test-not, das Testkriterium bestehen, zu einer neuen Liste, also einer Schnittmenge (engl. intersection) zusammen. Die Reihenfolge der Elemente in der neuen Liste ist implementationsabhängig und gilt als nicht vorhersehbar. Wird eine bestimmte Reihenfolge benötigt, so muß das Ergebnis entsprechend noch mit Hilfer der SORT-Funktion sortiert werden.
NINTERSECTION ist eine nicht-destruktive Funktion, hat aber auch eine Geschwisterfunktion, NINTERSECTION, die unter Umständen liste1 zerstören würde - und daher einen destruktiven Seiteneffekt hat.
Beispiel: (intersection '(a b c d) '(f a d s l)) => (A D)")

(dokumentation-austauschen 'nset-difference 'function "(nset-difference liste1 liste2 &key :test :test-not :key)
NSET-DIFFERENCE ermittelt die Differenzmenge von Liste1 zu Liste2. Die zurückgegebene Liste enthält also alle Elemente aus Liste1, die nicht in Liste2 vorhanden sind. Der Ausgang kann mit Hilfe der verschiedenen Schlüssel beeinflusst werden.
NSET-DIFFERENCE ist eine destruktive Funktion, die vermutlich Liste1 zerstören wird. Sie verfügt auch über eine nicht-destruktive Bruderfunktion, SET-DIFFERENCE.
Beispiel: (nset-difference '(a b c d) '(c d e f)) => (B A)")

(dokumentation-austauschen 'nsubst 'function "(nsubst neu alt baum &key :test :test-not :key)
NSUBST erstellt eine Kopie des Baumes, wobei ein Austausch von Alt gegen Neu erfolgt. Der Austausch kann zusätzlich durch die Schlüsselworte beeinflusst werden. Der Orginalbaum wird vermutlich zerstört, der neue Baum kann jedoch Teile des Originalbaums erhalten.
NSUBST verfügt über zwei Kind-Funktionen, NSUBST-IF und NSUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine nicht zerstörerische Version, die den Ursprungsbaum zerstören kann, SUBST, SUBST-IF und SUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (nsubst 'tempest 'hurricane '(shakespeare wrote (the hurricane))) =>  (SHAKESPEARE WROTE (THE TEMPEST))")

(dokumentation-austauschen 'nsubst-if 'function "(nsubst-if neu prädikat baum &key :key)
NSUBST-IF erstellt eine Kopie des Baumes, wobei ein Austausch erfolgt, wenn ein Blatt oder Ast das Prädikat erfüllt. Der Austausch erfolgt gegen Neu, wobei dieser zusätzlich durch das Schlüsselwort beeinflusst werden kann. Der Orginalbaum wird vermutlich zerstört, der neue Baum kann jedoch Teile des Originalbaums erhalten.
NSUBST-IF verfügt über eine Elternfunktion, NSUBST und eine Bruderfunktion, NSUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine nicht zerstörerische Version, die den Ursprungsbaum nicht zerstören, SUBST, SUBST-IF und SUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
 (nsubst-if 5 #'oddp tree1) =>  5")

(dokumentation-austauschen 'nth 'function "(nth n liste)
NTH gibt den CAR-Wet des Elements ab Position N zurück, wobei mit der Zählung beim nullten Element begonnen wird.
 (nth n liste) ist gleichwertig zu (car (nthcdr n liste)).
Ist die übergebene Liste eine punktierte Liste, darf nicht über den letzten CAR-Wert hinaus abgefragt werden, ansonsten kommt es zu einem Fehler.
Bei einer nicht punktierten Liste wird der Wert NIL zurückgeliefert, wenn über den letzen CAR-Wert hinaus eine Abfrage getroffen wird.
Beispiele: (nth 0 '(a b . c)) => A
 (nth 1 '(a b . c)) => B
 (nth 2 '(a b . c)) => FEHLER
 (nth 3 '(a b c)) => NIL")

(dokumentation-austauschen 'nthcdr 'function "(nthcdr n liste)
NTHCDR gibt den CDR-Wert von Liste ab der der mit N angegeben Position zurück. Wird 0 für N eingegeben, wird die vollständige Liste zurückgegeben. Ist N größer oder gleich der Position des letzten Elements, so wird NIL zurückgegeben.
Dies gilt nicht, wenn eine punktierte Liste übergeben wurde. Geht man hier über das abschließende Element hinaus, so kommt es zu einer Fehlermeldung.
Beispiele: (nthcdr 1 '(a b c)) => (B C)
 (nthcdr 3 '(a b c)) => NIL
 (nthcdr 0 '(a b c)) => (A B C)
 (nthcdr 2 '(a b . c)) => C
 (nthcdr 3 '(a b . c)) => FEHLER")

(dokumentation-austauschen 'rassoc 'function "(rassoc objekt a-liste &key :test :test-not :key)
RASSOC durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CDR-Wert dem von Objekt entspricht, oder aber NIL. 
RASSOC verfügt über zwei Kind-Funktionen: RASSOC-IF und RASSOC-IF-NOT.
RASSOC benötigt A-Listen, deren Objekte Paare aus gepunkteten Listen sind.
Beispiel: (rassoc 'b '((a hubbel) (b dubbel) (c gubbel))) => NIL
 (rassoc 'dubbel '((a . hubbel) (b . dubbel) (c . gubbel))) => (B . DUBBEL)")

(dokumentation-austauschen 'rassoc-if 'function "(rassoc-if prädikat a-liste &key :key)
RASSOC-IF durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CDR-Wert dem Prädikat entspricht, oder aber NIL. 
RASSOC-IF verfügt über eine Eltern-Funktion, RASSOC, und eine Bruderfunktion, RASSOC-IF-NOT.
RASSOC-IF benötigt A-Listen, deren Objekte Paare aus gepunkteten Listen sind.
Beispiel: (rassoc-if 'b '((a hubbel) (b dubbel) (c gubbel))) => NIL
 (rassoc-if #'oddp '((hubbel . 2) (dubbel . 3) (gubbel . 4))) => (DUBBEL . 3)")

(dokumentation-austauschen 'rassoc-if-not 'function "(rassoc-if-not prädikat a-liste &key :key)
RASSOC-IF-NOT durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CDR-Wert nicht dem Prädikat entspricht, oder aber NIL.
RASSOC-IF-NOT verfügt über eine Eltern-Funktion, RASSOC, und eine Bruderfunktion, RASSOC-IF.
RASSOC-IF-NOT benötigt A-Listen, deren Objekte Paare aus gepunkteten Listen sind.
Beispiel: (rassoc-if-not 'b '((a hubbel) (b dubbel) (c gubbel))) => NIL
 (rassoc-if-not #'oddp '((hubbel . 2) (dubbel . 3) (gubbel . 4))) => (HUBBEL . 2)")

(dokumentation-austauschen 'remove 'function "(remove objekt reihenfolge &key :from-end :test :test-not :start :end :count :key)
REMOVE liefert eine Reihenfolge zurück, die weitestgehend der übergebenen Reihenfolge entspricht, jedoch wurden jene zwischen :start und :end herausgenommen, die dem :test entsprachen. Die Vorgabe für :test ist der Vergleich per EQL auf Objekt. Das :count Argument, wenn angegeben, sorgt dafür das nicht mehr Argumente als gewünscht entfernt werden. Ausschließlich wenn das :count Argument angegeben wurde. In diesem Fall arbeitet die Funktion vom Ende aus rückwerts die Liste ab.
REMOVE ist nicht destruktiv, die Ursprungsliste wird nicht verändert.
REMOVE hat zwei Abkömmlinge: REMOVE-IF und REMOVE-IF-NOT.
Beispiele: (remove 4 '(1 3 4 5 9)) =>  (1 3 5 9)
 (remove 4 '(1 2 4 1 3 4 5)) =>  (1 2 1 3 5)
 (remove 4 '(1 2 4 1 3 4 5) :count 1) =>  (1 2 1 3 4 5)
 (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t) =>  (1 2 4 1 3 5)
 (remove 3 '(1 2 4 1 3 4 5) :test #'>) =>  (4 3 4 5)")

(dokumentation-austauschen 'remove-duplicates 'function "(remove-duplicates reihenfolge &key :from-end :test :start :end :key)
REMOVE-DUPLICATES überprüft die Reihenfolge paarweise, und wenn zwei daraus übereinstimmend sind, wird das erste entfernt, es sei denn FROM-END ist wahr, dann wird das letztere entfernt. Andere Schlüsselworte können ebenfalls einen Einfluss auf die Überprüfung auswirken. Das Ergebnis wird zurückgeliefert.
Die Funktion ist destruktiv, die Original-Reihenfolge wird vermutlich zerstört werden.
Das Schlüsselwort :test-not gilt als veraltet.
Beispiel: (remove-duplicates '(a b c b d d e)) => (a d b c e)")

(dokumentation-austauschen 'remove-if 'function "(remove-if test reihenfolge &key :from-end :start :end :count :key)
REMOVE-IF liefert eine Reihenfolge zurück, die weitestgehend der übergebenen Reihenfolge entspricht, jedoch wurden jene zwischen :start und :end herausgenommen, die dem test entsprachen. Das :count Argument, wenn angegeben, sorgt dafür das nicht mehr Argumente als gewünscht entfernt werden. Ausschließlich wenn das :count Argument angegeben wurde. In diesem Fall arbeitet die Funktion vom Ende aus rückwerts die Liste ab.
REMOVE-IF ist nicht destruktiv, die Ursprungsliste wird nicht verändert.
REMOVE-IF ist ein Abkömmling von REMOVE und hat REMOVE-IF-NOT als Bruder an seiner Seite.
Beispiele: (remove-if #'oddp '(1 2 4 1 3 4 5)) => (2 4 4)
 (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t) => (1 2 4 1 3 5)")

(dokumentation-austauschen 'remove-if-not 'function "(remove-if-not test reihenfolge &key :from-end :start :end :count :key)
REMOVE-IF-NOT liefert eine Reihenfolge zurück, die weitestgehend der übergebenen Reihenfolge entspricht, jedoch wurden jene zwischen :start und :end herausgenommen, die dem test nicht entsprachen. Das :count Argument, wenn angegeben, sorgt dafür das nicht mehr Argumente als gewünscht entfernt werden. Ausschließlich wenn das :count Argument angegeben wurde. In diesem Fall arbeitet die Funktion vom Ende aus rückwerts die Liste ab.
REMOVE-IF-NOT ist nicht destruktiv, die Ursprungsliste wird nicht verändert.
REMOVE-IF-NOT ist ein Abkömmling von REMOVE und hat REMOVE-IF als Bruder an seiner Seite.
Beispiel: (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
=>  (1 2 3 4 5 6 8)")

(dokumentation-austauschen 'reverse 'function "(reverse &rest reihenfolge)
REVERSE nimmt eine Sequenz, eine Reihenfolge entgegen - und kehrt sie um. Hierbei wird jedoch ausschließlich eine Umkehrung der obersten Elementebene durchgeführt.
Es wird eine Liste erzeugt, die ursprünglichen Werte werden nicht verändert, die Funktion ist also non-destruktiv.
Beispiel: (reverse '(a b c d)) => (D C B A)")

(dokumentation-austauschen 'set-difference 'function "(set-difference liste1 liste2 &key :test :test-not :key)
SET-DIFFERENCE ermittelt die Differenzmenge von Liste1 zu Liste2. Die zurückgegebene Liste enthält also alle Elemente aus Liste1, die nicht in Liste2 vorhanden sind. Der Ausgang kann mit Hilfe der verschiedenen Schlüssel beeinflusst werden.
SET-DIFFERENCE ist eine nicht-destruktive Funktion. Sie verfügt über eine destruktive Bruderfunktion, NSET-DIFFERENCE, die bei ihrer Anwendung vermutlich Liste1 zerstören wird.
Beispiel: (set-difference '(a b c d) '(c d e f)) => (B A)")

(dokumentation-austauschen 'setf 'function "(setf ort1 wert1 {ort2 wert2 {ortN wertN}})
Die Bedeutung des Mnemonics von SETF ist die Abkürzung von SETForm.
SETF nimmt Argumentpaare entgegen, so wie es auch SETQ tut. Jedes Wertepaar setzt sich aus einem Ort und einem Wert zusammen. Der Wert wird hierbei am vorgegebenen Ort gespeichert. Der letzte Wert wird zurückgegeben. Das Ortargument kann eine Variable oder eine Zugriffsform aus der folgenden Liste sein: AREF NTH ELT REST FIRST SECOND FIRST THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH CAR CDR Cxx{x{x}}R {x=A|D} SVREF GET GETF GETHASH DOCUMENTATION FILL-POINTER SYMBOL-VALUE SYMBOL-FUNCTION SYMBOL-PLIST MACRO-FUNCTION.
Beispiel: (defparameter *test* '(a b c d)) => *TEST*
 (setf (car *test*) 'n) => N
*test* => (N B C D)")

(dokumentation-austauschen 'sublis 'function "(sublis a-liste baum &key :test :test-not :key)
SUBLIS macht Ersetzungen an einem Baum, so wie es SUBST auch vornimmt, jedoch werden viele Ersetzungen auf einmal anhand einer Assoziations-Liste erstellt. Sublis berücksichtig bei der Suche alle Blätter und Zweige des Baumes. Die Suche kann zusätzlich durch Schlüsselbegriffe und Testmöglichkeiten beeinflusst werden.
Beispiel: (sublis '((rosen . veilchen) (rot . blau)) '(rosen sind rot)) => VEILCHEN SIND BLAU.")

(dokumentation-austauschen 'subsetp 'function "(subsetp liste1 liste2 &key :test :test-not :key)
SUBSETP ist ein Prädikat, daß das Ergebnis zurückliefert, ob Liste1 eine Untermenge von Liste2 ist. Gegebenenfalls kann das Ergebnis mit Hilfe der Schlüsselwörter beeinflusst werden.
Beispiel: (subsetp '(rot grün) '(rot grün blau gelb)) => T") 

(dokumentation-austauschen 'subst 'function "(subst neu alt baum &key :test :test-not :key)
SUBST erstellt eine Kopie des Baumes, wobei ein Austausch von Alt gegen Neu erfolgt. Der Austausch kann zusätzlich durch die Schlüsselworte beeinflusst werden. Der Orginalbaum bleibt unverändert, der neue Baum kann jedoch Teile des Originalbaums erhalten, er muss also keine unabhängige Kopie sein.
SUBST verfügt über zwei Kind-Funktionen, SUBST-IF und SUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine zerstörerische Version, die den Ursprungsbaum zerstören kann, NSUBST, NSUBST-IF und NSUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (subst 'tempest 'hurricane '(shakespeare wrote (the hurricane))) =>  (SHAKESPEARE WROTE (THE TEMPEST))")

(dokumentation-austauschen 'subst-if 'function "(subst-if neu prädikat baum &key :key)
SUBST-IF erstellt eine Kopie des Baumes, wobei ein Austausch erfolgt, wenn ein Blatt oder Ast das Prädikat erfüllt. Der Austausch erfolgt gegen Neu, wobei dieser zusätzlich durch das Schlüsselwort beeinflusst werden kann. Der Orginalbaum bleibt unverändert, der neue Baum kann jedoch Teile des Originalbaums erhalten, er muss also keine unabhängige Kopie sein.
SUBST verfügt über eine Elternfunktion, SUBST und eine Bruderfunktion, SUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine zerstörerische Version, die den Ursprungsbaum zerstören kann, NSUBST, NSUBST-IF und NSUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
 (subst-if 5 #'listp tree1) =>  5")

(dokumentation-austauschen 'union 'function "(union liste1 liste2 &key :test :test-not :key)
UNION liefert die Vereinigungsmenge aus Liste1 und Liste2 zurück. Sind Schlüssel wie :test oder :test-not angegeben, werden die entsprechenden Prüfverfahren im Vorfeld herangezogen.
Die Reihenfolge der Elemente in der Ergebnisliste sind Implementationsabhängig und gelten als nicht vorhersagbar. Wird eine bestimmte Reihenfolge im Ergebnis erwartet, so muß dieses per SORT-Funktion erzeugt werden.
Beispiel: (union '(rot grün blau) '(gelb magenta blau)) => (ROT GRÜN GELB MAGENTA BLAU)
 (union '((x 5) (y 6)) '((z 2) (x 4)) :key #'first) => ((x 4) (z 2) (y 6))")
