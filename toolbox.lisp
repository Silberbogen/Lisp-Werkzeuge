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

(defmacro liste? (objekt)
  "(liste? objekt)
LISTE? ist ein Ersatz für das Prädikat LISTP.
LIST? prüft ob ein Objekt eine Liste ist, wenn ja, so wird T zurückgegeben, ansonsten NIL.
Beispiel: (liste? nil) => T"
  `(listp ,objekt))

(defmacro nichtleereliste? (objekt)
  "(nichtleereliste? objekt)
NICHTLEERELISTE? ist ein Ersatz für das Prädikat CONSP.
NICHTLEERELISTE? prüft ob ein Objekt eine nicht leere Liste ist, wenn ja, so wird T zurückgegeben, ansonsten NIL.
Beispiel: (nichtleereliste? '(a b))"
  `(consp ,objekt))

(defmacro nil! (ausdruck)
  "(nil! ausdruck)
NIL! setzt eine Variable oder einen Ausdruck auf NIL.
Beispiel: (defparameter *test* '(1 2 3 4)) => *TEST*
 (nil! (first *test*)) => NIL
 *test* => (NIL 2 3 4)"
  `(setf ,ausdruck nil))

(defmacro nil? (objekt)
  "(nil? test)
NIL ist ein Ersatz für das Prädikat NULL.
NIL? prüft ob ein Objekt NIL ist. Ist es NIL, so wird T zurückgegeben, sonst NIL.
Beispiele: (nil? nil) => T
 (nil? 'a) => NIL"
  `(null ,objekt))

(defun palindrom-p (sequenz) "(palindrom-p sequenz)
Palindromp testet, ob eine übergebene Sequenz, eine übergebene Zeichenkette oder ein übergebenes Symbol ein Palindrom darstellt.
Beispiele: (palindrom-p '(1 2 3 4 3 2 1)) => T
 (palindrom-p 'otto) => T
 (palindrom-p 'otta) => NIL
 (palindrom-p \"Otto\") => T"
       (typecase sequenz
	 (null nil)
	 (string (string= sequenz (reverse sequenz)))
	 (symbol (string= (symbol-name sequenz) (reverse (symbol-name sequenz))))
	 (list (equal sequenz (reverse sequenz)))
	 (otherwise nil)))

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

(defun über-dieses-symbol (x)
  (format t "symbol-name: ~A~%" (symbol-name x))
  (format t "symbol-package: ~A~%" (symbol-package x))
  (format t "symbol-plist: ~A~%" (symbol-plist x))
  (format t "symbol-value: ~A~%" (symbol-value x))
  (when (functionp x)
    (format t "~&symbol-function: ~A~%" (symbol-function x))))

;;; =========================
;;; Abschnitt 4 - Neue Macros
;;; =========================

(defmacro wert? (objekt &rest position)
  `(elt ,objekt ,@position))

(defmacro wert-array? (objekt &rest position)
  `(aref ,objekt ,@position))

(defmacro wert-liste? (objekt &rest position)
  `(nth ,@position ,objekt))

(defmacro wert-string? (objekt &rest position)
  `(char ,objekt ,@position))

(defmacro wert-vector? (objekt &rest position)
  `(svref ,objekt ,@position))

(defmacro wert! (objekt &rest neuwert)
  `(setf ,objekt ,@neuwert))



;;; ===========================================
;;; Abschnitt 5 - Es ist einfach nur Mathematik
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



;;; ===================================
;;; Abschnitt 6 - Allgemeine Funktionen
;;; ===================================

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
;;; Abschnitt 7 - Ein-/Ausgabe
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
;;; Abschnitt 8 - Lernalgorithmen
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




