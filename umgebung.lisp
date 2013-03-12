;-*- coding: utf-8 -*-
;;;; Dateiname: umgebung.lisp
;;;; Beschreibung: Meine tägliche Arbeitsumgebung in Lisp
;;;; ------------------------------------------------------------------------
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
;;;; ------------------------------------------------------------------------
;;;; Hinweis:
;;;; Das Laden dieser Datei in Common Lisp erfolgt per:
;;;; (load "umgebung.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "umgebung.lisp")

;;; ======
;;; Makros
;;; ======

(defmacro nil! (ausdruck)
  "(nil! ausdruck)
NIL! setzt eine Variable oder einen Ausdruck auf NIL.
Beispiel: (defparameter *test* '(1 2 3 4)) => *TEST*
 (nil! (first *test*)) => NIL
 *test* => (NIL 2 3 4)"
  `(setf ,ausdruck nil))


;;; =========
;;; Prädikate
;;; =========
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
      ((j ja) t)
      ((n ne nei nein) nil)
      (otherwise (j-oder-n-p zeichenkette)))))

(defun palindrom-p (sequenz) "(palindrom-p sequenz)
Palindromp testet, ob eine übergebene Sequenz, eine übergebene Zeichenkette oder ein übergebenes Symbol ein Palindrom darstellt.
Beispiele: (palindrom-p '(1 2 3 4 3 2 1)) => T
 (palindrom-p 'otto) => T
 (palindrom-p 'otta) => NIL
 (palindrom-p \"Otto\") => T"
       (typecase sequenz
	 (null nil)
	 (number (string= (write-to-string sequenz) (reverse (write-to-string sequenz))))
	 (string (string= sequenz (reverse sequenz)))
	 (symbol (string= (symbol-name sequenz) (reverse (symbol-name sequenz))))
	 (list (equal sequenz (reverse sequenz)))
	 (otherwise nil)))

(defun primzahl-p (x)
  "Prüft ob eine Zahl eine echte Primzahl ist."
  (labels ((versuche (&optional (n 2))
			 (if (> (expt n 2) x)
				 t
				 (if (zerop (rem x n))
					 nil
					 (versuche (1+ n))))))
    (and (/= 1 x)
		 (versuche))))

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

;;; ==========
;;; Funktionen
;;; ==========

(defun about (x)
  (format t "symbol-name: ~A~%" (symbol-name x))
  (format t "symbol-package: ~A~%" (symbol-package x))
  (format t "symbol-plist: ~A~%" (symbol-plist x))
  (format t "symbol-value: ~A~%" (symbol-value x))
  (when (functionp x)
    (format t "~&symbol-function: ~A~%" (symbol-function x))))

(defun alle-permutationen (liste)
  "Alle Permutationen einer Liste erzeugen; Beispiel: (alle-permutationen (list 'a 'b 'c 'd 'e))"
  (if (null liste) '(())
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (alle-permutationen (remove x liste :count 1)))) liste)))

(defun anhängen (item liste)
  "(anhängen item liste)
ANHÄNGEN ist ein umgekehrtes cons, das Item wird an das Ende der Liste gesetzt, nicht an den Anfang.
Beispiel: (hinten-einfügen 'Alpha '(Beta Gamma)) => (BETA GAMMA ALPHA)
Hinweis: (SNOC item liste) ist eine weitaus schnellere Fassung dieser Funktion." 
  (rückwärts (cons item (rückwärts liste))))
  
(defun durchschnitt (&rest liste)
  "(durchschnitt liste)
DURCHSCHNITT ermöglicht es, den Durchschnitt einer Reihe von Zahlen zu berechnen.
Beispiel: (durchschnitt 2 3 4) => 3"
  (if (null liste) ; Ist die Liste leer ...
      nil ; ... muss man nichts tun ...
      (/ ; ... ansonsten: Teile die Gesamtsumme der Elemente durch ihre Anzahl
	   (reduce #'+ liste) ; Wendet die Funktion + auf die Liste an und errechnet so die Gesamtsumme
	   (length liste)))) ; Wendet die Funktion Länge auf die Liste an und errechnet so die Anzahl Elemente

(defun entferne-letztes (liste)
  "(entferne-letztes liste)
ENTFERNE-LETZES entfernt das letzte Top-Level-Element einer Liste.
Beispiel: (entferne-letztes '(a b c d)) => (A B C)"
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

(defun erzeuge-primzahl (x)
  "Erzeugt die x. Primzahl."
  (labels ((durchgang (&optional (test 2) (zähler 0))
			 (if (=  zähler x)
				 (1- test)
				 (if (primzahl-p test)
					 (durchgang (1+ test) (1+ zähler))
					 (durchgang (1+ test) zähler)))))
    (durchgang)))
    
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
    ((km2 mb megabyte megameter ) 1000000) ; 10e6
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

(defun fibonacci-folge (n &optional (a 0) (b 1))
	   "Bildet die Fibonacci-Folge zur n. Zahl; Beispiel: (fibonacci-reihe 20) => 6765"
  (if (= n 0)
      a
	  (fibonacci-folge (- n 1) b (+ a b))))

(defun hole-zahl (string)
  "(hole-zahl string)
HOLE-ZAHL gibt die Zeichenkette String aus und erzwingt die Eingabe einer Zahl."
  (format t "~A" string)
  (let ((zahl (read)))
    (if (not (numberp zahl))
	(hole-zahl string)
	zahl)))

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

(defun sieb-des-eratosthenes (maximum)
  (let ((composites (make-array (1+ maximum) :element-type 'bit
                                             :initial-element 0)))
    (loop for candidate from 2 to maximum
          when (zerop (bit composites candidate))
            collect candidate
            and do (loop for composite from (expt candidate 2) to maximum by candidate
                         do (setf (bit composites composite) 1)))))

(defun sicheres-lesen-aus-string (string &rest read-from-string-args)
  "(sicheres-lesen-aus-string string &rest read-from-string-args)
SICHERES-LESEN-AUS-STRING ermöglicht die Nutzung der Funktion read-from-string, wobei sie fehlertoleranter ist."
  (let ((*read-eval* nil))
    (ignore-errors
      (apply 'read-from-string string read-from-string-args))))

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

(defun würfelwurf (&optional (seiten 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (1+ (random seiten)))    

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

;;; =========================
;;; Hilfen
;;; Macro von David Touretzky
;;; =========================
(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion:")
	    (pprint exp))
	   (t (format t "~&First step of expansion:")
	      (pprint exp1)
	      (format t "~%~%Final expansion:")
	      (pprint exp)))
     (format t "~%~%")
     (values)))
     
