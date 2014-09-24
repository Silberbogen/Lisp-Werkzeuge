;-*- coding: utf-8 -*-
;;;; Dateiname: hilfsroutinen.lisp
;;;; Beschreibung: Routinen, die mich bei diversen Aufgaben unterstützen
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
;;;; (load "hilfsroutinen.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "hilfsroutinen.lisp")



(defun addiere-ziffern (n &optional (summe 0))
  "Nimmt eine Zahl entgegen und gibt die Summe all ihrer Ziffern zurück."
  (cond
	((zerop n)
	 summe)
	(t
	 (addiere-ziffern (truncate (/ n 10)) (+ summe (rem n 10))))))



(defun collatz-sequenz (n &optional (liste nil))
  "Gibt die Collatz-Sequenz einer gegebenen Zahl n als Liste zurück."
  (push n liste)
  (cond
	((= n 1)
	 (reverse liste))
	((evenp n)
	 (setf n (/ n 2))
	 (collatz-sequenz n liste))
	(t
	 (setf n (1+ (* 3 n)))
	 (collatz-sequenz n liste))))



(defun faktor (n)
  "(faktor zahl)
FAKTOR berechnet den Faktor einer Zahl.
Ein Faktor von 6 wird zum Beispiel errechnet, indem man die Werte von 1 bis 6 miteinander malnimmt, also 1 * 2 * 3 * 4 * 5 * 6. Faktoren haben die unangenehme Eigenschaft, das sie sehr schnell sehr groß werden können.
Beispiel: (faktor 20) =>  2432902008176640000"
  (if (eql n 0) 
	  1 
	  (* n (faktor (1- n)))))



(defun faktorisiere (n)
  "(faktorisiere n)
Gibt eine Liste der Faktoren der Zahl N zurück.
Beispiel: (faktorisiere 1000) => (2 2 2 5 5 5)"  
  (when (and (integerp n) (> n 0))
	(let
		((max-d (isqrt n)))
	  (do ((d 2 (incf d (if (evenp d)
							1
							2))))
		  ((cond ((> d max-d)
				  (return (list n)))
				 ((zerop (rem n d))
				  (return (cons d (faktorisiere (truncate n d)))))))))))



(defun fibonacci-folge (n &optional (a 0) (b 1))
  "Bildet die Fibonacci-Folge zur n. Zahl; Beispiel: (fibonacci-folge 20) => 6765"
  (if (zerop n)
      a
	  (fibonacci-folge (- n 1) b (+ a b))))



(defun palindrom-p (sequenz)
  "(palindrom-p sequenz)
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



; ------------------------------------------



(defun primzahl-p (x)
  "Prüft ob eine Zahl eine echte Primzahl ist.
Beispiele:
   (primzahl-p 24) => NIL
   (primzahl-p 29) => T
   (primzahl-p 1299709) => T"  
  (when (and (integerp x) (> x 1))
	(let
		((max-d (isqrt x)))
	  (do ((d 2 (incf d (if (evenp d)
							1
							2))))
		  ((cond ((> d max-d)
				  (return t))
				 ((zerop (rem x d))
				  (return nil))))))))



(defun nächste-primzahl (&optional (zahl 0))
  "Ein Primzahlen-Generator, der die nächste Primzahl nach der angegebenen Zahl berechnet.
Beispiele:
   (nächste-primzahl 19) => 23
   (nächste-primzahl 20) => 23
   (nächste-primzahl 23) => 29"
  (do ((i (1+ zahl) (1+ i)))
	  ((primzahl-p i)
	   i)))



(defun nth-primzahl (x &optional (rang 1) (letzte-primzahl 0))
  "Erzeugte die Primzahl eines bestimmten Rangs.
Beispiele:
   (nth-primzahl 1) => 2
   (nth-primzahl 1000) => 7919
   (nth-primzahl 100000) => 1299709"
  (cond ((< x 1)
		 nil)
		((= x rang)
		 (nächste-primzahl letzte-primzahl))
		(t
		 (nth-primzahl x (1+ rang) (nächste-primzahl letzte-primzahl)))))



(defmacro primzahl-rang (x)
  `(nth-primzahl ,x))


  
; -------------------------------------------



(defun route-dreieck (liste)
  "Findet den Weg vom Boden zur Spitze einer Zahlenpyramide anhand des teuersten Weges."
  (if (null (rest liste))
	  (first liste)
	  (let ((bottom-row (pop liste))
			(new-row nil))
		(do ((i 0 (1+ i)))
			((= i (1- (length bottom-row))) new-row)
		  (push (reduce #'max bottom-row
						:start i :end (+ i 2))
				new-row))
		(push (mapcar #'+ (pop liste) (reverse new-row))
			  liste)
		(route-dreieck liste))))



; -------------------------------------------



(defun sammle-divisoren (n &optional (ohne-selbst nil))
  "Erstellt eine Liste aller Divisoren einer Zahl, wahlweise mit oder ohne sich selbst in die Liste einzubeziehen, vorgegeben, ist sich selbst mit einzubeziehen.
Beispiele:
   (sammle-divisoren 28) => (7 4 14 2 28 1)
   (sammle-divisoren 8128) => (127 64 254 32 508 16 1016 8 2032 4 4064 2 8128 1)
   (sammle-divisoren 2000 t) => (1 2 1000 4 500 5 400 8 250 10 200 16 125 20 100 25 80 40 50)"
  (let ((liste nil))
	(do ((i 1 (1+ i)))
		((> i (sqrt n))
		 (if ohne-selbst
			 (set-difference liste (list n))
			 liste))
	  (when (zerop (mod n i))
		(push i liste)
		(unless (= i (/ n i))
		  (push (/ n i) liste))))))



(defun abundante-zahl-p (n)
  "Eine natürliche Zahl heißt abundant (lat. abundans „überladen“), wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) größer ist als die Zahl selbst. Die kleinste abundante Zahl ist 12 (1+2+3+4+6 = 16 > 12). Die ersten geraden abundanten Zahlen lauten 12, 18, 20, 24, 30, 36, 40, 42, …"
  (> (apply #'+ (sammle-divisoren n t)) n))



(defun befreundete-zahl-p (n)
  "Zwei verschiedene natürliche Zahlen, von denen wechselseitig jeweils eine Zahl gleich der Summe der echten Teiler der anderen Zahl ist, bilden ein Paar befreundeter Zahlen.
Das kleinste befreundete Zahlenpaar wird von den Zahlen 220 und 284 gebildet. Man rechnet leicht nach, dass die beiden Zahlen der Definition genügen:
    Die Summe der echten Teiler von 220 ergibt 1 + 2 + 4 + 5 + 10 + 11 + 20 + 22 + 44 + 55 + 110 = 284 und die Summe der echten Teiler von 284 ergibt 1 + 2 + 4 + 71 + 142 = 220.
In einem befreundeten Zahlenpaar ist stets die kleinere Zahl abundant und die größere Zahl defizient."
  (let* ((bz (apply #'+ (sammle-divisoren n t)))
		(bz-summe (apply #'+ (sammle-divisoren bz t))))
	(when (= n bz-summe)
		bz)))



(defun defiziente-zahl-p (n)
  "Eine natürliche Zahl heißt defizient, wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) kleiner ist als die Zahl selbst. Ist die Teilersumme dagegen gleich der Zahl, spricht man von einer vollkommenen Zahl, ist sie größer, so spricht man von einer abundanten Zahl.
Beispiele: Die Zahl 10 ist defizient, denn 1+2+5 = 8 < 10.
Ebenso sind alle Primzahlen defizient, da ihre echte Teilersumme immer Eins ist."
  (< (apply #'+ (sammle-divisoren n t)) n))



(defun vollkommene-zahl-p (n)
  "Eine natürliche Zahl n wird vollkommene Zahl (auch perfekte Zahl) genannt, wenn sie gleich der Summe σ*(n) aller ihrer (positiven) Teiler außer sich selbst ist. Eine äquivalente Definition lautet: eine vollkommene Zahl n ist eine Zahl, die halb so groß ist wie die Summe aller ihrer positiven Teiler (sie selbst eingeschlossen), d. h. σ(n) = 2n. Die kleinsten drei vollkommenen Zahlen sind 6, 28 und 496. Alle bekannten vollkommenen Zahlen sind gerade und von Mersenne-Primzahlen abgeleitet."
  (= n (apply #'+ (sammle-divisoren n t))))
	


; --------------------------------



(defun nur-buchstaben (text)
  "Entfernt die Nicht-Buchstaben eines Textes."
  (remove-if #'(lambda (string) (not (alpha-char-p string)))
			 text))



(defun zähle-buchstaben (text)
  "Zählt die Buchstaben eines angegebenen Texts."
	(length (nur-buchstaben text)))



;---------------------------------



(defun wochentag (tag monat jahr)
  "Gibt den Tag der Woche als Zahl zurück. Montag = 0 ... Sonntag = 6."
  (seventh (multiple-value-list
			(decode-universal-time
			 (encode-universal-time 0 0 0 tag monat jahr)))))

(defun montag-p (tag monat jahr)
  (= (wochentag tag monat jahr) 0))

(defun dienstag-p (tag monat jahr)
  (= (wochentag tag monat jahr) 1))

(defun mittwoch-p (tag monat jahr)
  (= (wochentag tag monat jahr) 2))

(defun donnerstag-p (tag monat jahr)
  (= (wochentag tag monat jahr) 3))

(defun freitag-p (tag monat jahr)
  (= (wochentag tag monat jahr) 4))

(defun samstag-p (tag monat jahr)
  (= (wochentag tag monat jahr) 5))

(defun sonntag-p (tag monat jahr)
  (= (wochentag tag monat jahr) 6))



; ---------------------------------



(defun alle-permutationen (liste)
  "Alle Permutationen einer Liste erzeugen; Beispiel: (alle-permutationen (list 'a 'b 'c 'd 'e))"
  (if (null liste) '(())
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (alle-permutationen (remove x liste :count 1)))) liste)))



(defun but-nth (n liste)
  (if (zerop n)
	  (rest liste)
	  (cons (first liste)
			(but-nth (1- n) (rest liste)))))



(defun nth-permutation (x liste)
  (if (zerop x)
	  liste
	  (let* ((länge (length liste))
			 (sublen (1- länge))
			 (modulus (faktor sublen)))
		(if (> x (* länge modulus))
			(format t "Die Liste mit der Länge ~A ermöglicht keine ~A Permutationen." länge x)
			(multiple-value-bind (quotient remainder)
				(floor x modulus)
			  (cons (nth quotient liste)
					(nth-permutation remainder (but-nth quotient liste))))))))



(defmacro permutations-rang (x liste)
  "Translator zwischen Mensch und Maschine, um die Zählung bei 1 (Mensch) gegen die Zählung bei 0 (Maschine) auszutauschen"
  `(nth-permutation (1- ,x) ,liste))



; ------------------------------------



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



; --------------------------------------



(defun durchschnitt (&rest liste)
  "(durchschnitt liste)
DURCHSCHNITT ermöglicht es, den Durchschnitt einer Reihe von Zahlen zu berechnen.
Beispiel: (durchschnitt 2 3 4) => 3"
  (if (null liste)
      nil
      (/ (reduce #'+ liste) 
		 (length liste)))) 



; ---------------------------------------



(defun erstelle-namensliste (stream-name)
  "Liest eine Datei der Form STRINGKOMMASTRINGKOMMASTRING ein und erstellt aus den gewonnenen Daten eine Liste aller Strings, während die Kommatas entfallen."
  (let ((namensliste nil))
	(with-open-file (stream stream-name)
	  (do ((i (read stream nil)
			  (read stream nil)))
		  ((null i)
		   (sort namensliste #'string<))
		(push i namensliste)
		(read-char-no-hang stream nil)))))



(defun alphabetischer-wert (string)
  "Errechnet den alphabetischen Wert eines Strings, momentan nur für Großbuchstaben korrekt."
  (let ((länge (length string))
		(summe 0))
	(do ((i 0 (1+ i)))
		((= i länge)
		 summe)
	  (incf summe (- (char-int (aref string i)) 64)))))


; ----------------------------------------



(defun reziproker-zyklus-länge (n)
  (let ((i 1))
	(cond
	  ((zerop (rem n 2))
	   (reziproker-zyklus-länge (truncate (/ n 2.0))))
	  ((zerop (rem n 5))
	   (reziproker-zyklus-länge (truncate (/ n 5.0))))
	  (t (do ()
			 ((zerop (rem (1- (expt 10 i)) n))
			  i)
		   (incf i))))))



; ----------------------------------------



(defun expt-ziffern (n p &optional (summe 0))
  "Berechnet den Exponent p zu jeder einzelnen Ziffer der Zahl n. Bemerkenswert:
(expt-ziffern 9474 4) => 9474"
  (if (zerop n)
	  summe
	  (expt-ziffern (truncate (/ n 10)) p (+ summe (expt (rem n 10) p)))))



; ----------------------------------------



(defun zahl->ziffern (zahl)
  "Die übergebene Zahl wird als Liste von Ziffern zurückgegeben."
  (map 'list #'(lambda (zeichen) (read-from-string (string zeichen)))
	   (prin1-to-string zahl)))		



(defun pandigital-p (liste)
  (equal (sort liste #'<) '(1 2 3 4 5 6 7 8 9)))



(defun alle-pandigitalen-produkte ()
  (let ((liste nil)
		(produkt 0))
	(do ((i 1 (1+ i)))
		((> i 99)
		 liste)
	  (do ((j 100 (1+ j)))
		  ((> j 9999))
		(setf produkt (* i j))
		(if (and (< produkt 9999)
				 (pandigital-p (append (zahl->ziffern i)
									   (zahl->ziffern j)
									   (zahl->ziffern produkt))))
			(pushnew produkt liste))))))



; ----------------------------------------



(defun liste->zahl (liste &optional (zahl 0))
  (if (null liste)
	  zahl
	  (liste->zahl (cdr liste) (+ (* zahl 10) (car liste)))))



(defun zirkuläre-primzahl-p (zahl)
  (let
	  ((länge (length (zahl->ziffern zahl))))
	(if (= länge 1)
		(when (primzahl-p zahl)
		  t)
		(let
			((temp-zahl zahl)
			 (temp-liste (zahl->ziffern zahl)))
		  (do ((i 1 (1+ i)))
			  ((= i länge)
			   t)
			(setf temp-liste (append (cdr temp-liste) (cons (car temp-liste) '())))
			(setf temp-zahl (liste->zahl temp-liste))
			(unless (primzahl-p temp-zahl)
			  (return nil)))))))



; --------------------------------------------



(defun doppel-basis-palindrom (zahl)
  (and (palindrom-p zahl) (palindrom-p (format nil "~B" zahl))))



; -------------------------------------------



(defun abtrennbare-primzahl-p (zahl)
  (if (< zahl 10)
	  nil
	  (let
		  ((länge (length (zahl->ziffern zahl))))
		(do ((i 1 (1+ i)))
			((= i länge)
			 t)
		  (unless (and (primzahl-p (truncate (/ zahl (expt 10 i))))
					   (primzahl-p (rem zahl (expt 10 i))))
			(return nil))))))
	
	  
		



