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



(defun abundante-zahl-p (n)
  "Eine natürliche Zahl heißt abundant (lat. abundans „überladen“), wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) größer ist als die Zahl selbst. Die kleinste abundante Zahl ist 12 (1+2+3+4+6 = 16 > 12). Die ersten geraden abundanten Zahlen lauten 12, 18, 20, 24, 30, 36, 40, 42, …"
  (> (apply #'+ (sammle-divisoren n t)) n))



(defun addiere-ziffern (n &optional (summe 0))
  "Nimmt eine Zahl entgegen und gibt die Summe all ihrer Ziffern zurück."
  (cond
	((zerop n)
	 summe)
	(t
	 (addiere-ziffern (truncate (/ n 10)) (+ summe (rem n 10))))))



(defun alle-permutationen (liste)
  "Alle Permutationen einer Liste erzeugen; Beispiel: (alle-permutationen (list 'a 'b 'c 'd 'e))"
  (if (null liste) '(())
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (y) (cons x y))
			  (alle-permutationen (remove x liste :count 1)))) liste)))



(defun alphabetischer-wert (string)
  "Errechnet den alphabetischen Wert eines Strings, momentan nur für Großbuchstaben korrekt."
  (let ((länge (length string))
		(summe 0))
	(do ((i 0 (1+ i)))
		((= i länge)
		 summe)
	  (incf summe (- (char-int (aref string i)) 64)))))



(defun befreundete-zahl-p (n)
  "Zwei verschiedene natürliche Zahlen, von denen wechselseitig jeweils eine Zahl gleich der Summe der echten Teiler der anderen Zahl ist, bilden ein Paar befreundeter Zahlen.
Das kleinste befreundete Zahlenpaar wird von den Zahlen 220 und 284 gebildet. Man rechnet leicht nach, dass die beiden Zahlen der Definition genügen:
    Die Summe der echten Teiler von 220 ergibt 1 + 2 + 4 + 5 + 10 + 11 + 20 + 22 + 44 + 55 + 110 = 284 und die Summe der echten Teiler von 284 ergibt 1 + 2 + 4 + 71 + 142 = 220.
In einem befreundeten Zahlenpaar ist stets die kleinere Zahl abundant und die größere Zahl defizient."
  (let* ((bz (apply #'+ (sammle-divisoren n t)))
		(bz-summe (apply #'+ (sammle-divisoren bz t))))
	(when (= n bz-summe)
		bz)))



(defun but-nth (n liste)
  "Gibt die Liste, ohne das nte Element zurück. Die Zählung der Liste beginnt bei NULL."
  (if (zerop n)
	  (rest liste)
	  (cons (first liste)
			(but-nth (1- n) (rest liste)))))



(defun collatz-rang (n &optional (durchgang 1))
  "Gibt die Länge der Sequenz der Collatz-Folge beginnend mit n zurück."
  (cond ((= n 1)
		 (return-from collatz-rang durchgang))
		((evenp n)
		 (collatz-rang (/ n 2) (1+ durchgang)))
		(t
		 (collatz-rang (1+ (* 3 n)) (1+ durchgang)))))



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



(defun defiziente-zahl-p (n)
  "Eine natürliche Zahl heißt defizient, wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) kleiner ist als die Zahl selbst. Ist die Teilersumme dagegen gleich der Zahl, spricht man von einer vollkommenen Zahl, ist sie größer, so spricht man von einer abundanten Zahl.
Beispiele: Die Zahl 10 ist defizient, denn 1+2+5 = 8 < 10.
Ebenso sind alle Primzahlen defizient, da ihre echte Teilersumme immer Eins ist."
  (< (apply #'+ (sammle-divisoren n t)) n))



(defun dreieckszahl-rang (zahl)
  "Gibt die Dreieckszahl des gewünschten Rangs aus."
  (/ (* zahl (1+ zahl)) 2))



(defun dreieckszahlp (zahl)
  "Prüft ob eine Zahl eine Dreieckszahl ist."
  (let
	  ((wert (sqrt (1+ (* 8 zahl)))))
	(= wert (truncate wert))))



(defun durchschnitt (&rest liste)
  "(durchschnitt liste)
DURCHSCHNITT ermöglicht es, den Durchschnitt einer Reihe von Zahlen zu berechnen.
Beispiel: (durchschnitt 2 3 4) => 3"
  (if (null liste)
      nil
      (/ (reduce #'+ liste) 
		 (length liste)))) 



(defun echte-teilmenge-p (a b)
  "(echte-teilmenge-p liste1 liste2)
ECHTE-TEILMENGE-P überprüft, ob Liste1 ein wirklicher Subset von Liste2 ist. Das bedeutet, das Liste1 ausschließlich Elemente aus Liste 2 enthält, nicht aber alle Elemente der Liste 2. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiele: (echte-teilmenge-p '(rot grün) '(grün blau rot gelb)) => T
 (echte-teilmenge-p '(rot schwarz) '(grün blau gelb)) => NIL"
	   (when (and (subsetp a b) (not (subsetp b a)))
	     t))



(defun faktor (n)
  "(faktor zahl)
FAKTOR berechnet den Faktor einer Zahl.
Ein Faktor von 6 wird zum Beispiel errechnet, indem man die Werte von 1 bis 6 miteinander malnimmt, also 1 * 2 * 3 * 4 * 5 * 6. Faktoren haben die unangenehme Eigenschaft, das sie sehr schnell sehr groß werden können.
Beispiel: (faktor 20) =>  2432902008176640000"
  (if (eql n 0) 
	  1 
	  (* n (faktor (1- n)))))



(defun faktorisiere (zahl)
  "(faktorisiere n)
Gibt eine Liste der Faktoren der Zahl N zurück.
Beispiel: (faktorisiere 1000) => (2 2 2 5 5 5)"  
  (when (> zahl 1)
    (let
		((limit (1+ (isqrt zahl))))
      (do ((i 2 (1+ i)))
		  ((> i limit)
		   (list zahl))
        (when (zerop (mod zahl i))
          (return-from faktorisiere
            (cons i (faktorisiere (/ zahl i)))))))))


(defun fibonacci-folge (n &optional (a 0) (b 1))
  "Bildet die Fibonacci-Folge zur n. Zahl; Beispiel: (fibonacci-folge 20) => 6765"
  (if (zerop n)
      a
	  (fibonacci-folge (- n 1) b (+ a b))))



(defun fünfeckszahl-rang (zahl)
  "Gibt die Fünfeckszahl des gewünschten Rangs aus."
  (/ (* zahl (1- (* 3 zahl))) 2))



(defun fünfeckszahlp (zahl)
  "Prüft ob eine Zahl eine Dreieckszahl ist."
  (zerop (mod (/ (1+ (sqrt (1+ (* 24 zahl)))) 6) 1)))



(defun gleichwertige-elemente (a b)
  "(gleichwertige-elemente liste1 liste2)
GLEICHWERTIGE-ELEMENTE überprüft, ob Liste1 und Liste2 über dieselben Elemente verfügen. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiel: (gleichwertige-elemente '(rot blau grün) '(grün rot blau)) => "T
	   (when (and (subsetp a b) (subsetp b a))
	     t))



(defun liste->zahl (liste)
  "Die übergebene Liste wird als Zahl zurückgegeben."
  (reduce #'(lambda (x y) (+ (* 10 x) y)) liste))



(defun lychrel-zahl-p (zahl &optional (versuche 50))
  "Jede natürliche Zahl n, die nicht durch eine endliche Anzahl von Inversionen und Additionen zu einem Zahlen-Palindrom führt, wird als Lychrel-Zahl bezeichnet. Als Inversion versteht man hier das Bilden der spiegelverkehrten Zahl m. Führt die Addition n+m dabei zu einem Zahlenpalindrom, ist der Algorithmus beendet. Falls nicht, wird durch erneute Inversion und Addition dieser Vorgang solange ausgeführt, bis das Ergebnis ein Palindrom ist.
Beispiele
    Man nimmt die Zahl 5273. Die spiegelverkehrte Zahl dazu lautet 3725 (Inversion). Durch Addition erhält man das Zahlenpalindrom 8998.
    Bei anderen Zahlen dauert dieser Algorithmus länger:
        4753 + 3574 = 8327
        8327 + 7238 = 15565
        15565 + 56551 = 72116
        72116 + 61127 = 133243
        133243 + 342331 = 475574 (ein Palindrom)"
  (if (zerop versuche)
	  t
	  (let ((kandidat (+ zahl (liste->zahl (reverse (zahl->liste zahl))))))
		(if (palindromp kandidat)
			nil
			(lychrel-zahl-p kandidat (1- versuche))))))



(defun nth-permutation (x liste)
  "Gibt die nte Permutation einer Liste zurück. Die Zählung beginnt bei NULL."
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



(defun nur-buchstaben (text)
  "Entfernt die Nicht-Buchstaben eines Textes."
  (remove-if #'(lambda (string) (not (alpha-char-p string)))
			 text))



(defun palindromp (sequenz)
  "(palindromp sequenz)
Palindromp testet, ob eine übergebene Sequenz, eine übergebene Zeichenkette oder ein übergebenes Symbol ein Palindrom darstellt.
Beispiele: (palindromp '(1 2 3 4 3 2 1)) => T
 (palindromp 'otto) => T
 (palindromp 'otta) => NIL
 (palindromp \"Otto\") => T"
  (typecase sequenz
	(null nil)
	(number (string= (write-to-string sequenz) (reverse (write-to-string sequenz))))
	(string (string= sequenz (reverse sequenz)))
	(symbol (string= (symbol-name sequenz) (reverse (symbol-name sequenz))))
	(list (equal sequenz (reverse sequenz)))
	(otherwise nil)))



(defun pandigitalp (n)
  "Prüft, ob die Zahl n pandigital ist. Eine pandigitale Zahl (aus griechisch παν: „jedes“ und digital) ist eine dezimale ganze Zahl, die jede der zehn Ziffern von 0 bis 9 genau einmal enthält. Die erste Ziffer darf dabei nicht 0 sein."
  (typecase n
	(null nil)
	(number (let ((p (search (sort (prin1-to-string n) #'char<) "123456789")))
			  (if (and (integerp p) (zerop p)) t nil)))
	(list (equal (sort n #'<) '(1 2 3 4 5 6 7 8 9)))
	(otherwise nil)))



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



(defun sechseckzahl-rang (zahl)
  "Gibt die Sechseckzahl des gewünschten Rangs aus."
  (* zahl (1- (* 2 zahl))))



(defun tausche-ziffer (zahl original-ziffer neue-ziffer)
  "Vertauscht alle Vorkommen einer bestimmten Ziffer einer Zahl gegen eine andere aus."
  (liste->zahl
   (mapcar #'(lambda (x) (if (= x original-ziffer)
							 neue-ziffer
							 x))
	   (zahl->liste zahl))))



(defun vollkommene-zahl-p (n)
  "Eine natürliche Zahl n wird vollkommene Zahl (auch perfekte Zahl) genannt, wenn sie gleich der Summe σ*(n) aller ihrer (positiven) Teiler außer sich selbst ist. Eine äquivalente Definition lautet: eine vollkommene Zahl n ist eine Zahl, die halb so groß ist wie die Summe aller ihrer positiven Teiler (sie selbst eingeschlossen), d. h. σ(n) = 2n. Die kleinsten drei vollkommenen Zahlen sind 6, 28 und 496. Alle bekannten vollkommenen Zahlen sind gerade und von Mersenne-Primzahlen abgeleitet."
  (= n (apply #'+ (sammle-divisoren n t))))



(defun zahl->liste (zahl)
  "Die übergebene Zahl wird als Liste von Ziffern zurückgegeben."
  (map 'list #'(lambda (zeichen) (read-from-string (string zeichen)))
	   (prin1-to-string zahl)))



(defun zähle-buchstaben (text)
  "Zählt die Buchstaben eines angegebenen Texts."
	(length (nur-buchstaben text)))



(defun ziffer-summe (zahl)
  (apply #'+ (zahl->liste zahl)))



; ------------------------------------------
;          Abteilung: Primzahlen
; ------------------------------------------



(defun sieb-des-eratosthenes (maximum)
  (let ((composites (make-array (1+ maximum) :element-type 'bit
								:initial-element 0)))
    (loop for candidate from 2 to maximum
	   when (zerop (bit composites candidate))
	   collect candidate
	   and do (loop for composite from (expt candidate 2) to maximum by candidate
				 do (setf (bit composites composite) 1)))))



(defun primzahlp (x)
  "Prüft ob eine Zahl eine echte Primzahl ist.
Beispiele:
   (primzahlp 24) => NIL
   (primzahlp 29) => T
   (primzahlp 1299709) => T"  
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
	  ((primzahlp i)
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
  "Mensch -> Maschine Übersetzer"
  `(nth-primzahl ,x))



(defun trunkierbare-primzahl-p (zahl)
  "Die Primzahl bleibt eine Primzahl, selbst wenn die Ziffern von vorne oder von hinten abgetrennt werden."
  (if (< zahl 10)
	  nil
	  (let
		  ((länge (length (zahl->liste zahl))))
		(do ((i 1 (1+ i)))
			((= i länge)
			 t)
		  (unless (and (primzahlp (truncate (/ zahl (expt 10 i))))
					   (primzahlp (rem zahl (expt 10 i))))
			(return nil))))))



(defun kreisförmige-primzahl-p (zahl)
  "Die Ziffern können rotiert werden, vorne raus, hinten rein - und es ergibt sich dennoch immer eine Primzahl."
  (let
	  ((länge (length (zahl->liste zahl))))
	(if (= länge 1)
		(when (primzahlp zahl)
		  t)
		(let
			((temp-zahl zahl)
			 (temp-liste (zahl->liste zahl)))
		  (do ((i 1 (1+ i)))
			  ((= i länge)
			   t)
			(setf temp-liste (append (cdr temp-liste) (cons (car temp-liste) '())))
			(setf temp-zahl (liste->zahl temp-liste))
			(unless (primzahlp temp-zahl)
			  (return nil)))))))



(defun summe-fortlaufender-primzahlen (start maximum)
  (let
	  ((summe 0)
	   (anzahl 0))
	(unless (primzahlp start)
	  (setf start (nächste-primzahl start)))
	(do ((i start (nächste-primzahl i)))
		((> (+ summe i) maximum)
		 (list summe anzahl))
	  (incf summe i)
	  (incf anzahl))))



; -------------------------------------------
;        Abteilung: Zeit und Datum
; -------------------------------------------



(defun wochentag (tag monat jahr)
  "Gibt den Tag der Woche als Zahl zurück. Montag = 0 ... Sonntag = 6."
  (seventh (multiple-value-list
			(decode-universal-time
			 (encode-universal-time 0 0 0 tag monat jahr)))))



(defun montagp (tag monat jahr)
  (= (wochentag tag monat jahr) 0))



(defun dienstagp (tag monat jahr)
  (= (wochentag tag monat jahr) 1))



(defun mittwochp (tag monat jahr)
  (= (wochentag tag monat jahr) 2))



(defun donnerstagp (tag monat jahr)
  (= (wochentag tag monat jahr) 3))



(defun freitagp (tag monat jahr)
  (= (wochentag tag monat jahr) 4))



(defun samstagp (tag monat jahr)
  (= (wochentag tag monat jahr) 5))



(defun sonntagp (tag monat jahr)
  (= (wochentag tag monat jahr) 6))



; -------------------------------------------
;     Abteilung: Mehr als einmal benötigt
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



(defun erstelle-wortliste (stream-name)
  "Einleseformat: TextKommaTextKommaText ohne Leerzeichen"
  (let ((wortliste nil))
	(with-open-file (stream stream-name)
	  (do ((i (read stream nil)
			  (read stream nil)))
		  ((null i)
		   (sort wortliste #'string<))
		(push i wortliste)
		(read-char-no-hang stream nil)))))



(defun erstelle-zahlenliste (stream-name)
  "Einleseformat: ZahlKommaZahlKommaZahl ohne Leerzeichen"
  (let ((zahlenliste nil))
	(with-open-file (stream stream-name)
	  (do ((i (read stream nil)
			  (read stream nil)))
		  ((null i)
		   (reverse zahlenliste))
		(push i zahlenliste)
		(read-char-no-hang stream nil)))))



; -------------------------------------------
;     Abteilung: Nur einmal benötigt
; -------------------------------------------



(defun expt-ziffern (n p &optional (summe 0))
  "Berechnet den Exponent p zu jeder einzelnen Ziffer der Zahl n. Bemerkenswert:
(expt-ziffern 9474 4) => 9474"
  (if (zerop n)
	  summe
	  (expt-ziffern (truncate (/ n 10)) p (+ summe (expt (rem n 10) p)))))



(defun goldbach-aufgliedern (zahl)
  "Aufgliederung einer Zahl nach Goldbach's anderer Vermutung"
  (let
	  ((maximum (isqrt zahl)))
	(do ((i 1 (1+ i)))
		((> i maximum))
	  (let
		  ((p (- zahl (* 2 (expt i 2)))))
		(when (primzahlp p)
		  (return (list p i)))))))



(defun teil-der-liste (zahl liste)
  (dolist (i liste)
	(cond
	  ((= i zahl)
	   (return t))
	  ((> i zahl)
	   (return nil)))))



(defun trennsymbolep (c) (position c " ,.;/"))



(defun string-aufteilen (string &key (trennzeichenp #'trennsymbolep))
  (loop :for beg = (position-if-not trennzeichenp string)
	 :then (position-if-not trennzeichenp string :start (1+ end))
	 :for end = (and beg (position-if trennzeichenp string :start beg))
	 :when beg :collect (subseq string beg end)
	 :while end))
