;;;; -*- coding: utf-8 -*-
;;;; Dateiname: hilfsroutinen.lisp
;;;; Beschreibung: Routinen, die mich bei diversen Aufgaben unterstützen
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha Biermanns, <skkd.h4k1n9@yahoo.de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2011-2014 Sascha Biermanns
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


;;; #############
;;; # Variablen #
;;; #############


(defvar *collatz-hash-table* (make-hash-table))


;;; ##########
;;; # Makros #
;;; ##########


(defmacro with-gensym (syms &body body)
  "Generiert ein gensym je Element aus der Liste SYMS."
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
				 syms)
	 ,@body))


(defmacro dosequence ((var seq &optional result) &body body
					  &aux (seq-len (length seq)))
  "Iteriert über die gegebene Sequenz SEQ."
  (with-gensym (i)
	`(do ((,i 0 (1+ ,i)))
		 ((= ,i ,seq-len)
		  ,result)
	   (let ((,var (elt ,seq ,i)))
		 ,@body))))


(defmacro for ((var start stop &optional (step 1)) &body body)
  "Eine for-Schleife mit optionaler Schrittweite"
  (with-gensym (gstop)
	`(do ((,var ,start (+ ,var ,step))
		  (,gstop ,stop))
		 ((if (minusp ,step)
			  (< ,var ,gstop)
			  (> ,var ,gstop)))
	   ,@body)))


(defmacro forever (&body body)
  "Eine Endlos-Schleife"
  `(do ()
	   (nil)
	 ,@body))


(defmacro in (obj &rest choices)
  "Prüft ob OBJ in CHOICES vorkommt und gibt entsprechend T oder NIL zurück."
  (with-gensym (gobj)
	`(let ((,gobj ,obj))
	   (or ,@(mapcar #'(lambda (c) `(eql ,gobj ,c))
					 choices)))))


(defmacro let1 (var val &body body)
  "Dient zum schnellen Anlegen und Zuweisen einer einzigen Variablen."
  `(let ((,var ,val))
	 ,@body))


(defmacro mac (form)
  "Expandiert ein Macro und gibt es schön formatiert aus."
  `(pprint (macroexpand-1 ',form)))


(defmacro permutations-rang (n lst)
  "Translator zwischen Mensch und Maschine, um die Zählung bei 1 (Mensch) gegen die Zählung bei 0 (Maschine) auszutauschen"
  `(nth-permutation (1- ,n) ,lst))


(defmacro until (test &body body)
  "Eine until-Kontrollstruktur"
  `(do ()
	   (,test)
	 ,@body))


(defmacro while (test &body body)
  "Eine while-Kontrollstruktur"
  `(do ()
	   ((not ,test))
	 ,@body))


;;; ##############
;;; # Funktionen #
;;; ##############


(defun 2d-array->list (array)
  "Macht aus einem zweidimensionalem Array eine Liste"
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))


(defun abundante-zahl-p (n)
  "Eine natürliche Zahl heißt abundant (lat. abundans „überladen“), wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) größer ist als die Zahl selbst. Die kleinste abundante Zahl ist 12 (1+2+3+4+6 = 16 > 12). Die ersten geraden abundanten Zahlen lauten 12, 18, 20, 24, 30, 36, 40, 42, …"
  (> (apply #'+ (divisoren n t)) n))


(defun achteckszahl (n)
  "Gibt die Achteckszahl des gewünschten Rangs aus."
 (* (- (* 3 n) 2) n))


(defun addiere-ziffern (n &optional (sum 0))
  "Nimmt eine Zahl entgegen und gibt die Summe all ihrer Ziffern zurück.
Beispiel: (addiere-ziffern 125) => 8"
  (if (zerop n)
	  sum
	  (addiere-ziffern (truncate (/ n 10)) (+ sum (rem n 10)))))


(defun alle-permutationen (lst)
  "Alle Permutationen einer Liste erzeugen; Beispiel: (alle-permutationen (list 'a 'b 'c 'd 'e))"
  (if (null lst) '(nil)
      (mapcan #'(lambda (x)
				  (mapcar #'(lambda (y) (cons x y))
						  (alle-permutationen (remove x lst :count 1)))) lst)))


(defun alphabetischer-wert (str)
  "Errechnet den alphabetischen Wert eines Strings, momentan nur für Großbuchstaben korrekt.
Beispiel: (alphabetischer-wert \"abc\") => 102"
  (loop for c across str summing (- (char-int c) 64)))
  

(defun arabisch->römisch (n)
  "Übersetzt eine Zahl mit arabischen Ziffern in einen String mit römische Ziffern um.
Beispiel: (arabisch->römisch 1968) => \"MCMLXVIII\""
  (format nil "~@R" n))


(defun befreundete-zahl-p (n)
  "Zwei verschiedene natürliche Zahlen, von denen wechselseitig jeweils eine Zahl gleich der Summe der echten Teiler der anderen Zahl ist, bilden ein Paar befreundeter Zahlen.
Das kleinste befreundete Zahlenpaar wird von den Zahlen 220 und 284 gebildet. Man rechnet leicht nach, dass die beiden Zahlen der Definition genügen:
    Die Summe der echten Teiler von 220 ergibt 1 + 2 + 4 + 5 + 10 + 11 + 20 + 22 + 44 + 55 + 110 = 284 und die Summe der echten Teiler von 284 ergibt 1 + 2 + 4 + 71 + 142 = 220.
In einem befreundeten Zahlenpaar ist stets die kleinere Zahl abundant und die größere Zahl defizient."
  (let* ((bz (apply #'+ (divisoren n t)))
		 (bz-sum (apply #'+ (divisoren bz t))))
	(when (= n bz-sum)
	  bz)))


(defun but-nth (n lst)
  "Gibt die Liste, ohne das nte Element zurück. Die Zählung der Liste beginnt bei NULL.
Beispiel: (but-nth 4 '(1 2 3 4 5 6 7 8 9)) => (1 2 3 4 6 7 8 9)"
  (if (zerop n)
	  (rest lst)
	  (cons (first lst)
			(but-nth (1- n) (rest lst)))))


(defun collatz-rang (n &optional (durchgang 1))
  "Gibt die Länge der Sequenz der Collatz-Folge beginnend mit n zurück."
  (cond ((= n 1)
		 (return-from collatz-rang durchgang))
		((evenp n)
		 (collatz-rang (/ n 2) (1+ durchgang)))
		(t
		 (collatz-rang (1+ (* 3 n)) (1+ durchgang)))))


(defun collatz-sequenz (n &optional (lst nil))
  "Gibt die Collatz-Sequenz einer gegebenen Zahl n als Liste zurück.
Beispiel: (collatz-sequenz 19) => (19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)"
  (labels ((zurück (lst1 &optional lst2)
			 (maplist #'(lambda (x)
						  (setf (gethash (first x) *collatz-hash-table*) (append x lst2)))
					  lst1)
			 (append lst1 lst2)))
	(let ((n-lst (gethash n *collatz-hash-table* 'nil)))
	  (if n-lst
		  (progn
			(let ((nr (nreverse lst)))
			  (zurück nr n-lst)))
		  (progn
			(push n lst)
			(cond ((= n 1)
				   (zurück (reverse lst)))
				  ((evenp n)
				   (setf n (/ n 2))
				   (collatz-sequenz n lst))
				  (t
				   (setf n (1+ (* 3 n)))
				   (collatz-sequenz n lst))))))))


(defun defiziente-zahl-p (n)
  "Eine natürliche Zahl heißt defizient, wenn ihre echte Teilersumme (die Summe aller Teiler ohne die Zahl selbst) kleiner ist als die Zahl selbst. Ist die Teilersumme dagegen gleich der Zahl, spricht man von einer vollkommenen Zahl, ist sie größer, so spricht man von einer abundanten Zahl.
Beispiele: Die Zahl 10 ist defizient, denn 1+2+5 = 8 < 10.
Ebenso sind alle Primzahlen defizient, da ihre echte Teilersumme immer Eins ist."
  (< (apply #'+ (divisoren n t)) n))


(defun divisoren (n &optional (ohne-selbst nil)
						 &aux (lows nil) (highs nil) (limit (isqrt n)))
"(divisoren 28) => (1 2 4 7 14 28)
 (divisoren 8128) => (1 2 4 8 16 32 64 127 254 508 1016 2032 4064 8128)
 (divisoren 2000 t) => (1 2 4 5 8 10 16 20 25 40 50 80 100 125 200 250 400 500 1000)"
(let ((feld (make-array (1+ limit) :element-type 'bit :initial-element 1)))
  (loop for i from 1 to limit
	   do(unless (zerop (elt feld i))
		   (multiple-value-bind (quotient remainder)
			   (floor n i)
			 (if (zerop remainder)
				 (progn
				   (unless (= i quotient)
					 (push i lows)
					 (push quotient highs)))
				 (loop for j from i to limit by i do
					  (setf (elt feld j) 0))))))
	   (when (= n (expt limit 2))
		 (push limit highs))
	   (if ohne-selbst
		   (butlast (nreconc lows highs))
		   (nreconc lows highs))))


(defun dreieckszahl (n)
  "Gibt die Dreieckszahl des gewünschten Rangs aus."
  (/ (* n (1+ n)) 2))


(defun dreieckszahlp (n)
  "Prüft ob eine Zahl eine Dreieckszahl ist."
  (let ((wert (sqrt (1+ (* 8 n)))))
	(= wert (truncate wert))))


(defun dreisatz (menge a b &optional (modus 'p))
  "Ein einfacher Dreisatz-Löser.
Beispiel proportional:
Ein Auto fährt mit 12 Litern 162 km weit. Wie weit fährt es mit 20 Litern?
   (dreisatz 162 12 20 'p) => 270
Beispiel umgekehrt proportional:
8 Pferde fressen in 5 Tagen den gesamten Hafervorat. Wie lange würde dieselbe Menge bei 10 Pferden reichen?
   (dreisatz 5 8 10 'u) => 4"
  (case modus
	((p proportional)
	 (* (/ menge a) b))
	((u unproportional umgekehrt-proportional)
	 (/ (* menge a) b))
	(otherwise
	 (error "~&Sie haben statt 'p oder 'u den Wert ~A als vierten Parameter angegeben.~%" modus))))


(defun durchschnitt (&rest lst)
  "(durchschnitt lst)
DURCHSCHNITT ermöglicht es, den Durchschnitt einer Reihe von Zahlen zu berechnen.
Beispiel: (durchschnitt 2 3 4) => 3"
  (if (null lst)
      nil
      (/ (reduce #'+ lst) 
		 (length lst)))) 


(defun echte-teilmenge-p (a b)
  "(echte-teilmenge-p liste1 liste2)
ECHTE-TEILMENGE-P überprüft, ob Liste1 ein wirklicher Subset von Liste2 ist. Das bedeutet, das Liste1 ausschließlich Elemente aus Liste 2 enthält, nicht aber alle Elemente der Liste 2. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiele: (echte-teilmenge-p '(rot grün) '(grün blau rot gelb)) => T
 (echte-teilmenge-p '(rot schwarz) '(grün blau gelb)) => NIL"
	   (when (and (subsetp a b) (not (subsetp b a)))
	     t))


(defun eingabe (&optional ctrl &rest args)
  "Erzwingt eine Eingabe."
  (do ((danach nil t)
	   (ctrl (concatenate 'string "~&" ctrl " > ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe deine Antwort ein und drücke dann die Eingabe-Taste.~%"))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (unless (string-equal antw "")
		(return antw)))))


(defun faktor (n)
  "(faktor zahl)
FAKTOR berechnet den Faktor einer Zahl.
Ein Faktor von 6 wird zum Beispiel errechnet, indem man die Werte von 1 bis 6 miteinander malnimmt, also 1 * 2 * 3 * 4 * 5 * 6. Faktoren haben die unangenehme Eigenschaft, das sie sehr schnell sehr groß werden können.
Beispiel: (faktor 20) =>  2432902008176640000"
  (reduce #'* (loop for i from 1 to n collect i)))


(defun fibonacci-folge (max)
  "Erstellt eine Liste aller Fibonacci-Zahlen von der ersten bis zur MAXten."
  (do ((i 1 (1+ i))
	   (a 1 a-next)
	   (a-next 1 (+ a a-next))
	   lst)
	  ((> i max)
	   (nreverse lst))
	(push a lst)))


(defun fibonaccizahl (n &optional (a 0) (b 1))
 "Bildet die Fibonaccizahl zur n. Zahl; Beispiel: (fibonaccizahl 20) => 6765"
  (if (zerop n)
      a
	  (fibonaccizahl (1- n) b (+ a b))))


(defun fünfeckszahl (n)
  "Gibt die Fünfeckszahl des gewünschten Rangs aus."
  (/ (* n (1- (* 3 n))) 2))


(defun fünfeckszahl-folge (max &optional lst (len (length lst)))
  "Erstellt eine Liste aller Fünfecks-Zahlen von der ersten bis zur MAXten."
  (when (zerop len)
	(setf len 1))
  (do* ((i len (1+ i)))
	   ((> i max)
		(nreverse lst))
	(push (fünfeckszahl i) lst)))


(defun fünfeckszahlp (n)
  "Prüft ob eine Zahl eine Fünfeckszahl ist."
  (let ((lst (do* ((i 10 (+ i 10))
				   (lst (fünfeckszahl-folge i) (fünfeckszahl-folge i lst)))
				  ((>= (first (last lst)) n)
				   lst))))
	(when (member n lst)
	  't)))


(defun gleichwertige-elemente (a b)
  "(gleichwertige-elemente liste1 liste2)
GLEICHWERTIGE-ELEMENTE überprüft, ob Liste1 und Liste2 über dieselben Elemente verfügen. Die Reihenfolge der Elemente spielt hierbei keinerlei Rolle.
Beispiel: (gleichwertige-elemente '(rot blau grün) '(grün rot blau)) => "T
	   (when (and (subsetp a b) (subsetp b a))
	     t))


(defun j-oder-n-p (&optional ctrl &rest args)
  "Erzwingt die Beantwortung einer Eingabe mit j oder n."
  (do ((danach nil t)
	   (ctrl (concatenate 'string ctrl " (j oder n) ")))
	  (nil)
	(when danach
	  (format *query-io* "~&Bitte tippe \"j\" für Ja oder  \"n\" für Nein.~%"))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
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
	(force-output *query-io*)
	(let ((antw (string-trim " " (read-line *query-io*))))
	  (cond ((string-equal antw "ja")
			 (return-from ja-oder-nein-p 't))
			((string-equal antw "nein")
			 (return-from ja-oder-nein-p 'nil))))))


(defun kreisförmige-primzahl-p (n)
  "Die Ziffern können rotiert werden, vorne raus, hinten rein - und es ergibt sich dennoch immer eine Primzahl."
  (let ((len (length (zahl->liste n))))
	(if (= len 1)
		(when (primzahlp n)
		  t)
		(let ((temp-n n)
			  (temp-lst (zahl->liste n)))
		  (do ((i 1 (1+ i)))
			  ((= i len)
			   t)
			(setf temp-lst (append (cdr temp-lst) (cons (car temp-lst) '())))
			(setf temp-n (liste->zahl temp-lst))
			(unless (primzahlp temp-n)
			  (return nil)))))))


(defun liste->zahl (lst)
  "Die übergebene Liste wird als Zahl zurückgegeben."
  (reduce #'(lambda (x y) (+ (* 10 x) y)) lst))


(defun lychrel-zahl-p (n &optional (versuche 50))
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
	  (let ((kandidat (+ n (liste->zahl (reverse (zahl->liste n))))))
		(if (palindromp kandidat)
			nil
			(lychrel-zahl-p kandidat (1- versuche))))))


(defun mischen (lst &optional (durchgang (* 2 (length lst))) &aux (len (length lst)))
  "(mischen liste &optional durchgang)
MISCHEN dient dazu, eine Liste mit einer frei wählbaren Anzahl an Durchgängen zu mischen. Wird keine Anzahl an Durchgängen genannt, so wird der Vorgang 20 Mal durchgeführt.
Beispiel: (mischen '(1 2 3 4 5)) => (5 2 1 4 3)"
  (let ((zn (random len))) ; Zufallszahl
	(cond ((zerop durchgang)
		   lst)
		  ((oddp zn)
		   (mischen (append (reverse (nthcdr zn lst))
							(butlast lst (- len zn)))
					(1- durchgang)))
		  ((evenp zn)
		   (mischen (append (nthcdr zn lst)
							(butlast lst (- len zn)))
					(1- durchgang)))
		  (t
		   (mischen (append (nthcdr zn lst)
							(reverse (butlast lst (- len zn)))
							(1- durchgang)))))))


(defun münzwurf ()
  "Münzwurf bildet den Wurf einer Münze nach. Es ist möglich, daß die Münze auf der Kante stehen bleibt! Beispiel: (münzwurf) => ZAHL"
       (let ((wurf (random 101)))
	 (cond ((< wurf 50) 'kopf)
	       ((> wurf 50) 'zahl)
	       (t 'kante))))


(defun nächste-primzahl (&optional (n 0))
  "Ein Primzahlen-Generator, der die nächste Primzahl nach der angegebenen Zahl berechnet.
Beispiele:
   (nächste-primzahl 19) => 23
   (nächste-primzahl 20) => 23
   (nächste-primzahl 23) => 29"
  (cond ((< n 2)
		 2)
		(t
		 (do ((i (+ n (if (evenp n) 1 2)) (+ i 2)))
			 ((primzahlp i)
			  i)))))


(defun nth-permutation (n lst)
  "Gibt die nte Permutation einer Liste zurück. Die Zählung beginnt bei NULL."
  (if (zerop n)
	  lst
	  (let* ((len (length lst))
			 (sublen (1- len))
			 (modulus (faktor sublen)))
		(if (> n (* len modulus))
			(format t "Die Liste mit der Länge ~A ermöglicht keine ~A Permutationen." len n)
			(multiple-value-bind (quotient remainder)
				(floor n modulus)
			  (cons (nth quotient lst)
					(nth-permutation remainder (but-nth quotient lst))))))))


(defun nur-buchstaben (text)
  "Entfernt die Nicht-Buchstaben eines Textes."
  (remove-if #'(lambda (x) (not (alpha-char-p x)))
			 text))


(defun nur-ziffern (text)
  "Entfernt die Nicht-Ziffern eines Textes."
  (remove-if #'(lambda (x) (not (digit-char-p x)))
			 text))


(defun palindromp (seq)
  "(palindromp sequenz)
Palindromp testet, ob eine übergebene Sequenz, eine übergebene Zeichenkette oder ein übergebenes Symbol ein Palindrom darstellt.
Beispiele: (palindromp '(1 2 3 4 3 2 1)) => T
 (palindromp 'otto) => T
 (palindromp 'otta) => NIL
 (palindromp \"Otto\") => T"
  (typecase seq
	(null nil)
	(number (string= (write-to-string seq) (reverse (write-to-string seq))))
	(string (string= seq (reverse seq)))
	(symbol (string= (symbol-name seq) (reverse (symbol-name seq))))
	(list (equal seq (reverse seq)))
	(otherwise nil)))


(defun pandigitalp (n)
  "Prüft, ob die Zahl n pandigital ist. Eine pandigitale Zahl (aus griechisch παν: „jedes“ und digital) ist eine dezimale ganze Zahl, die jede der zehn Ziffern von 0 bis 9 genau einmal enthält. Die erste Ziffer darf dabei nicht 0 sein."
  (typecase n
	(null nil)
	(number (let ((p (search (sort (prin1-to-string n) #'char<) "123456789")))
			  (if (and (integerp p) (zerop p)) t nil)))
	(list (equal (sort n #'<) '(1 2 3 4 5 6 7 8 9)))
	(otherwise nil)))


(defun phi-tabelle (n &aux (n+1 (1+ n)))
  "Erstellt eine Tabelle der phi-Werte bis n"
  (let ((phi (make-array n+1 :initial-element 1)))
    (do ((k 2 (1+ k)))
        ((>= k n+1))
      (if (= 1 (aref phi k))
          (let ((m (/ (1- k) k)))
            (do ((i k (+ k i)))
                ((>= i n+1))
              (setf (aref phi i) (* (aref phi i) m))))))
    (dotimes (i n+1)
      (setf (aref phi i) (* i (aref phi i))))
    phi))


(defun primfaktoren (n)
  "(primfaktoren n)
Gibt eine Liste der Primfaktoren der Zahl N zurück.
Beispiel: (primfaktoren 1000) => (2 2 2 5 5 5)"  
  (when (> n 1)
	(do ((i 2 (1+ i))
		 (limit (1+ (isqrt n))))
		((> i limit)
		 (list n))
	  (when (zerop (mod n i))
		(return-from primfaktoren
		  (cons i (primfaktoren (/ n i))))))))


(defun primzahlp (n)
  "Prüft ob eine Zahl eine echte Primzahl ist.
Beispiele:
   (primzahlp 24) => NIL
   (primzahlp 29) => T
   (primzahlp 1299709) => T"  
  (when (and (integerp n) (> n 1))
	(let ((max-d (isqrt n)))
	  (do ((d 2 (incf d (if (evenp d)
							1
							2))))
		  ((cond ((> d max-d)
				  (return t))
				 ((zerop (rem n d))
				  (return nil))))))))


(defun primzahl-rang (n)
  "Erzeugte die Primzahl eines bestimmten Rangs.
Beispiele:
   (primzahl-rang 1) => 2
   (primzahl-rang 1000) => 7919
   (primzahl-rang 100000) => 1299709"
  (labels ((nth-primzahl (x &optional (rang 1) (last-x 0))
			 (cond ((< x 1)
					nil)
				   ((= x rang)
					(nächste-primzahl last-x))
				   (t
					(nth-primzahl x (1+ rang) (nächste-primzahl last-x))))))
	(nth-primzahl n)))


(defun prozent (x n)
  "Berechnet x Prozent von n."
  (/ (* x n) 100))


(defun quadratzahl (n)
  "Berechne die Quadratzahl von n."
  (expt n 2))


(defun quadratzahlp (n)
  "Ist die übergebene Zahl N ein perfektes Quadrat?"
  (= n (expt (isqrt n) 2)))


(defun römisch->arabisch (str)
  "Übersetzt eine String, der eine Zahl als römische Ziffern enthält und wand diese in einer Zahl mit arabischen Ziffern um."
  (let ((römische-ziffern "IVXLCDM")
		(arabische-werte (list 1 5 10 50 100 500 1000)))
	(flet ((übersetze (str)
			 (loop as c across str
				as i = (position c römische-ziffern)
				collect (and i (nth i arabische-werte)))))
  (loop with zahlen = (übersetze str)
        as (a b) on zahlen if a sum (if (and b (< a b)) (- a) a)))))


(defun sechseckszahl (n)
  "Gibt die Sechseckszahl des gewünschten Rangs aus."
  (* n (1- (* 2 n))))


(defun sieb-des-eratosthenes (max)
  (let ((composites (make-array (1+ max) :element-type 'bit
								:initial-element 0)))
    (loop for candidate from 2 to max
	   when (zerop (bit composites candidate))
	   collect candidate
	   and do (loop for composite from (expt candidate 2) to max by candidate
				 do (setf (bit composites composite) 1)))))


(defun siebeneckszahl (n)
  "Gibt die Siebeneckszahl des gewünschten Rangs aus."
  (/ (* n(- (* 5 n) 3)) 2))


(defun sortiere-ziffern (n)
  "Nimmt eine Zahl entgegen und gibt sie als Liste zurück, die Ziffern aufsteigend sortiert."
  (sort (zahl->liste n) #'<))


(defun string-aufteilen (str &key (trennzeichenp #'(lambda (x)
													 (position x " ,.;?!/\\"))))
  "Wandelt einen String in eine Liste von Worten um."
  (loop :for beg = (position-if-not trennzeichenp str)
	 :then (position-if-not trennzeichenp str :start (1+ end))
	 :for end = (and beg (position-if trennzeichenp str :start beg))
	 :when beg :collect (subseq str beg end)
	 :while end))


(defun summe-der-farey-folge (n)
  "Bildet die Summe der Farey-Folge 1 bis n"
  (let* ((n+1 (1+ n))
		 (phi (phi-tabelle n)))
    (do ((i 1 (1+ i))
         (sum 1))
        ((>= i n+1) sum)
      (incf sum (aref phi i)))))


(defun summe-fortlaufender-primzahlen (start max)
  (unless (primzahlp start)
	(setf start (nächste-primzahl start)))
  (do ((i start (nächste-primzahl i))
	   (sum 0)
	   (anz 0))
	  ((> (+ sum i) max)
	   (list sum anz))
	(incf sum i)
	(incf anz)))


(defun tausche-ziffer (n old-dig new-dig)
  "Vertauscht alle Vorkommen einer bestimmten Ziffer einer Zahl gegen eine andere aus."
  (liste->zahl
   (mapcar #'(lambda (x) (if (= x old-dig)
							 new-dig
							 x))
	   (zahl->liste n))))


(defun temperatur (n &optional (smbl 'celsius))
  "TEMPERATUR wandelt den angegebenen Temperaturwert in eine Liste der Werte aller drei Maßsysteme um."
  (let ((kelvin (case smbl
				  ((celsius c) (+ n 273.15))
				  ((fahrenheit f) (* (+ n 459.67) 5/9))
				  ((kelvin k) n))))
    (when kelvin
      (values kelvin
			  (- kelvin 273.15)
			  (- (* kelvin 1.8) 459.67)))))


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
		  (format *query-io* "~&Bitte wählen sie, was sie benötigen, aus der Liste aus. Geben sie \"nichts\" ein, wenn sie nichts möchten oder \"alles\" wenn sie gerne alles hätten.~%")
		  (format *query-io* "~&Bitte wählen sie, was sie benötigen, aus der Liste aus. Geben sie \"nichts\" ein, wenn sie nichts möchten.~%")))
	(apply #'format *query-io* ctrl args)
	(force-output *query-io*)
	(let* ((antw (string-trim " " (read-line *query-io*)))
		   (antw-lst (string-aufteilen antw))
		   antwort)
	  (dolist (i antw-lst)
		(push (read-from-string i) antwort))
	  (unless (null antwort)
		(cond ((not mehrfach)
			   (if (null (rest antwort))
				   (return-from text-auswahl (first antwort))
				   (format *query-io* "~&Sie dürfen nur eines auswählen!~%")))
			  ((subsetp antwort lst)
			   (return-from text-auswahl antwort))
			  ((eql (first antwort) 'alles)
			   (return-from text-auswahl lst))
			  ((eql (first antwort) 'nichts)
			   (return-from text-auswahl 'nil))
			  (t (format *query-io* "~&Etwas aus ihrer Eingabe ist mir unbekannt!~%")))))))


(defun trunkierbare-primzahl-p (n)
  "Die Primzahl bleibt eine Primzahl, selbst wenn die Ziffern von vorne oder von hinten abgetrennt werden."
  (if (< n 10)
	  nil
	  (do ((i 1 (1+ i))
		   (len (length (zahl->liste n))))
		  ((= i len)
		   t)
		(unless (and (primzahlp (truncate (/ n (expt 10 i))))
					 (primzahlp (rem n (expt 10 i))))
		  (return nil)))))


(defun umwandeln (n von nach)
  "UMWANDELN dient dazu, eine Zahl von einer Maßeinheit in eine andere umzurechnen.
Beispiel: (umwandeln 10 'cm 'mm) => 100 MM"
  (flet ((faktor-festlegen (n)
		   (case n
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
			 ((kb kg km kilobyte kilogramm kilometer myriameter tausend) 1000) ; 10e3
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
			 (otherwise nil))))
	(if (eql von nach)
		(values n nach)
		(let ((faktor1 (faktor-festlegen von))
			  (faktor2 (faktor-festlegen nach)))
		  (/ (* n faktor1) faktor2)))))


(defun vollkommene-zahl-p (n)
  "Eine natürliche Zahl n wird vollkommene Zahl (auch perfekte Zahl) genannt, wenn sie gleich der Summe σ*(n) aller ihrer (positiven) Teiler außer sich selbst ist. Eine äquivalente Definition lautet: eine vollkommene Zahl n ist eine Zahl, die halb so groß ist wie die Summe aller ihrer positiven Teiler (sie selbst eingeschlossen), d. h. σ(n) = 2n. Die kleinsten drei vollkommenen Zahlen sind 6, 28 und 496. Alle bekannten vollkommenen Zahlen sind gerade und von Mersenne-Primzahlen abgeleitet."
  (= n (apply #'+ (divisoren n t))))


(defun wochentag (tag monat jahr)
  "Gibt den Tag der Woche zurück, als Zahl und als Symbol"
  (let ((tag (seventh (multiple-value-list
					   (decode-universal-time
						(encode-universal-time 0 0 0 tag monat jahr)))))
		(name '(montag dienstag mittwoch donnerstag freitag samstag sonntag)))
	(values tag (elt name tag))))


(defun würfelwurf (&optional (n 6))
  "(würfelwurf &optional seiten)
WÜRFELWURF bildet den Wurf mit einem in Spieleboxen üblichen, voreingestellt 6-seitigen, Würfel nach. Durch einen Aufruf mit einer anderen Seitenzahl wird ein entsprechender über Seiten verfügender Würfel angenommen.
Beispiel: (würfelwurf) => 4"
  (1+ (random n)))


(defun zahl->liste (n)
  "Die übergebene Zahl wird als Liste von Ziffern zurückgegeben."
  (map 'list #'(lambda (x) (read-from-string (string x)))
	   (prin1-to-string n)))


(defun zähle-buchstaben (text)
  "Zählt die Buchstaben eines angegebenen Texts."
	(length (nur-buchstaben text)))


(defun ziffer-summe (n)
  (apply #'+ (zahl->liste n)))
