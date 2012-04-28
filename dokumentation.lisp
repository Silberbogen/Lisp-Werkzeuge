;-*-lisp-*-
;-*- coding: utf-8 -*-

;;;; Dateiname: dokumentation.lisp
;;;; Beschreibung: Deutsche Dokumentationszeichenketten für den Umgang mit Lisp
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
;;;; (load "dokumentation")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "dokumentation")

;;;; Klärende Fragen wurden von den Nutzern des Kanals #lisp auf irc.freenode.net beantwortet.
;;;; Mein Dank geht hier besonders an:
;;;; - pjb

;; ===================
;; 0 Notwendige Macros
;; ===================

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


;;; ==================
;;; 1 Zahlen / Numbers
;;; ==================

;;; ---------------------
;;; 1.1 Zahlen, Prädikate
;;; ---------------------

(dokumentation-austauschen '= 'function "(= zahl &rest zahlen)
T wenn alle Zahlen gleichwertig sind in ihrem Wert.
Beispiele: (= 5) => T
 (= 5 5.0 10/2) => T
 (= 5 7) => NIL
 (= 5.0000 5.0001) => NIL")

(dokumentation-austauschen '/= 'function "(/= zahl &rest zahlen)
T wenn alle Zahlen ungleich sind in ihrem Wert.
Beispiele: (/= 5) => T
 (/= 5 7) => T
 (/= 5 5 7) => NIL")

(dokumentation-austauschen '> 'function "(> zahl &rest zahlen)
T wenn alle Zahlen monoton absteigend sind.
Beispiele: (> 5) => T
 (> 11 9 5 3) => T
 (> 11 9 5 5 3) => NIL")

(dokumentation-austauschen '>= 'function "(>= zahl &rest zahlen)
T wenn alle Zahlen nicht monoton ansteigend sind.
Beispiele: (>= 5) => T
 (>= 11 9 5 3) => T
 (>= 11 9 5 5 3) => T
 (>= 11 9 5 3 5) => NIL")

(dokumentation-austauschen '< 'function "(< zahl &rest zahlen)
T wenn alle Zahlen monoton ansteigend sind.
Beispiele: (< 3) => T
 (< 3 5 9 17) => T
 (< 3 5 5 9 17) => NIL")

(dokumentation-austauschen '<= 'function "(<= zahl &rest zahlen)
T wenn alle Zahlen nicht monoton absteigend sind.
Beispiele: (<= 3) => T
 (<= 3 5 9 17) => T
 (<= 3 5 5 9 17) => T
 (<= 5 3 5 9 17) => NIL")

(dokumentation-austauschen 'minusp 'function "(minusp zahl)
T wenn zahl < 0.
Beispiele: (minusp -2) => T
 (minusp -1.05) => T
 (minusp -8/3) => T
 (minusp 0.0) => NIL
 (minusp 3.79) => NIL")

(dokumentation-austauschen 'zerop 'function "(zerop zahl)
T wenn zahl = 0.
Beispiele: (zerop 0) => T
 (zerop 0.0) => T
 (zerop 0/100000) => T
 (zerop -0) => T
 (zerop -0.05) => NIL
 (zerop 3) => NIL")

(dokumentation-austauschen 'plusp 'function "(plusp zahl)
T wenn zahl > 0.
Beispiele: (plusp 1) => T
 (plusp 0.000083) => T
 (plusp 14/765) => T
 (plusp -7.5) => NIL
 (plusp 0.0) => NIL
 (plusp +0) => NIL")

(dokumentation-austauschen 'evenp 'function "(evenp ganzzahl)
T wenn eine Integerzahl und von der Parität her eine gerade Zahl, bei einer Integerdivision durch 2 verbleibt somit kein Rest.
Beispiele: (evenp 2) => T
 (evenp 0) => T
 (evenp -8) => T
 (evenp 1) => NIL
 (evenp -3) => NIL
 (evenp 9.2) => FEHLER!
 (evenp 10/100) => FEHLER!")

(dokumentation-austauschen 'oddp 'function "(oddp ganzzahl)
T wenn eine Integerzahl und von der Parität her eine ungerade Zahl, bei einer Integerdivision durch 2 verbleibt ein Rest von 1.
Beispiele: (oddp 1) => T
 (oddp 0) => NIL
 (oddp -3) => T
 (oddp 2) => NIL
 (oddp -9.2) => FEHLER!
 (oddp 23/57) => FEHLER!")

(dokumentation-austauschen 'numberp 'function "(numberp zahl)
T wenn zahl eine Realzahl, Bruchzahl, Fliesspunkt, Ganzzahl oder Komplexzahl ist.
Beispiele: (numberp 12) => T
 (numberp 1.25) => T
 (numberp -8/57) => T
 (numberp 0) => T
 (numberp (expt 2 130)) => T
 (numberp #c(5/3 7.2)) => T
 (numberp #c(-57/18 6.5)) => T
 (numberp #2r101) => T
 (numberp nil) => NIL
 (numberp (cons 1 2)) => NIL")

(dokumentation-austauschen 'realp 'function "(realp zahl)
T wenn zahl eine Realzahl ist.
Beispiele:  (realp 27) => T
 (realp 5.4) => T
 (realp -29/20) => T
 (realp 0) => T
 (realp #2r101) => T
 (realp #c(5/4 8.3)) => NIL
 (realp nil) => NIL
 (realp (cons 1 2)) => NIL")

(dokumentation-austauschen 'rationalp 'function "(rationalp zahl)
T wenn zahl eine Bruchzahl oder Ganzzahl ist.
Beispiele: (rationalp -12/27) => T
 (rationalp 3) => T
 (rationalp 0) => T
 (rationalp #2r101) => T
 (rationalp 5.3) => NIL
 (rationalp #c(5/4 3)) => NIL")

(dokumentation-austauschen 'floatp 'function "(floatp zahl)
T wenn zahl eine Fliesspunktzahl ist.
Beispiele: (floatp 27.3) => T
 (floatp -19.0) => T
 (floatp 0.0) => T
 (floatp 19) => NIL
 (floatp 13/10) => NIL
 (floatp #c(5.2 8.3)) => NIL")

(dokumentation-austauschen 'integerp 'function "(integerp zahl)
T wenn zahl eine Ganzzahl (Integerzahl) ist. Eine Zahl evaluiert eventuell zu einer Ganzzahl, bevor integerp auch nur von der Zahl erfahren hat (Evaluierungsregeln).
Beispiele: (integerp 5) => T
 (integerp -2) => T
 (integerp 0) => T
 (integerp 10/2) => T
 (integerp #2r101) => T
 (integerp 1.0) => NIL
 (integerp #c(1 1)) => NIL")

(dokumentation-austauschen 'complexp 'function "(complexp zahl)
T wenn zahl eine Komplexzahl ist.
Beispiele: (complexp #c(1 1)) => T
 (complexp #c(5.2 8.3)) => T
 (complexp #c(0 0.0)) => T
 (complexp 0) => NIL
 (complexp 1.4) => NIL")

(dokumentation-austauschen 'random-state-p 'function "(random-state-p objekt)
T wenn objekt eine Random-State-Struktur ist.
Beispiele:  (random-state-p *random-state*) => T
 (random-state-p (make-random-state)) => T
 (random-state-p 'wasauchimmer) => NIL
 (random-state-p 5) => NIL")

;;; ---------------------------------
;;; 1.2 Zahlen, numerische Funktionen
;;; ---------------------------------

(dokumentation-austauschen '+ 'function "(+ &rest zahlen)
+ liefert die Summe der übergebenen Zahlen zurück.
Beispiele:  (+) => 0
 (+ 3) => 3
 (+ -1) => -1
 (+ 1 2 3 4) => 10
 (+ 11/100 89/100) =>  1
 (+ 2/5 0.6) =>  1.0")

(dokumentation-austauschen '* 'function "(* &rest zahlen)
* liefert das Produkt der übergebenen Zahlen zurück.
Beispiele:  (*) => 1
 (* 4 7) => 28
 (* 1/2 1/4) => 1/8
 (* -7 23/6 5.4) => -144.90001
 (* 1.0 #c(11 9) 23/7) => #C(36.142857 29.57143)")

(dokumentation-austauschen '- 'function "(- zahl &rest zahlen)
- liefert die Differenz der übergebenen Zahlen zurück.
Beispiele: (- 4) => -4
 (- 39 4 17.2) => 17.8
 (- 25 16/3) => 59/3
 (- 25 16/3 0.0) => 19.666666
 (-) => FEHLER!")

(dokumentation-austauschen '/ 'function "(/ zahl &rest zahlen)
/ liefert den Quotient der übergebenen Zahlen zurück. Wird nur eine Zahl übergeben, so wird von dieser der Kehrwert gebildet.
Beispiele: (/ 5) => 1/5 ; Kehrwert
 (/ 5 2) => 5/2
 (/ 5 2 3.1) => 0.8064516
 (/ 5/2 2/5) => 25/4
 (/ 5/2 5/2) => 1")

(dokumentation-austauschen '1+ 'function "(1+ zahl)
1+ liefert die Summe von zahl + 1 zurück.
Beispiele: (1+ 5) => 6
 (1+ (complex 0.0)) => #C(1.0 0.0) 
 (1+ 7/3) =>  10/3 
 (1+ 0.94732) => 1.94732
 (1+ pi) => 4.141592653589793d0
 (1+) => FEHLER!")

(dokumentation-austauschen '1- 'function "(1- zahl)
1- liefert die Differenz von zahl - 1 zurück.
Beispiele: (1- 5) => 4
 (1- (complex 0.0)) => #C(-1.0 0.0) 
 (1- 8/3) =>  5/3 
 (1- 0.94732) => -0.052680016
 (1- pi) => 2.141592653589793d0
 (1-) => FEHLER!")

(dokumentation-austauschen 'incf 'function "(incf ort &optional delta-form)
INCF (Mnemonic für INCrease Form) erhöht den Wert von ort um delta-form.
Der neue Wert wird zurückgeliefert.
Seiteneffekt: ort wird verändert.
INCF entspricht (setf ort (+ ort delta-form))
Beispiele: (setq n-ort 0)
 (incf n-ort) => 1      
 n-ort => 1
 (decf n-ort 5) => -4   
 n-ort => -4
 (decf n-ort -5) =>  1      
 (decf n-ort) =>  0      
 (incf n-ort 0.5) =>  0.5
 (decf n-ort) =>  -0.5
 n-ort => -0.5")

(dokumentation-austauschen 'decf 'function "(decf ort &optional delta-form)
DECF (Mnemonic für DECrease Form) vermindert den Wert von ort um delta-form.
Der neue Wert wird zurückgeliefert.
Seiteneffekt: ort wird verändert.
DECF entspricht (setf ort (- ort delta-form))
Beispiele: (setq n-ort 0)
 (incf n-ort) => 1      
 n-ort => 1
 (decf n-ort 5) => -4   
 n-ort => -4
 (decf n-ort -5) =>  1      
 (decf n-ort) =>  0      
 (incf n-ort 0.5) =>  0.5
 (decf n-ort) =>  -0.5
 n-ort => -0.5")

(dokumentation-austauschen 'exp 'function "(exp zahl)
EXP liefert die Potenz zahl der Eulerschen Zahl (etwa 2,7182818284590452) basiert auf dem natürlichen Logarithmus zurück.
Beispiele: (exp 1) => 2.7182817
 (exp 0) => 1.0
 (exp 70) => 2.5154387e30
 (exp (log 7)) => 6.9999995
 (exp (log 5)) => 5.0")

(dokumentation-austauschen 'expt 'function "(expt basiswert potenzwert)
EXPT liefert die Potenz potenzwert der Basis basiswert zurück.
Beispiele: (expt 2 64) => 18446744073709551616
 (expt 0 1) => 0
 (expt 0 0) => 1
 (expt 2 8) => 256
 (expt 4 .7) => 2.6390157
 (expt #c(0 1) 2) => -1
 (expt #c(2 2) 3) => #C(-16 16)
 (expt #c(2 2) 4) => -64 
 (expt 25 25) => 88817841970012523233890533447265625
 (expt 99 99) => 369729637649726772657187905628805440595668764281741102430259972423552570455277523421410650010128232727940978889548326540119429996769494359451621570193644014418071060667659301384999779999159200499899")

(dokumentation-austauschen 'log 'function "(log zahl &optional basis)
LOG liefert den Logarithmus der zahl zur basis zurück. Wird basis nicht angegeben, so wird der natürliche Logarithmus anhand der Eulerschen Zahl (etwa 2,7182818284590452) verwendet.
Beispiele:  (log 100 10) =>  2.0 ; oder 2 ... implementationsabhängig
 (log 100.0 10) => 2.0
 (log #c(0 1) #c(0 -1)) =>  #C(-1.0 0.0) ; oder  #C(-1 0) ... implementationsabhängig
 (log 8.0 2) => 3.0
 (log #c(-16 16) #c(2 2)) => #C(3.0 0.0) ; oder 3 ... implementationsabhängig")

(dokumentation-austauschen 'sqrt 'function "(sqrt zahl)
SQRT liefert die Quadratwurzel der zahl zurück. Ist die zahl keine Komplexzahl aber negativ, so wird das Ergebnis als Komplexzahl zurückgeliefert.
Beispiele: (sqrt 9.0) => 3.0
 (sqrt -9.0) => #C(0.0 3.0)
 (sqrt 12) => 3.4641016
 (sqrt 25) => 5.0 ; oder 5 ... implementationsabhängig
 (sqrt -1) => #C(0.0 1.0)
 (sqrt #c(0 2)) => #C(1.0 1.0)")

(dokumentation-austauschen 'isqrt 'function "(isqrt natürlichezahl)
SQRT liefert die Quadratwurzel von natürlichezahl zurück. Das Ergebnis ist die abgerundete naheste Ganzzahl (Integerzahl) der natürlichen Zahl natürlichezahl.
Beispiele: (isqrt 9) => 3
 (isqrt 12) => 3
 (isqrt 300) => 17
 (isqrt 325) => 18
 (isqrt 25) => 5
 (isqrt 3.14) => FEHLER!
 (isqrt pi) => FEHLER!
 (isqrt -1) => FEHLER!
 (isqrt 3/7) => FEHLER!
 (isqrt #c(0 2)) => FEHLER!")

(dokumentation-austauschen 'lcm 'function "(lcm &optional ganzzahlen)
LCM liefert das kleinste gemeinsame Vielfache (engl. least common multiple) einer Zahlenliste zurück. Das Ergebnis ist immer eine positive Ganzzahl (natürliche Zahl).
Beispiele: (lcm) => 1
 (lcm 2 4 7 9) => 252
 (lcm 0 1) => 0
 (lcm -2 7) => 14
 (lcm 1 1/2) => FEHLER!
 (lcm 2 4 7 9.0) => FEHLER!")

(dokumentation-austauschen 'gcd 'function "gcd &optional ganzzahlen)
GCD liefert den größten gemeinsamen Teiler (greates common divisor) einer Zahlenliste zurück. Das Ergebnis ist immer eine positive Ganzzahl (natürliche Zahl).
Beispiele:  (gcd) => 0
 (gcd 0) => 0
 (gcd 7) => 7
 (gcd -5) => 5
 (gcd 60 42) => 6
 (gcd 1 3 57) =  1
 (gcd 3333 -33 1002001) => 11
 (gcd 91 -49) =>  7
 (gcd 2 4 8 16) => 2
 (gcd 63 -42 35) =>  7
 (gcd 1.0) => FEHLER!
 (gcd 2/3) => FEHLER!")

(dokumentation-austauschen 'pi 'variable "pi
PI syombolisiert dargestellt als long-float Konstante die Ludolphsche Zahl.
Beispiel: pi => 3.141592653589793d0 ; der genaue Wert ist implementationsabhängig
 (cos pi) => -1.0d0 ; oder -1.0L0 ... implementationsabhängig")

(dokumentation-austauschen 'sin 'function "(sin radiant)
SIN liefert den Sinus von radiant zurück.
Beispiele: (sin 0) => 0.0
 (sin 1) => 0.84147096")

(dokumentation-austauschen 'cos 'function "(cos radiant)
COS liefert den Kosinus von radiant zurück.
Beispiele: (cos 0) => 1.0
 (cos 1) => 0.5403023")

(dokumentation-austauschen 'tan 'function "(tan radiant)
TAN liefert die Tangente von radiant zurück.
Beispiele: (tan 0) => 0.0
 (tan 1) => 1.5574077")

(dokumentation-austauschen 'asin 'function "(asin radiant)
ASIN liefert den Arkussinus von radiant zurück.
Beispiele: (asin 0) => 0.0
 (asin 1) => 1.5707964")

(dokumentation-austauschen 'acos 'function "(acos radiant)
ACOS liefert den Arkuskosinus von radiant zurück.
Beispiele: (acos 0) => 1.5707964
 (acos 1) => 0.0")

(dokumentation-austauschen 'atan 'function "(atan radiant)
ATAN liefert die Arkustangente von radiant zurück.
Beispiele: (atan 0) => 0.0
(atan 1) => 0.7853982")

(dokumentation-austauschen 'sinh 'function "(sinh radiant)
SIN liefert den hyperbolischen Sinus von radiant zurück.
Beispiele: (sinh 0) => 0.0
 (sinh 1) => 1.1752012")

(dokumentation-austauschen 'cosh 'function "(cosh radiant)
COS liefert den hyperbolischen Kosinus von radiant zurück.
Beispiele: (cosh 0) => 1.0
 (cosh 1) => 1.5430807")

(dokumentation-austauschen 'tanh 'function "(tanh radiant)
TAN liefert die hyperbolische Tangente von radiant zurück.
Beispiele: (tanh 0) => 0.0
 (tanh 1) => 0.7615942")

(dokumentation-austauschen 'asinh 'function "(asinh radiant)
ASIN liefert den hyperbolischen Arkussinus von radiant zurück.
Beispiele: (asinh 0) => 0.0
 (asinh 1) => 0.8813736")

(dokumentation-austauschen 'acosh 'function "(acosh radiant)
ACOS liefert den hyperbolischen Arkuskosinus von radiant zurück.
Beispiele: (acosh 0) => #C(0.0 1.5707964)
CL-USER> (acosh 1) => 0.0")

(dokumentation-austauschen 'atanh 'function "(atanh radiant)
ATAN liefert die hyperbolische Arkustangente von radiant zurück.
Beispiele: (atanh 0) => 0.0
 (atanh 1) => FEHLER! Division durch 0!")

(dokumentation-austauschen 'cis 'function "(cis radiant)
CIS liefert den Wert von e^i* radiant zurück, eine Komplexzahl, deren reeller Teil dem Kosinus des Radianten und deren imaginärer Teil dem Sinus des radiant entspricht.
Beispiele: (cis 0) => #C(1.0 0.0)
CL-USER> (cis 1) => #C(0.5403023 0.84147096)")

(dokumentation-austauschen 'conjugate 'function "(conjugate zahl)
CONJUGATE liefert die konjugierte Komplexzahl einer Zahl zurück. Die Konjugation einer Realzahl ist die Zahl selber.
Beispiele:  (conjugate #c(0 -1)) => #C(0 1)
 (conjugate #c(1 1)) => #C(1 -1)
 (conjugate 1.0) => 1.0
 (conjugate #C(3/4 4/5)) => #C(3/4 -4/5)
 (conjugate #C(0.0D0 -1.0D0)) => #C(0.0d0 1.0d0)")

(dokumentation-austauschen 'max 'function "(max zahl &rest zahlen)
MAX liefert die größte Zahl einer Liste von Zahlen zurück.
Beispiele: (max 3 7 29 -11 3/2) => 29
 (max -14 -27.245 -9.2 -13.5) => -9.2
 (max 0) => 0")

(dokumentation-austauschen 'min 'function "(min zahl &rest zahlen)
MIN liefert die kleinste Zahl einer Liste von Zahlen zurück.
 (max -7 19 22 -13.2 39) => -13.2
 (min 2/3 1/2 29/5011) => 29/5011-13.2")

(dokumentation-austauschen 'round 'function "(round zahl &optional (divisor 1))
ROUND teilt die zahl durch den divisor und gibt das Ergebnis gerundet als quotient und rest zurück.
Die Ergebnisse sind Ganzzahlen (Integerzahlen).
zahl = quotient * divisor + rest
Beispiel: (round 5.286) => 5 0.28599977
 (round 7.649) => 8 -0.35099983")

(dokumentation-austauschen 'floor 'function "(floor zahl &optional (divisor 1))
FLOOR teilt die zahl durch den divisor und gibt das Ergebnis nach unten gerundet als quotient und rest zurück.
Die Ergebnisse sind Ganzzahlen (Integerzahlen).
zahl = quotient * divisor + rest
Beispiel: (floor 5.286) => 5 0.28599977
 (floor 7.649) => 7 0.64900017")

(dokumentation-austauschen 'ceiling 'function "(ceiling zahl &optional (divisor 1))
CEILING teilt die zahl durch den divisor und gibt das Ergebnis nach oben gerundet als quotient und rest zurück.
Die Ergebnisse sind Ganzzahlen (Integerzahlen).
zahl = quotient * divisor + rest
Beispiel: (ceiling 5.286) => 6 -0.7140002
 (ceiling 7.649) => 8 -0.35099983")

(dokumentation-austauschen 'truncate 'function "(truncate zahl &optional (divisor 1))
TRUNCATE teilt die zahl durch den divisor und gibt das Ergebnis am Komma abgeschnitten als quotient und rest zurück.
Die Ergebnisse sind Ganzzahlen (Integerzahlen).
zahl = quotient * divisor + rest
Beispiel: (truncate 5.286) => 5 0.28599977
 (truncate 7.649) => 7 0.64900017")

(dokumentation-austauschen 'fround 'function "(fround zahl &optional (divisor 1))
FROUND teilt die zahl durch den divisor und gibt das Ergebnis gerundet als quotient und rest zurück.
Die Ergebnisse sind Fliesspunktzahlen.
zahl = quotient * divisor + rest
Beispiel: (fround 5.286) => 5.0 0.28599977
 (fround 7.649) => 8.0 -0.35099983")

(dokumentation-austauschen 'ffloor 'function "(ffloor zahl &optional (divisor 1))
FFLOOR teilt die zahl durch den divisor und gibt das Ergebnis nach unten gerundet als quotient und rest zurück.
Die Ergebnisse sind Fliesspunktzahlen.
zahl = quotient * divisor + rest
Beispiel: (ffloor 5.286) => 5.0 0.28599977
 (ffloor 7.649) => 7.0 0.64900017")

(dokumentation-austauschen 'fceiling 'function "(fceiling zahl &optional (divisor 1))
FCEILING teilt die zahl durch den divisor und gibt das Ergebnis nach oben gerundet als quotient und rest zurück.
Die Ergebnisse sind Fliesspunktzahlen.
zahl = quotient * divisor + rest
Beispiel: (fceiling 5.286) => 6.0 -0.7140002
 (fceiling 7.649) => 8.0 -0.35099983")

(dokumentation-austauschen 'ftruncate 'function "(ftruncate zahl &optional (divisor 1))
FTRUNCATE teilt die zahl durch den divisor und gibt das Ergebnis am Komma abgeschnitten als quotient und rest zurück.
Die Ergebnisse sind Fliesspunktzahlen.
zahl = quotient * divisor + rest
Beispiel: (ftruncate 5.286) => 5.0 0.28599977
 (ftruncate 7.649) => 7.0 0.64900017")

(dokumentation-austauschen 'mod 'function "(mod zahl divisor)
MOD ist das Mnemonic für MODulo.
MOD teilt die zahl durch den divisor und gibt das Ergebnis des Rests zurück.
Die Berechnung selbst entspricht FLOOR.
Beispiel: (mod 5.286 1) => 0.28599977
 (mod 7.649 1) => 0.64900017
 (mod -1 5) => 4
 (mod 13 4) => 1
 (mod -13 4) =>3
 (mod 13 -4) => -3
 (mod -13 -4) => -1
 (mod 13.4 1) => 0.4
 (mod -13.4 1) => 0.6")

(dokumentation-austauschen 'rem 'function "(rem zahl divisor)
REM ist das Mnemonic für REMainder.
REM teilt die zahl durch den divisor und gibt das Ergebnis des Rests zurück.
Die Berechnung selber entspricht TRUNCATE.
Beispiel: (rem 5.286 1) => 0.28599977
 (rem  7.649 1) => 0.64900017
 (rem -1 5) => -1
 (rem 13 4) => 1
 (rem -13 4) => -1
 (rem 13 -4) => 1
 (rem -13 -4) => -1
 (rem 13.4 1) => 0.4
 (rem -13.4 1) => -0.4")

(dokumentation-austauschen 'random 'function "(random limit &optional (zustand *random-state*))
RANDOM liefert eine positive Zufallszahl zurück, die größer oder gleich 0 und kleiner als das Limit ist. Zusätzlich kann der Zufallszahlgenerator durch eine Übermittlung eines random-state beeinflusst werden.
Beispiel: (random 5) => 4
 (1+ (random 6)) => 6
 (1+ (random 6)) => 3
 (setf test *random-state*)
 (1+ (random 6 test)) => 2")

(dokumentation-austauschen 'make-random-state 'function "(make-random-state {status|nil|t})
Erstellt ein frisches random-state Objekt, das durch status, NIL oder T beeinflusst werden kann. Vorgabe ist NIL.
Beispiele: (let* ((randomstate1 (make-random-state nil))
        (randomstate2 (make-random-state t))
        (randomstate3 (make-random-state randomstate2))
        (randomstate4 nil))
   (list (loop for i from 1 to 10 
               collect (random 100)
               when (= i 5)
                do (setq randomstate4 (make-random-state)))
         (loop for i from 1 to 10 collect (random 100 randomstate1))
         (loop for i from 1 to 10 collect (random 100 randomstate2))
         (loop for i from 1 to 10 collect (random 100 randomstate3))
         (loop for i from 1 to 10 collect (random 100 randomstate4))))
((81 13 90 83 12 96 91 22 63 30) (81 13 90 83 12 96 91 22 63 30)
 (83 15 51 97 71 13 82 2 0 30) (83 15 51 97 71 13 82 2 0 30)
 (96 91 22 63 30 9 54 27 18 54))")

(dokumentation-austauschen '*random-state* 'variable "*random-state*
Der momentane Zustand des Zufallsgenerators, der z.B. für die Funktion RANDOM genutzt wird.
Beispiel: *random-state* => #S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                                     :INITIAL-CONTENTS
                                     '(0 2567483615 20 2601187879 3919438689
                                       2270374771 3254473187 705526435
                                       752899028 4259895275 1635503293
                                       287311810 3348146311 587101971
                                       1133963260 197444494 1569747226
                                       2853653046 3654449492 3823320007
                                       1939491435 191871982 2550916200))) ; sehr stark gekürzt!!!")


;;; 1.3 Zahlen, logische Funktionen
;;; 1.4 Zahlen, Ganzzahl-Funktionen / Integer-Funktionen
;;; 1.5 Zahlen, Implementationsabhängig

;;; =========
;;; 2 Zeichen
;;; =========

;;; =========================
;;; 3 Zeichenketten / Strings
;;; =========================

;;; =====================
;;; 4 Konstrukte / Conses
;;; =====================

;;; -------------------------
;;; 4.1 Konstrukte, Prädikate
;;; -------------------------

(dokumentation-austauschen 'consp 'function "(consp objekt)
CONSP gibt T zurück, wenn OBJEKT ein CONS - und somit eine nicht leere Liste ist, ansonsten NIL. Eine leere Liste / NIL ist kein CONS, daher gibt CONSP hier ebenfalls NIL zurück.
Beispiele:  (consp nil) =>  false
 (consp (cons 1 2)) =>  true
 (consp '()) ==  (consp 'nil) =>  false")

(dokumentation-austauschen 'listp 'function "(listp objekt)
LISTP gibt wahr zurück, wenn OBJEKT eine Liste ist, sonst NIL.
Beispiele:(listp nil) =>  true
 (listp (cons 1 2)) =>  true
 (listp (make-array 6)) =>  false
 (listp t) =>  false")

(dokumentation-austauschen 'endp 'function "(endp objekt)
ENDP ist der empfohlene Weg um auf das Ende einer echten Liste hin zu testen. Ist OBJEKT NIL, wird T zurückgegeben. Ist OBJEKT ein CONS wird NIL zurückgegeben.Jeder andere Wert führt zu einem Fehler.
Beispiele: (endp nil) =>  true
 (endp '(1 2)) =>  false
 (endp (cddr '(1 2))) =>  true")

(dokumentation-austauschen 'atom 'function "(atom objekt)
ATOM ist ein Prädikat, auch wenn das markante P am Ende fehlt. ATOM liefert T zurück, wenn OBJEKT ein ATOM ist, ansonsten NIL.
Beispiele: (atom 'sss) =>  true
 (atom (cons 1 2)) =>  false
 (atom nil) =>  true
 (atom '()) =>  true
 (atom 3) =>  true")

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
MEMBER-IF-NOT gilt als veraltet, stattdessen sollte MEMBER-IF in Verbindung mit COMPLEMENT verwendet werden.
Beispiel: (member-if-not  #'oddp '(1 2 3 4)) => (2 3 4)")

(dokumentation-austauschen 'subsetp 'function "(subsetp liste1 liste2 &key :test :test-not :key)
SUBSETP ist ein Prädikat, daß das Ergebnis zurückliefert, ob Liste1 eine Untermenge von Liste2 ist. Gegebenenfalls kann das Ergebnis mit Hilfe der Schlüsselwörter beeinflusst werden.
Beispiel: (subsetp '(rot grün) '(rot grün blau gelb)) => T") 

;;; ------------------------------
;;; 4.2 Konstrukte, Listen (Lists)
;;; ------------------------------

(dokumentation-austauschen 'cons 'function "(cons objekt1 objekt2)
Die Bedeutung des Mnemonics von CONS ist die Abkürzung von CONStruct.
CONS erzeugt eine neues Konstrukt, in CL auch CONS genannt, das sich zu einer Liste aus Objekt1 und Objekt2 zusammensetzt.
CONS erzeugt genau eine neue CONS-Zelle. Es wird oftmals eingesetzt, um ein neues Element an den Anfang einer Liste zu setzen.
Hinweis: Mit der selbgeschriebenen Funktion SNOC kann ein Element an das Ende einer Liste gesetzt werden.
Beispiel: (cons 'a '(b)) => (A B)
 (cons 'a 'b) => (A . B)")

(dokumentation-austauschen 'list 'function "(list & rest argumente)
LIST konstruiert eine Liste aus den der Funktion übergebenen Argumenten, wobei diese notfalls zuerst evaluiert werden - und gibt die so entstandene Liste zurück.
LIST bildet Listen aus einer Serie von CONS-Zellen und beendet die neue Liste mit NIL. Das CAR jeder Zelle deutet auf das übergebene Argument hin.
Beispiele: (list 'a 'b) => (A B)
 (list 1 2 'ö (car '(a . b)) (+ 7 -3)) => (1 2 Ö A 4)")

(dokumentation-austauschen 'car 'function "(car liste)
Die Bedeutung des Mnemonics von CAR ist \"Contents of the Adress Register\" bei einer IBM 704.
CAR gibt das erste Objekt eines CONStructs / einer Liste oder NIL zurück. Die Verwendung von CAR geht in die Ursprünge von Lisp zurück, einfacher zu lesen ist stattdessen die gleichwertige Verwendung des Befehls FIRST.
Beispiel: (car '(a b)) => A")

(dokumentation-austauschen 'cdr 'function "(cdr liste)
Die Bedeutung des Mnemonics von CDR ist \"Contents of the Decrement Register\" bei einer IBM 704.
CDR gibt eine Liste der Objekte nach dem ersten Objekt einer Liste oder NIL zurück.
Die Verwendung von REST anstelle von CDR kann einen Quelltext sehr viel lesbarer machen, beide Funktionen sind Synonyme.
Beispiel: (cdr '(a b c)) => (B C)")

(dokumentation-austauschen 'nthcdr 'function "(nthcdr n liste)
NTHCDR gibt den CDR-Wert von Liste ab der der mit N angegeben Position zurück. Wird 0 für N eingegeben, wird die vollständige Liste zurückgegeben. Ist N größer oder gleich der Position des letzten Elements, so wird NIL zurückgegeben.
Dies gilt nicht, wenn eine punktierte Liste übergeben wurde. Geht man hier über das abschließende Element hinaus, so kommt es zu einer Fehlermeldung.
Beispiele: (nthcdr 1 '(a b c)) => (B C)
 (nthcdr 3 '(a b c)) => NIL
 (nthcdr 0 '(a b c)) => (A B C)
 (nthcdr 2 '(a b . c)) => C
 (nthcdr 3 '(a b . c)) => FEHLER")

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

(dokumentation-austauschen 'nth 'function "(nth n liste)
NTH gibt den CAR-Wet des Elements ab Position N zurück, wobei mit der Zählung beim nullten Element begonnen wird.
 (nth n liste) ist gleichwertig zu (car (nthcdr n liste)).
Ist die übergebene Liste eine punktierte Liste, darf nicht über den letzten CAR-Wert hinaus abgefragt werden, ansonsten kommt es zu einem Fehler.
Bei einer nicht punktierten Liste wird der Wert NIL zurückgeliefert, wenn über den letzen CAR-Wert hinaus eine Abfrage getroffen wird.
Beispiele: (nth 0 '(a b . c)) => A
 (nth 1 '(a b . c)) => B
 (nth 2 '(a b . c)) => FEHLER
 (nth 3 '(a b c)) => NIL")

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

;;; --------------------------------------------
;;; 4.3 Konstrukte, Assoziations-Listen (Alists)
;;; --------------------------------------------

(dokumentation-austauschen 'assoc 'function "(assoc objekt a-liste &key :test :test-not :key)
ASSOC durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert dem von Objekt entspricht, oder aber NIL. 
ASSOC verfügt über zwei Kind-Funktionen: ASSOC-IF und ASSOC-IF-NOT.
Beispiel: (assoc 'b '((a hubbel) (b dubbel) (c gubbel))) => (B DUBBEL)")

(dokumentation-austauschen 'rassoc 'function "(rassoc objekt a-liste &key :test :test-not :key)
RASSOC durchsucht die A-Liste nach einer Assoziation des Objekts. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CDR-Wert dem von Objekt entspricht, oder aber NIL. 
RASSOC verfügt über zwei Kind-Funktionen: RASSOC-IF und RASSOC-IF-NOT.
RASSOC benötigt A-Listen, deren Objekte Paare aus gepunkteten Listen sind.
Beispiel: (rassoc 'b '((a hubbel) (b dubbel) (c gubbel))) => NIL
 (rassoc 'dubbel '((a . hubbel) (b . dubbel) (c . gubbel))) => (B . DUBBEL)")

(dokumentation-austauschen 'assoc-if 'function "(assoc-if prädikat a-liste &key :key)
ASSOC-IF durchsucht die A-Liste nach einer Assoziation anhand des Prädikats. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert die Prüfung von Prädikat erfolgreich besteht, oder aber NIL. 
ASSOC-IF verfügt über eine Eltern-Funktion, ASSOC und eine Bruder-Funktionen, ASSOC-IF-NOT.
Beispiel: (assoc-if #'oddp '((2 hubbel) (3 dubbel) (5 gubbel))) => (3 DUBBEL)")

(dokumentation-austauschen 'assoc-if-not 'function "(assoc-if-not prädikat a-liste &key :key)
ASSOC-IF-NOT durchsucht die A-Liste nach einer Assoziation anhand des Prädikats. Hierbei können Prüfungen durch die Schlüsselwörter Einfluss nehmen. Der zurückgegebene Wert ist das erste Paar, dessen CAR-Wert die Prüfung von Prädikat nicht besteht, oder aber NIL. 
ASSOC-IF-NOT verfügt über eine Eltern-Funktion, ASSOC und eine Bruder-Funktionen, ASSOC-IF.
ASSOC-IF-NOT gilt als veraltet. Stattdessen sollte in ASSOC-IF in Verbindung mit COMPLEMENT benutzt werden.
Beispiel: (assoc-if-not #'oddp '((2 hubbel) (3 dubbel) (5 gubbel))) => (2 HUBBEL)")

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

;;; -----------------------------
;;; 4.4 Konstrukte, Bäume (Trees)
;;; -----------------------------

(dokumentation-austauschen 'subst 'function "(subst neu alt baum &key :test :test-not :key)
SUBST erstellt eine Kopie des Baumes, wobei ein Austausch von Alt gegen Neu erfolgt. Der Austausch kann zusätzlich durch die Schlüsselworte beeinflusst werden. Der Orginalbaum bleibt unverändert, der neue Baum kann jedoch Teile des Originalbaums erhalten, er muss also keine unabhängige Kopie sein.
SUBST verfügt über zwei Kind-Funktionen, SUBST-IF und SUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine zerstörerische Version, die den Ursprungsbaum zerstören kann, NSUBST, NSUBST-IF und NSUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (subst 'tempest 'hurricane '(shakespeare wrote (the hurricane))) =>  (SHAKESPEARE WROTE (THE TEMPEST))")

(dokumentation-austauschen 'nsubst 'function "(nsubst neu alt baum &key :test :test-not :key)
NSUBST erstellt eine Kopie des Baumes, wobei ein Austausch von Alt gegen Neu erfolgt. Der Austausch kann zusätzlich durch die Schlüsselworte beeinflusst werden. Der Orginalbaum wird vermutlich zerstört, der neue Baum kann jedoch Teile des Originalbaums erhalten.
NSUBST verfügt über zwei Kind-Funktionen, NSUBST-IF und NSUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine nicht zerstörerische Version, die den Ursprungsbaum zerstören kann, SUBST, SUBST-IF und SUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (nsubst 'tempest 'hurricane '(shakespeare wrote (the hurricane))) =>  (SHAKESPEARE WROTE (THE TEMPEST))")

(dokumentation-austauschen 'subst-if 'function "(subst-if neu prädikat baum &key :key)
SUBST-IF erstellt eine Kopie des Baumes, wobei ein Austausch erfolgt, wenn ein Blatt oder Ast das Prädikat erfüllt. Der Austausch erfolgt gegen Neu, wobei dieser zusätzlich durch das Schlüsselwort beeinflusst werden kann. Der Orginalbaum bleibt unverändert, der neue Baum kann jedoch Teile des Originalbaums erhalten, er muss also keine unabhängige Kopie sein.
SUBST verfügt über eine Elternfunktion, SUBST und eine Bruderfunktion, SUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine zerstörerische Version, die den Ursprungsbaum zerstören kann, NSUBST, NSUBST-IF und NSUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
 (subst-if 5 #'listp tree1) =>  5")

(dokumentation-austauschen 'nsubst-if 'function "(nsubst-if neu prädikat baum &key :key)
NSUBST-IF erstellt eine Kopie des Baumes, wobei ein Austausch erfolgt, wenn ein Blatt oder Ast das Prädikat erfüllt. Der Austausch erfolgt gegen Neu, wobei dieser zusätzlich durch das Schlüsselwort beeinflusst werden kann. Der Orginalbaum wird vermutlich zerstört, der neue Baum kann jedoch Teile des Originalbaums erhalten.
NSUBST-IF verfügt über eine Elternfunktion, NSUBST und eine Bruderfunktion, NSUBST-IF-NOT.
Von allen drei Funktionen gibt es zusätzlich eine nicht zerstörerische Version, die den Ursprungsbaum nicht zerstören, SUBST, SUBST-IF und SUBST-IF-NOT.
Die Funktionen SUBST-IF-NOT und NSUBST-IF-NOT gelten als veraltet und überflüssig.
Beispiel: (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
 (nsubst-if 5 #'oddp tree1) =>  5")

(dokumentation-austauschen 'sublis 'function "(sublis a-liste baum &key :test :test-not :key)
SUBLIS macht Ersetzungen an einem Baum, so wie es SUBST auch vornimmt, jedoch werden viele Ersetzungen auf einmal anhand einer Assoziations-Liste erstellt. Sublis berücksichtig bei der Suche alle Blätter und Zweige des Baumes. Die Suche kann zusätzlich durch Schlüsselbegriffe und Testmöglichkeiten beeinflusst werden.
Beispiel: (sublis '((rosen . veilchen) (rot . blau)) '(rosen sind rot)) => VEILCHEN SIND BLAU.")

;;; -----------------------------
;;; 4.5 Konstrukte, Mengen (Sets)
;;; -----------------------------

(dokumentation-austauschen 'intersection 'function "(intersection list1 list2 &key :test :test-not :key)
INTERSECTION fasst alle Elemente, die in beiden Listen, list1 und list2 vorhanden sind und,  wenn angegeben :test oder :test-not, das Testkriterium bestehen, zu einer neuen Liste, also einer Schnittmenge (engl. intersection) zusammen. Die Reihenfolge der Elemente in der neuen Liste ist implementationsabhängig und gilt als nicht vorhersehbar. Wird eine bestimmte Reihenfolge benötigt, so muß das Ergebnis entsprechend noch mit Hilfer der SORT-Funktion sortiert werden.
INTERSECTION ist eine nicht-destruktive Funktion, hat aber auch eine Geschwisterfunktion, NINTERSECTION, die unter Umständen liste1 zerstören würde - und daher einen destruktiven Seiteneffekt hat.
Beispiel: (intersection '(a b c d) '(f a d s l)) => (A D)")

(dokumentation-austauschen 'set-difference 'function "(set-difference liste1 liste2 &key :test :test-not :key)
SET-DIFFERENCE ermittelt die Differenzmenge von Liste1 zu Liste2. Die zurückgegebene Liste enthält also alle Elemente aus Liste1, die nicht in Liste2 vorhanden sind. Der Ausgang kann mit Hilfe der verschiedenen Schlüssel beeinflusst werden.
SET-DIFFERENCE ist eine nicht-destruktive Funktion. Sie verfügt über eine destruktive Bruderfunktion, NSET-DIFFERENCE, die bei ihrer Anwendung vermutlich Liste1 zerstören wird.
Beispiel: (set-difference '(a b c d) '(c d e f)) => (B A)")

(dokumentation-austauschen 'union 'function "(union liste1 liste2 &key :test :test-not :key)
UNION liefert die Vereinigungsmenge aus Liste1 und Liste2 zurück. Sind Schlüssel wie :test oder :test-not angegeben, werden die entsprechenden Prüfverfahren im Vorfeld herangezogen.
Die Reihenfolge der Elemente in der Ergebnisliste sind Implementationsabhängig und gelten als nicht vorhersagbar. Wird eine bestimmte Reihenfolge im Ergebnis erwartet, so muß dieses per SORT-Funktion erzeugt werden.
Beispiel: (union '(rot grün blau) '(gelb magenta blau)) => (ROT GRÜN GELB MAGENTA BLAU)
 (union '((x 5) (y 6)) '((z 2) (x 4)) :key #'first) => ((x 4) (z 2) (y 6))")

(dokumentation-austauschen 'nintersection 'function "(nintersection list1 list2 &key :test :test-not :key)
NINTERSECTION fasst alle Elemente, die in beiden Listen, list1 und list2 vorhanden sind und,  wenn angegeben :test oder :test-not, das Testkriterium bestehen, zu einer neuen Liste, also einer Schnittmenge (engl. intersection) zusammen. Die Reihenfolge der Elemente in der neuen Liste ist implementationsabhängig und gilt als nicht vorhersehbar. Wird eine bestimmte Reihenfolge benötigt, so muß das Ergebnis entsprechend noch mit Hilfer der SORT-Funktion sortiert werden.
NINTERSECTION ist eine nicht-destruktive Funktion, hat aber auch eine Geschwisterfunktion, NINTERSECTION, die unter Umständen liste1 zerstören würde - und daher einen destruktiven Seiteneffekt hat.
Beispiel: (intersection '(a b c d) '(f a d s l)) => (A D)")

(dokumentation-austauschen 'nset-difference 'function "(nset-difference liste1 liste2 &key :test :test-not :key)
NSET-DIFFERENCE ermittelt die Differenzmenge von Liste1 zu Liste2. Die zurückgegebene Liste enthält also alle Elemente aus Liste1, die nicht in Liste2 vorhanden sind. Der Ausgang kann mit Hilfe der verschiedenen Schlüssel beeinflusst werden.
NSET-DIFFERENCE ist eine destruktive Funktion, die vermutlich Liste1 zerstören wird. Sie verfügt auch über eine nicht-destruktive Bruderfunktion, SET-DIFFERENCE.
Beispiel: (nset-difference '(a b c d) '(c d e f)) => (B A)")

;;; =================
;;; 5 Felder / Arrays
;;; =================
;;; 5.1 Felder, Prädikate
;;; 5.2 Felder, Feldfunktionen
;;; 5.3 Felder, Vektorfunktionen

;;; =====================================
;;; 6 Reihenfolgen, Sequenzen / Sequences
;;; =====================================
;;; 6.1 Reihenfolgen, Prädikate

;;; ----------------------------
;;; 6.2 Reihenfolgen, Funktionen
;;; ----------------------------

(dokumentation-austauschen 'reverse 'function "(reverse &rest reihenfolge)
REVERSE nimmt eine Sequenz, eine Reihenfolge entgegen - und kehrt sie um. Hierbei wird jedoch ausschließlich eine Umkehrung der obersten Elementebene durchgeführt.
Es wird eine Liste erzeugt, die ursprünglichen Werte werden nicht verändert, die Funktion ist also non-destruktiv.
Beispiel: (reverse '(a b c d)) => (D C B A)")

(dokumentation-austauschen 'remove 'function "(remove objekt reihenfolge &key :from-end :test :test-not :start :end :count :key)
REMOVE liefert eine Reihenfolge zurück, die weitestgehend der übergebenen Reihenfolge entspricht, jedoch wurden jene zwischen :start und :end herausgenommen, die dem :test entsprachen. Die Vorgabe für :test ist der Vergleich per EQL auf Objekt. Das :count Argument, wenn angegeben, sorgt dafür das nicht mehr Argumente als gewünscht entfernt werden. Ausschließlich wenn das :count Argument angegeben wurde. In diesem Fall arbeitet die Funktion vom Ende aus rückwerts die Liste ab.
REMOVE ist nicht destruktiv, die Ursprungsliste wird nicht verändert.
REMOVE hat zwei Abkömmlinge: REMOVE-IF und REMOVE-IF-NOT.
Beispiele: (remove 4 '(1 3 4 5 9)) =>  (1 3 5 9)
 (remove 4 '(1 2 4 1 3 4 5)) =>  (1 2 1 3 5)
 (remove 4 '(1 2 4 1 3 4 5) :count 1) =>  (1 2 1 3 4 5)
 (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t) =>  (1 2 4 1 3 5)
 (remove 3 '(1 2 4 1 3 4 5) :test #'>) =>  (4 3 4 5)")

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

(dokumentation-austauschen 'remove-duplicates 'function "(remove-duplicates reihenfolge &key :from-end :test :start :end :key)
REMOVE-DUPLICATES überprüft die Reihenfolge paarweise, und wenn zwei daraus übereinstimmend sind, wird das erste entfernt, es sei denn FROM-END ist wahr, dann wird das letztere entfernt. Andere Schlüsselworte können ebenfalls einen Einfluss auf die Überprüfung auswirken. Das Ergebnis wird zurückgeliefert.
Die Funktion ist destruktiv, die Original-Reihenfolge wird vermutlich zerstört werden.
Das Schlüsselwort :test-not gilt als veraltet.
Beispiel: (remove-duplicates '(a b c b d d e)) => (a d b c e)")

;;; =============================
;;; 7 Hash-Tabellen / Hash Tables
;;; =============================

;;; =========================
;;; 8 Strukturen / Structures
;;; =========================

;;; =========================================
;;; 9 Kontrollstrukturen / Control Structures
;;; =========================================

;;; ---------------------------------
;;; 9.1 Kontrollstrukturen, Prädikate
;;; ---------------------------------

(dokumentation-austauschen 'boundp 'function "(boundp objekt)
BOUNDP liefert T Wert zurück, wenn OBJEKT eine gebundene Variable ist, sonst NIL.
Beispiele: (defparameter *test* 1) => 1
 (boundp '*test*) =>  T
 (makunbound '*test*) =>  *TEST*
 (boundp '*test*) =>  NIL")

;;; ---------------------------------
;;; 9.2 Kontrollstrukturen, Variablen
;;; ---------------------------------

(dokumentation-austauschen 'setf 'function "(setf ort1 wert1 {ort2 wert2 {ortN wertN}})
Die Bedeutung des Mnemonics von SETF ist die Abkürzung von SETForm.
SETF nimmt Argumentpaare entgegen, so wie es auch SETQ tut. Jedes Wertepaar setzt sich aus einem Ort und einem Wert zusammen. Der Wert wird hierbei am vorgegebenen Ort gespeichert. Der letzte Wert wird zurückgegeben. Das Ortargument kann eine Variable oder eine Zugriffsform aus der folgenden Liste sein: AREF NTH ELT REST FIRST SECOND FIRST THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH TENTH CAR CDR Cxx{x{x}}R {x=A|D} SVREF GET GETF GETHASH DOCUMENTATION FILL-POINTER SYMBOL-VALUE SYMBOL-FUNCTION SYMBOL-PLIST MACRO-FUNCTION.
Beispiel: (defparameter *test* '(a b c d)) => *TEST*
 (setf (car *test*) 'n) => N
*test* => (N B C D)")

(dokumentation-austauschen 'setq 'function "(setq variable1 form1 {variable2 form2 {variableN formN}})
Die Bedeutung des Mnemonics von SETQ ist die Abkürzung von SETQuote. Die Bedeutung rührt daher, das jedes Argumentpaar mit einem Symbol beginnt, das nicht evaluiert wird.
SETQ nimmt Argumentpaare entgegen, wobei es den ersten Wert, ein Symbol, als Variable mit dem dazugehörigen ersten Wert einer zuvor evaluierten Form bindet. Somit entspricht SETQ für Programmierer, die von anderem Programmiersprachen aus kommen, dem klassischen Zuweisungsoperator.
Beispiele: ;; Der einfachste Nutzen von SETQ ist es, als Seiteneffekt den Wert von evaulierten Formen in Variablen zu speichern.
 (setq a 1 b 2 c 3) =>  3
 a => 1
 b => 2
 c => 3
 ;; Ebenso kann SETQ genutzt werden, um Werte in der Reihenfolge von links nach rechts durch Zuweisung zu aktualisieren.
 (setq a (1+ b) b (1+ a) c (+ a b)) => 7
 a => 3
 b => 4
 c => 7
 ;; Dies stellt die Verwendung von SETQ in Bezug zu SYMBOL-MACROLET dar.
 (let ((x (list 10 20 30)))
   (symbol-macrolet ((y (first x)) (z (second x)))
     (setq y (1+ z) z (1+ y))
     (list x y z))) => ((21 22 30) 21 22)")

;;; ----------------------------------
;;; 9.3 Kontrollstrukturen, Funktionen
;;; ----------------------------------

(dokumentation-austauschen 'apply 'function "(apply funktion argument &rest mehr-argumente)
APPLY wendet eine Funktion auf ein oder mehrere Argumente in der Art und Weise wie LIST* es ihr gleicht tut. Das bedeutet, das bedeutet, das aus allen Argumenten ausser dem letzten eine Liste gemacht wird, die vorangesetzt wird an das letzte Argument, das seinerseits bereits eine Liste sein muss.
Beispiel: (apply #'+ '1 '2 '(3 4)) => 10")

;;; 9.4 Kontrollstrukturen, Makros
;;; 9.5 Kontrollstrukturen, Kontrollfluss
;;; 9.6 Kontrollstrukturen, Wiederholungen
;;; 9.7 Kontrollstrukturen, Schleifen-Möglichkeiten (Loop)

;;; =======
;;; 10 CLOS
;;; =======
;;; 10.1 CLOS, Klassen
;;; 10.2 CLOS, allgemeine Funktionen
;;; 10.3 CLOS, Methoden Kombinationsarten

;;; =================================================
;;; 11 Bedingungen und Fehler / Conditions and Errors
;;; =================================================

;;; ========================================
;;; 12 Typen und Klassen / Types and Classes
;;; ========================================

;;; =========================================
;;; 13 Eingabe und Ausgabe / Input and Output
;;; =========================================
;;; 13.1 E/A, Prädikate
;;; 13.2 E/A, Lesevorrichtung / Reader
;;; 13.3 E/A, Zeichengrammatik / Character Syntax
;;; 13.4 E/A, Ausgabevorrichtung / Printer
;;; 13.5 E/A, Formatierung / Format
;;; 13.6 E/A, Ströme / Streams
;;; 13.7 E/A, Pfade und Dateien / Pahts and Files

;;; ============================================
;;; 14 Pakete und Symbole / Packages and Symbols
;;; ============================================

;;; ----------------------------------
;;; 14.1 Pakete und Symbole, Prädikate
;;; ----------------------------------

(dokumentation-austauschen 'symbolp 'function "(symbolp objekt)
SYMBOLP liefert T zurück, wenn OBJEKT ein Symbol ist, sonst NIL.
Beispiele: (symbolp 'elephant) =>  true
 (symbolp 12) =>  false
 (symbolp nil) =>  true
 (symbolp '()) =>  true
 (symbolp :test) =>  true
 (symbolp \"hello\") =>  false")

;;; 14.2 Pakete und Symbole, Pakete
;;; 14.3 Pakete und Symbole, Symbole
;;; 14.4 Pakete und Symbole, Standardpakete

;;; ===========
;;; 15 Compiler
;;; ===========

;;; ------------------------
;;; 15.1 Compiler, Prädikate
;;; ------------------------

;;; --------------------------
;;; 15.2 Compiler, Compilieren
;;; --------------------------

(dokumentation-austauschen 'quote 'function "(quote objekt)
QUOTE ist eine besondere Funktion, die durch ihren Seiteneffekt die Evaluierung eines Objekts verhindert.
QUOTE kann abgekürzt werden, in dem man vor ein Objekt ein Hochkommata ' setzt. Dieses Hochkommate ersetzt QUOTE sowie sein dazugehöriges Klammerpaar.
Beispiele: (setq a 1) => 1
 (quote (setq a 3)) => (SETQ A 3)
 a => 1
 'a => A
 ''a => (QUOTE A) oder 'A 
 '''a => (QUOTE (QUOTE A)) oder ''A
 (setq a 43) => 43
 (list a (cons a 3)) => (43 (43 . 3))
 (list (quote a) (quote (cons a 3))) => (A (CONS A 3)) 
 1 => 1
 '1 => 1
 \"foo\" => \"foo\"
 '\"foo\" => \"foo\"
 (car '(a b)) => A
 '(car '(a b)) => (CAR (QUOTE (A B))) oder (CAR '(A B))
 #(car '(a b)) => #(CAR (QUOTE (A B))) oder #(CAR '(A B))
 '#(car '(a b)) => #(CAR (QUOTE (A B))) oder #(CAR '(A B))")

;;; 15.3 Compiler, REPL & Debug
;;; 15.4 Compiler, Bekanntmachungen / Declarations

;;; ==========================================
;;; 16 Externe Umgebung / External Environment
;;; ==========================================

















































