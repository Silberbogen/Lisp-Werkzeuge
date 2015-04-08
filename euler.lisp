;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: euler.lisp
;;;; Beschreibung: Lösungen diverser Aufgaben von Projekt Euler
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skkd PUNKT h4k1n9 AT yahoo PUNKT de>
;;;; Lizenz: GPL v3
;;;; Copyright (C) 2011-2015 Sascha K. Biermanns
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the
;;;; Free Software Foundation; either version 3 of the License, or (at your
;;;; option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;;; Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, see <http://www.gnu.org/licenses/>. 
;;;; ------------------------------------------------------------------------


(load "~/Quellen/git/Lisp-Werkzeuge/hilfsroutinen.lisp")
(use-package :drakma)


;;; ##################################################################
;;; #                            INHALT                              #
;;; # 1. Aufgaben übergreifende Routinen, die sinnlos sind im Alltag #
;;; # 2. Die Lösungen zu den einzelnen Aufgaben                      #
;;; # 3. Die Testumgebung                                            #
;;; ##################################################################


;;; ##################################################################
;;; # 1. Aufgaben übergreifende Routinen, die sinnlos sind im Alltag #
;;; ##################################################################


(defun route-dreieck (lst)
  "Findet den Weg vom Boden zur Spitze einer Zahlenpyramide anhand des teuersten Weges."
  (if (null (rest lst))
	  (first lst)
	  (let ((bottom-row (pop lst))
			(new-row nil))
		(do ((i 0 (1+ i)))
			((= i (1- (length bottom-row))) new-row)
		  (push (reduce #'max bottom-row
						:start i :end (+ i 2))
				new-row))
		(push (mapcar #'+ (pop lst) (reverse new-row))
			  lst)
		(route-dreieck lst))))


(defun erstelle-wortliste (stream-name)
  "Einleseformat: TextKommaTextKommaText ohne Leerzeichen"
  (let ((wortliste nil))
                                        ;	(with-open-file (stream stream-name)
    (with-input-from-string (stream stream-name)
	  (do ((i (read stream nil)
			  (read stream nil)))
		  ((null i)
		   (sort wortliste #'string<))
		(push i wortliste)
		(read-char-no-hang stream nil)))))


(defun erstelle-zahlenliste (stream-name)
  "Einleseformat: ZahlKommaZahlKommaZahl ohne Leerzeichen"
  (let ((zahlenliste nil))
	(with-input-from-string (stream stream-name)
	  (do ((i (read stream nil)
			  (read stream nil)))
		  ((null i)
		   (reverse zahlenliste))
		(push i zahlenliste)
		(read-char-no-hang stream nil)))))


;;; #############################################
;;; # 2. Die Lösungen zu den einzelnen Aufgaben #
;;; #############################################


(defun problem-1 ()
  (loop
     for i from 1 to 999
     when (or (zerop (mod i 3)) (zerop (mod i 5)))
     sum i))


(defun problem-2 ()
  (loop
     for i upfrom 1
     for x = (fibonacci i)
     when (>= x 4000000) return summe
     when (evenp x) sum x into summe))


(defun problem-3 (&optional (limit 600851475143))
  (apply #'max (primfaktoren limit)))


(defun problem-4 ()
  (loop with max = 0
     for i from 999 downto 100
     do (loop for j from i downto 100
           for n = (* i j)
           when (and (> n max) (palindromp n))
           do (setf max n))
       finally (return max)))


(defun problem-5 ()
  (lcm 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))


(defun problem-6 ()
  (loop
     for i from 1 to 100
     sum i into summe
     sum (expt i 2) into summe-quadrate
     finally (return (- (expt summe 2) summe-quadrate))))


(defun problem-7 ()
  (primzahl 10001))


(defun problem-8 (&optional (limit 13))
  (labels ((wert (x s)
			 (digit-char-p (char s x)))
		   (berechne-produkt (index limit zahl &aux (wert 1))
             (loop for i from 0 below limit
                for m = (wert (+ index i) zahl)
                if (zerop m) do (return-from berechne-produkt 0)
                else do (setf wert (* wert m))
                finally (return wert))))
	(let* ((zahl "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")
		   (zahl-länge (length zahl)))
      (apply #'max (loop for i from 0 to (- zahl-länge limit)
                      collect (berechne-produkt i limit zahl))))))


(defun problem-9 (&optional (wert 1000))
  (flet ((finde-tripel (wert)
		   (let ((halb-wert (/ wert 2)))
             (loop for a from 1 below halb-wert
                do (loop for b from (1+ a) below halb-wert
                      for c = (- wert a b)
                      when (= (+ (expt a 2) (expt b 2)) (expt c 2))
                      do (return-from finde-tripel (* a b c)))))))
	(finde-tripel wert)))


(defun problem-10 ()
  (reduce #'+ (sieb-des-eratosthenes 1999999)))


(defun problem-11 ()
  (let ((zahlen
		  (make-array '(20 20) :initial-contents
					  '(( 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8)
						(49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0)
						(81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65)
						(52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91 ) 
						(22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 ) 
						(24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50 ) 
						(32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 ) 
						(67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21 ) 
						(24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 ) 
						(21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95 ) 
						(78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92 ) 
						(16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57 ) 
						(86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58 ) 
						(19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40 ) 
						( 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66 ) 
						(88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69 ) 
						( 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36 ) 
						(20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16 ) 
						(20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54 ) 
						( 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48 ))))
		(n 0)
		(max-n 0))
	(labels ((zelle (x y)
			   (aref zahlen x y))
			 (sequenz-horizontal (x y)
			   (* (zelle x y)
				  (zelle (+ x 1) y)
				  (zelle (+ x 2) y)
				  (zelle (+ x 3) y)))
			 (sequenz-vertikal (x y)
			   (* (zelle x y)
				  (zelle x (+ y 1))
				  (zelle x (+ y 2))
				  (zelle x (+ y 3))))
			 (sequenz-lu-ro (x y)
			   (* (zelle x (+ y 3))
				  (zelle (+ x 1) (+ y 2))
				  (zelle (+ x 2) (+ y 1))
				  (zelle (+ x 3) y)))
			 (sequenz-lo-ru (x y)
			   (* (zelle x y)
				  (zelle (+ x 1) (+ y 1))
				  (zelle (+ x 2) (+ y 2))
				  (zelle (+ x 3) (+ y 3))))
			 (teste ()
			   (when (> n max-n)
				 (setf max-n n))))
	  (do ((i 0 (1+ i)))
		  ((>= i 20)
		   max-n)
		(do ((j 0 (1+ j)))
			((>= j 20))
		  (when (<= i 16)
			(setf n (sequenz-horizontal i j))
			(teste)
		  (when (<= j 16)
			(setf n (sequenz-vertikal i j))
			(teste)
		  (when (and (<= i 16) (<= j 16))
			(setf n (sequenz-lu-ro i j))
			(teste)
			(setf n (sequenz-lo-ru i j))
			(teste)))))))))


(defun problem-12 ()
  (loop for i upfrom 1
     for n = (dreieckszahl i)
     when (> (length (teiler n)) 500)
     do (return-from problem-12 n)))


(defun problem-13 ()
  (let* ((zahlen (list 37107287533902102798797998220837590246510135740250 
					   46376937677490009712648124896970078050417018260538 
					   74324986199524741059474233309513058123726617309629 
					   91942213363574161572522430563301811072406154908250 
					   23067588207539346171171980310421047513778063246676 
					   89261670696623633820136378418383684178734361726757 
					   28112879812849979408065481931592621691275889832738 
					   44274228917432520321923589422876796487670272189318 
					   47451445736001306439091167216856844588711603153276 
					   70386486105843025439939619828917593665686757934951 
					   62176457141856560629502157223196586755079324193331 
					   64906352462741904929101432445813822663347944758178 
					   92575867718337217661963751590579239728245598838407 
					   58203565325359399008402633568948830189458628227828 
					   80181199384826282014278194139940567587151170094390 
					   35398664372827112653829987240784473053190104293586 
					   86515506006295864861532075273371959191420517255829 
					   71693888707715466499115593487603532921714970056938 
					   54370070576826684624621495650076471787294438377604 
					   53282654108756828443191190634694037855217779295145 
					   36123272525000296071075082563815656710885258350721 
					   45876576172410976447339110607218265236877223636045 
					   17423706905851860660448207621209813287860733969412 
					   81142660418086830619328460811191061556940512689692 
					   51934325451728388641918047049293215058642563049483 
					   62467221648435076201727918039944693004732956340691 
					   15732444386908125794514089057706229429197107928209 
					   55037687525678773091862540744969844508330393682126 
					   18336384825330154686196124348767681297534375946515 
					   80386287592878490201521685554828717201219257766954 
					   78182833757993103614740356856449095527097864797581 
					   16726320100436897842553539920931837441497806860984 
					   48403098129077791799088218795327364475675590848030 
					   87086987551392711854517078544161852424320693150332 
					   59959406895756536782107074926966537676326235447210 
					   69793950679652694742597709739166693763042633987085 
					   41052684708299085211399427365734116182760315001271 
					   65378607361501080857009149939512557028198746004375 
					   35829035317434717326932123578154982629742552737307 
					   94953759765105305946966067683156574377167401875275 
					   88902802571733229619176668713819931811048770190271 
					   25267680276078003013678680992525463401061632866526 
					   36270218540497705585629946580636237993140746255962 
					   24074486908231174977792365466257246923322810917141 
					   91430288197103288597806669760892938638285025333403 
					   34413065578016127815921815005561868836468420090470 
					   23053081172816430487623791969842487255036638784583 
					   11487696932154902810424020138335124462181441773470 
					   63783299490636259666498587618221225225512486764533 
					   67720186971698544312419572409913959008952310058822 
					   95548255300263520781532296796249481641953868218774 
					   76085327132285723110424803456124867697064507995236 
					   37774242535411291684276865538926205024910326572967 
					   23701913275725675285653248258265463092207058596522 
					   29798860272258331913126375147341994889534765745501 
					   18495701454879288984856827726077713721403798879715 
					   38298203783031473527721580348144513491373226651381 
					   34829543829199918180278916522431027392251122869539 
					   40957953066405232632538044100059654939159879593635 
					   29746152185502371307642255121183693803580388584903 
					   41698116222072977186158236678424689157993532961922 
					   62467957194401269043877107275048102390895523597457 
					   23189706772547915061505504953922979530901129967519 
					   86188088225875314529584099251203829009407770775672 
					   11306739708304724483816533873502340845647058077308 
					   82959174767140363198008187129011875491310547126581 
					   97623331044818386269515456334926366572897563400500 
					   42846280183517070527831839425882145521227251250327 
					   55121603546981200581762165212827652751691296897789 
					   32238195734329339946437501907836945765883352399886 
					   75506164965184775180738168837861091527357929701337 
					   62177842752192623401942399639168044983993173312731 
					   32924185707147349566916674687634660915035914677504 
					   99518671430235219628894890102423325116913619626622 
					   73267460800591547471830798392868535206946944540724 
					   76841822524674417161514036427982273348055556214818 
					   97142617910342598647204516893989422179826088076852 
					   87783646182799346313767754307809363333018982642090 
					   10848802521674670883215120185883543223812876952786 
					   71329612474782464538636993009049310363619763878039 
					   62184073572399794223406235393808339651327408011116 
					   66627891981488087797941876876144230030984490851411 
					   60661826293682836764744779239180335110989069790714 
					   85786944089552990653640447425576083659976645795096 
					   66024396409905389607120198219976047599490197230297 
					   64913982680032973156037120041377903785566085089252 
					   16730939319872750275468906903707539413042652315011 
					   94809377245048795150954100921645863754710598436791 
					   78639167021187492431995700641917969777599028300699 
					   15368713711936614952811305876380278410754449733078 
					   40789923115535562561142322423255033685442488917353 
					   44889911501440648020369068063960672322193204149535 
					   41503128880339536053299340368006977710650566631954 
					   81234880673210146739058568557934581403627822703280 
					   82616570773948327592232845941706525094512325230608 
					   22918802058777319719839450180888072429661980811197 
					   77158542502016545090413245809786882778948721859617 
					   72107838435069186155435662884062257473692284509516 
					   20849603980134001723930671666823555245252804609722 
					   53503534226472524250874054075591789781264330331690)))
    (parse-integer (subseq (princ-to-string (apply #'+ zahlen)) 0 10))))


(defun problem-14 (&optional (max (expt 10 6)))
  (loop with länge = 0
     with n = ()
     for i from 1 below max
     for c = (collatz-sequenz i)
     for l = (length c)
     when (> l länge)
     do (setf länge l
              n c)
     finally (return (first n))))



(defun problem-15 ()
  (/ (faktor 40) (faktor 20) (faktor 20)))


(defun problem-16 ()
  (quersumme (expt 2 1000)))


(defun problem-17 ()
  (loop for i from 1 to 1000
     sum (zähle-buchstaben (format nil "~R" i)) into summe
     finally (return (+ (* 3 9 99) summe))))


(defun problem-18 ()
  (let ((dreieck
		 (reverse '((75)
					(95 64)
					(17 47 82)
					(18 35 87 10)
					(20 04 82 47 65)
					(19 01 23 75 03 34)
					(88 02 77 73 07 63 67)
					(99 65 04 28 06 16 70 92)
					(41 41 26 56 83 40 80 70 33)
					(41 48 72 33 47 32 37 16 94 29)
					(53 71 44 65 25 43 91 52 97 51 14)
					(70 11 33 28 77 73 17 78 39 68 17 57)
					(91 71 52 38 17 14 91 43 58 50 27 29 48)
					(63 66 04 68 89 53 67 30 73 16 69 87 40 31)
					(04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))))
	(first (route-dreieck dreieck))))


(defun problem-19 ()
  (flet ((sonntagp (tag monat jahr)
		   (= (wochentag tag monat jahr) 6)))
    (loop for jahr from 1901 to 2000
       sum (loop with anzahl = 0
              for monat from 1 to 12
              when (sonntagp 1 monat jahr)
              do (incf anzahl)
                finally (return anzahl)))))


(defun problem-20 ()
  (quersumme (faktor 100)))


(defun problem-21 (&optional (max 10000))
  (loop for i from 1 below max
     for bz = (befreundete-zahl-p i)
     when (and bz (/= i bz) (< bz max))
     sum i into summe
     finally (return summe)))


(defun problem-22 ()
  (let* ((datei (drakma:http-request "https://projecteuler.net/project/resources/p022_names.txt")) 
         (namensliste (erstelle-wortliste datei)))
    (loop for i from 1 to (length namensliste)
       sum (* i (alphabetischer-wert (pop namensliste))))))


(defun problem-23 (&optional (maximum 28123))
  (let (liste-abundanter-zahlen
		(summe 0))
	;; Summe aller Zahlen von 1-Maximum in summe speichern und
	;; eine Liste aller abundanten Zahlen erstellen
	(do ((i 1 (1+ i)))
		((> i maximum)
		 (setf liste-abundanter-zahlen (nreverse liste-abundanter-zahlen)))
	  (incf summe i)
	  (when (abundante-zahl-p i)
		(push i liste-abundanter-zahlen)))
	;; Ermittle die gesuchte Summe
	(do ((i 12 (1+ i)))
		((> i maximum)
		 summe)
	  (dolist (j liste-abundanter-zahlen)
		(when (>= j i)
		  (return))
		(when (abundante-zahl-p (- i j))
		  (decf summe i) ; bei jedem Treffer Reduzierung der Summe um i
		  (return))))))


(defun problem-24 ()
  (liste->zahl (permutations-rang 1000000 '(0 1 2 3 4 5 6 7 8 9))))


(defun problem-25 ()
  (loop for i upfrom 1
     when (= (length (zahl->liste (fibonacci i))) 1000)
     return i))


(defun problem-26 ()
  (labels ((kehrwert-zyklus-länge (n)
			 (let ((i 1))
			   (cond
				 ((zerop (rem n 2))
				  (kehrwert-zyklus-länge (truncate (/ n 2.0))))
				 ((zerop (rem n 5))
				  (kehrwert-zyklus-länge (truncate (/ n 5.0))))
				 (t (do ()
						((zerop (rem (1- (expt 10 i)) n))
						 i)
					  (incf i)))))))
	  (do ((i 1 (1+ i))
		   (länge 0)
		   (max-i 0)
		   (max-länge 0))
		  ((= i 1000)
		   max-i)
		(when (= 1 (gcd i 10))
		  (setf länge (kehrwert-zyklus-länge i))
		  (when (> länge max-länge)
			(setf max-länge länge
				  max-i i))))))


(defun problem-27 ()
  (labels ((primzahl-reihe (a b &optional (n 0))
			 (if (primzahlp (+ (expt n 2) (* a n) b))
				 (primzahl-reihe a b (1+ n))
				 n)))
	(let ((zahl1 0)
		  (zahl2 0)
		  (gezählte-primzahlen 0)
		  aktuelle-primzahlen)
      (loop for a from -999 below 0
         do (loop for b in (sieb-des-eratosthenes 999)
               when (> (setf aktuelle-primzahlen (primzahl-reihe a b))
                       gezählte-primzahlen)
               do (setf zahl1 a
                       zahl2 b
                       gezählte-primzahlen aktuelle-primzahlen))
           finally (return (* zahl1 zahl2))))))


(defun problem-28 (&optional (max 1000))
  (1+ (loop with kandidat = 1
         for i from 2 to max by 2
         sum (loop for j from 1 to 4
                do (incf kandidat i)
                sum kandidat))))


(defun problem-29 ()
  (loop with sammlung = ()
     for a from 2 to 100
     do (loop for b from 2 to 100
           do (pushnew (expt a b) sammlung))
     finally (return (length sammlung))))


(defun problem-30 (&optional (max 200000))
  (labels ((expt-ziffern (n p &optional (sum 0))
			 (if (zerop n)
				 sum
				 (expt-ziffern (truncate (/ n 10)) p (+ sum (expt (rem n 10) p))))))
    (loop for i from 2 to max
       if (= i (expt-ziffern i 5))
       sum i)))


(defun problem-31 (&optional (ziel 200))
  (let ((münzen '(1 2 5 10 20 50 100 200))
        (wege (make-array 201 :initial-element 0)))
    (incf (aref wege 0))
    (loop for i in münzen
       do (loop for j from i to ziel
               do (incf (aref wege j) (aref wege (- j i)))))
    (aref wege ziel)))


(defun problem-32 ()
  (flet ((alle-pandigitalen-produkte ()
           (loop with liste = ()
              for i from 1 below 100
              do (loop for j from 100 below 10000
                    for produkt = (* i j)
                    if (and (< produkt 9999)
                            (pandigitalp (append (zahl->liste i)
                                                 (zahl->liste j)
                                                 (zahl->liste produkt))))
                    do (pushnew produkt liste))
              finally (return liste))))
	(reduce #'+ (alle-pandigitalen-produkte))))


(defun problem-33 ()
  (let (liste)
    (loop for zähler from 11 to 98
         do (loop for nenner from (1+ zähler) to 99
               when (and (not (zerop (mod zähler 10)))
                         (not (zerop (mod nenner 10)))
                         (= (mod zähler 10)
                            (floor nenner 10))
                         (= (/ (/ (- zähler (mod zähler 10)) 10)
                               (mod nenner 10))
                            (/ zähler nenner)))
               do (push (/ zähler nenner) liste)))
    (denominator (reduce #'* liste))))


(defun problem-34 (&optional (max (* (faktor 9) 7)))
  (loop for i from 3 to max
     when (= i (reduce #'+ (mapcar #'faktor (zahl->liste i))))
		sum i))


(defun problem-35 ()
  (let (liste)
	(dolist (i (sieb-des-eratosthenes 999999) (values (length liste) (nreverse liste)))
	  (when (kreisförmige-primzahl-p i) 
		(push i liste)))))


(defun problem-36 ()
  (flet ((zweibasiges-palindrom-p (zahl)
		   (and (palindromp zahl)
                (palindromp (format nil "~B" zahl)))))
    (loop for i from 1 below 1000000
       when (zweibasiges-palindrom-p i)
         sum i)))


(defun problem-37 ()
  (loop for i in (sieb-des-eratosthenes 1000000)
     when (trunkierbare-primzahl-p i)
     sum i))


(defun problem-38 ()
  (loop for i downfrom 9999
     if (pandigitalp (append (zahl->liste i) (zahl->liste (* 2 i))))
     do (return (parse-integer (format nil "~a~a" i (* 2 i))))))


(defun problem-39 ()
  (do ((p 12 (+ p 2))
	   (max-p 0)
	   (max-anzahl 0))
	  ((> p 1000)
	   max-p)
	(do ((a 1 (1+ a))
		 (anzahl 0))
		((> a (/ p 3))
		 (when (> anzahl max-anzahl)
		   (setf max-anzahl anzahl
				 max-p p)))
	  (do ((b a (1+ b))
		   (a² (expt a 2)))
		  ((> b (/ (- p a) 2)))
		(let ((c (- p a b)))
		  (when (= (+ a² (expt b 2)) (expt c 2))
			(incf anzahl)))))))


(defun problem-40 ()
  (let ((digits (loop for i from 0 to 200000 append (zahl->liste i))))
	(apply #'* (mapcar #'(lambda (n) (nth n digits)) '(1 10 100 1000 10000 100000 1000000)))))


(defun problem-41 ()
  (loop for i in (sieb-des-eratosthenes 7654321)
     if (pandigitalp i)
     maximize i))


(defun problem-42 ()
  (let* ((datei (drakma:http-request "https://projecteuler.net/project/resources/p042_words.txt"))
         (wortliste (erstelle-wortliste datei))
		 (länge (length wortliste)))
	(do ((i 1 (1+ i))
		 (anzahl 0))
		((> i länge)
		 anzahl)
	  (when (dreieckszahlp (alphabetischer-wert (pop wortliste)))
		(incf anzahl)))))


(defun problem-43 ()
  (flet ((teile-p (n lst)
		   (zerop (mod (liste->zahl lst) n))))
	(let ((liste '(0 1 2 3 4 5 6 7 8 9))
		  (summe 0))
	  ;; Durchlaufe für jede Ziffer eine separat abgestimmte Liste
	  ;; Die Liste für d4 ist nur für durch 2 teilbare Zahlen, hier können wir teile-p 2 einsparen
	  ;; Die Liste für d6 ist für durch 5 teilbare Zahlen, wieder eine zeitliche Einsparmöglichkeit
	  (dolist (d1 liste summe)
		(dolist (d2 (set-difference liste (list d1)))
		  (dolist (d3 (set-difference liste (list d1 d2)))
			(dolist (d4 (set-difference '(0 2 4 6 8) (list d1 d2 d3))) ; Teilbarkeit durch 2, erspart teile-p 2
			  (dolist (d5 (set-difference liste (list d1 d2 d3 d4)))
				(dolist (d6 (set-difference '(0 5) (list d1 d2 d3 d4 d5))) ; Teilbarkeit durch 5, schnelleres teile-p 5
				  (dolist (d7 (set-difference liste (list d1 d2 d3 d4 d5 d6)))
					(dolist (d8 (set-difference liste (list d1 d2 d3 d4 d5 d6 d7)))
					  (dolist (d9 (set-difference liste (list d1 d2 d3 d4 d5 d6 d7 d8)))
						(dolist (d10 (set-difference liste (list d1 d2 d3 d4 d5 d6 d7 d8 d9)))
						  (when (and (teile-p 3 (list d3 d4 d5))
									 (teile-p 5 (list d4 d5 d6))
									 (teile-p 7 (list d5 d6 d7))
									 (teile-p 11 (list d6 d7 d8))
									 (teile-p 13 (list d7 d8 d9))
									 (teile-p 17 (list d8 d9 d10)))
							(incf summe (liste->zahl (list d1 d2 d3 d4 d5 d6 d7 d8 d9 d10)))))))))))))))))

		
(defun problem-44 ()
  (do ((pentagonal 1 (+ pentagonal adder))
       (adder 4 (+ 3 adder))
       (addlist nil (cons adder
						  (mapcar #'(lambda (x) (+ x adder)) addlist)))
       (plist nil (cons pentagonal plist))
       (result nil (loop for i in addlist
					  if (and (member i plist)
							  (member (- pentagonal (* 2 i)) plist))
					  return (- pentagonal i i))))
      (result result)))


(defun problem-45 ()
  (let ((tabelle (make-hash-table))
		(limit 100000))
	(flet ((fülle-pentagon-hash (limit)
			 (do ((i 1 (1+ i)))
				 ((= i limit))
			   (let
				   ((pentagon (* i (1- (* 3 i)) 1/2)))
				 (incf (gethash pentagon tabelle 0)))))
		   (fülle-hexagon-hash (limit)
			 (do ((i 1 (1+ i)))
				 ((= i limit))
			   (let
				   ((hexagon (* i (1- (* 2 i)))))
				 (incf (gethash hexagon tabelle 0))))))
	  (fülle-pentagon-hash limit)
	  (fülle-hexagon-hash limit))
	(third (loop for key being each hash-key of tabelle
			  if (>= (gethash key tabelle) 2)
			  collect key))))


(defun problem-46 ()
  (flet ((goldbach-aufgliedern (n)
		   (let ((max (isqrt n)))
			 (do ((i 1 (1+ i)))
				 ((> i max))
			   (let ((p (- n (* 2 (expt i 2)))))
				 (when (primzahlp p)
				   (return (list p i))))))))
    (loop for i upfrom 3 by 2
       unless (primzahlp i)
       when (null (goldbach-aufgliedern i))
       do (return i))))


(defun problem-47 (&optional (fortlaufend 4) (zahl 645))
  (if (= fortlaufend
         (length (remove-duplicates (primfaktoren zahl)))
         (length (remove-duplicates (primfaktoren (+ zahl 1))))
         (length (remove-duplicates (primfaktoren (+ zahl 2))))
         (length (remove-duplicates (primfaktoren (+ zahl 3)))))
      zahl
      (problem-47 fortlaufend (1+ zahl))))


(defun problem-48 ()
  (liste->zahl (last (zahl->liste (loop for i from 1 to 1000
                        sum (expt i i)))
                     10)))


(defun problem-49 ()
  (loop for i upfrom 1489 by 2
     for i2 = (+ i 3330)
     for i3 = (+ i 6660)
     when (and (primzahlp i) (primzahlp i2) (primzahlp i3))
       do (let ((l1 (remove-duplicates (zahl->liste i)))
             (l2 (remove-duplicates (zahl->liste i2)))
             (l3 (remove-duplicates (zahl->liste i3))))
         (when (and (subsetp l1 l2) (subsetp l1 l3) (subsetp l2 l1) (subsetp l3 l1))
           (return (parse-integer (format nil "~a~a~a" i (+ i 3330) (+ i 6660))))))))


(defun problem-50 ()
  (let ((max-zahl 0)
		(max-anzahl 0)
		(obergrenze (second (summe-fortlaufender-primzahlen 1 1000000))))
	(dolist (i (sieb-des-eratosthenes (1- obergrenze)) max-zahl)
	  (let* ((werte (summe-fortlaufender-primzahlen i 1000000))
			 (zahl (first werte))
			 (anzahl (second werte)))
		(when (and (> anzahl max-anzahl) (> zahl max-zahl) (primzahlp zahl))
		  (setf max-zahl zahl)
		  (setf max-anzahl anzahl))))))


(defun problem-51 ()
  (let ((kandidaten (remove-if #'(lambda (x) (< x 100000)) (sieb-des-eratosthenes 1000000)))
		(anzahl-primzahlen 8))
	(flet ((teste (i j)
			 (let ((anzahl 0))
			   (do ((k (if (= j (first (zahl->liste i))) 1 0) (1+ k)))
				   ((> k 9))
				 (if (member (tausche-ziffer i j k) kandidaten)
					 (incf anzahl))
				 (if (= anzahl anzahl-primzahlen)
					 (return t))
				 (if (< (+ anzahl (- 9 k)) anzahl-primzahlen)
					 (return nil))))))
	  (dolist (i kandidaten)
		(dolist (j (zahl->liste i))
		  (if (teste i j)
			  (return-from problem-51 i)))))))


(defun problem-52 ()
  (labels ((gleiche-ziffern (i maxmult)
			 (let ((d (sortiere-ziffern i)))
			   (do ((m 2 (1+ m)))
				   ((> m maxmult)
					(return t))
				 (unless (equal (sortiere-ziffern (* m i)) d)
				   (return nil)))))
		   (finde-gleiche-ziffern (maxmult)
			 (do ((i 100000 (1+ i)))
				 ((> i 1000000))
			   (if (gleiche-ziffern i maxmult)
				   (return i)))))
	(finde-gleiche-ziffern 6)))


(defun problem-53 (&optional (limit 100) (minimum 1000000))
  (labels ((möglichkeit (n r)
			 (/ (faktor n) (faktor r) (faktor (- n r)))))
	(do ((i 1 (1+ i))
		 (summe 0))
		((> i limit)
		 summe)
	  (dotimes (j i)
		(when (> (möglichkeit i j) minimum)
		  (incf summe))))))


(defun problem-54 ()
  (labels ((karten-wert (karte)
			 (declare (type (string 2) karte))
			 (or (parse-integer karte :end 1 :junk-allowed t)
				 (ecase (char-upcase (char karte 0))
				   (#\T 10)
				   (#\J 11)
				   (#\Q 12)
				   (#\K 13)
				   (#\A 14))))

		   (karte= (karte &rest weitere-karten)
			 (apply #'= (mapcar #'karten-wert (cons karte weitere-karten))))

		   (karte< (karte &rest weitere-karten)
			 (apply #'< (mapcar #'karten-wert (cons karte weitere-karten))))

		   (farbe= (karte &rest weitere-karten)
			 (apply #'char= (mapcar (lambda (c) (char-upcase (char c 1)))
									(cons karte weitere-karten))))

		   (entferne-karten (blatt &rest karten)
			 (set-difference blatt karten :test #'karte=))

		   (flushp (blatt)
			 (apply #'farbe= blatt))

		   (straightp (blatt)
			 (let ((sortiertes-blatt (sort (copy-seq blatt) #'karte<)))
			   (loop for a in sortiertes-blatt
				  for b in (rest sortiertes-blatt)
				  when (/= 1 (- (karten-wert b) (karten-wert a)))
				  do (return nil)
				  finally (return t))))

		   (n-einer-art-p (blatt n)
			 (dolist (test-karte (remove-duplicates blatt :test #'karte=))
			   (when (<= n (length (remove-if (lambda (c) (not (karte= test-karte c)))
											  blatt)))
				 (return test-karte))))

		   (höchste-karte (blatt)
			 (first (last (sort (copy-seq blatt) #'karte<))))

		   (höchste-karte< (h0 h1)
             (karte< (höchste-karte h0) (höchste-karte h1)))

		   (blatt< (blatt0 blatt1)
			 (labels ((blatt-test (test gleichstand-lösen)
						(if (funcall test blatt1)
							(return-from blatt<
							  (or (not (funcall test blatt0))
								  (funcall gleichstand-lösen blatt0 blatt1))))
						(if (funcall test blatt0)
							(return-from blatt< nil))))
			     
			 (or (blatt-test (lambda (h) (and (straightp h) (flushp h))) #'höchste-karte<) ; Straight / Flush
				 ;; Vierling
				 (blatt-test  (lambda (h) (n-einer-art-p h 4))
							  (lambda (h0 h1) (karte< (n-einer-art-p h0 4)
													  (n-einer-art-p h1 4))))
				 ;; Full House
				 (blatt-test  (lambda (h) (let ((3-of-a-kind (n-einer-art-p h 3)))
											(and 3-of-a-kind
												 (n-einer-art-p (entferne-karten h 3-of-a-kind)
																2))))
							  (lambda (h0 h1) (karte< (n-einer-art-p h0 3)
													  (n-einer-art-p h1 3))))
				 ;; Flush
				 (blatt-test #'flushp (lambda (h0 h1) (loop for c0 in (nreverse (sort (copy-seq h0) #'karte<))
														 for c1 in (nreverse (sort (copy-seq h1) #'karte<))
														 for result = (karte< c0 c1)
														 while (karte= c0 c1)
														 finally (return result))))
				 ;; Straight
				 (blatt-test #'straightp #'höchste-karte<)
				 ;; Drilling
				 (blatt-test (lambda (h) (n-einer-art-p h 3))
							 (lambda (h0 h1) (karte< (n-einer-art-p h0 3)
													 (n-einer-art-p h1 3))))
				 ;; Zwei Paare
				 (blatt-test (lambda (h) (let ((paar (n-einer-art-p h 2)))
										   (and paar (n-einer-art-p (entferne-karten h paar) 2))))
							 (lambda (h0 h1)
							   (let* ((paar0-0 (n-einer-art-p h0 2))
									  (paar0-1 (n-einer-art-p
												(entferne-karten h0 paar0-0) 2))
									  (paar1-0 (n-einer-art-p h1 2))
									  (paar1-1 (n-einer-art-p
												(entferne-karten h1 paar1-0) 2))
									  (entpaare0 (entferne-karten h0 paar0-0 paar0-1))
									  (entpaare1 (entferne-karten h1 paar1-0 paar1-1)))
								 (if (karte< paar0-0 paar0-1)
									 (rotatef paar0-0 paar0-1))
								 (if (karte< paar1-0 paar1-1)
									 (rotatef paar1-0 paar1-1))
								 (or (karte< paar0-0 paar1-0)
									 (and (karte= paar0-0 paar1-0)
										  (or (karte< paar0-1 paar1-1)
											  (and (karte= paar0-1 paar1-1)
												   (karte< entpaare0 entpaare1))))))))
				 ;; Ein Paar
				 (blatt-test (lambda (h) (n-einer-art-p h 2))
							 (lambda (h0 h1) (let ((paar0 (n-einer-art-p h0 2))
												   (paar1 (n-einer-art-p h1 2)))
											   (or (karte< paar0 paar1)
												   (and (karte= paar0 paar1)
														(höchste-karte< (remove paar0 h0 :test #'karte=)
																		(remove paar1 h1 :test #'karte=)))))))
				 ;; Höchste Karte
				 (blatt-test (lambda (h) t) #'höchste-karte<))))
	
		   (erstelle-kartenliste (stream-name)
			 "Einleseformat: 10 durch Leerzeichen getrennte Daten je Zeile"
			 (let (kartenliste)
			   (with-input-from-string (stream stream-name)
				 (do ((i (read-line stream nil)
						 (read-line stream nil)))
					 ((null i)
					  (reverse kartenliste))
				   (push (string-aufteilen i) kartenliste))))))
	;; ---------- ENDE der Unterprogramme ----------------------------------------
	(let* ((datei (drakma:http-request "https://projecteuler.net/project/resources/p054_poker.txt"))
          (kartenliste (erstelle-kartenliste datei)))
	  (loop for blatt-paar in kartenliste
		 when (blatt< (nthcdr 5 blatt-paar) (butlast blatt-paar 5))
		 sum 1))))


(defun problem-55 ()
  (loop for i from 1 to 10000
     when (lychrel-zahl-p i)
       count i))


(defun problem-56 ()
  (do ((a 1 (1+ a))
	   (maximum 0))
	  ((>= a 100)
	   maximum)
	(do ((b 1 (1+ b)))
		((>= b 100))
	  (let ((wert (ziffer-summe (expt a b))))
		(when (> wert maximum)
		  (setf maximum wert))))))


(defun problem-57 ()
  (labels ((nächste-expansion (bruch)
			 (+ 1 (/ 1 (+ 1 bruch))))
		   (expansions-schleife (durchläufe)
			 (loop for i from 0 to durchläufe
				as bruch = 1 then (nächste-expansion bruch)
				collecting bruch))
		   (zähle-treffer (brüche)
			 (let ((anzahl 0))
			   (dolist (bruch brüche anzahl)
				 (when (> (length (zahl->liste (numerator bruch)))
						  (length (zahl->liste (denominator bruch))))
				   (incf anzahl))))))
	(zähle-treffer (expansions-schleife 1000))))


(defun problem-58 ()
  (loop 
     for side = 0 then (if (= side 3) 0 (1+ side))
     for size = 3 then (if (zerop side) (+ 2 size) size)
     for no = 3 then (+ (1- size) no)
     count (primzahlp no) into primes
     count no into total
     until (and (zerop side) (<= (/ primes total) 1/10))
     finally (return (- size 2))))


(defun problem-59 ()
  (labels ((mögliche-entschlüsselung (pw1 pw2 pw3 crypto-text)
			 (let* ((summe 0)
					(entschlüsselt (with-output-to-string (sstr)
									 (let ((p crypto-text))
									   (dotimes (i 400)
										 (let ((d1 (logxor (pop p) pw1))
											   (d2 (logxor (pop p) pw2))
											   (d3 (logxor (pop p) pw3)))
										   (incf summe (+ d1 d2 d3))
										   (format sstr "~C~C~C" (code-char d1) (code-char d2) (code-char d3))))
									   (let ((d (logxor (first p) pw1)))
										 (incf summe d)
										 (write-char (code-char d) sstr))))))
			   (if (and (search " the " entschlüsselt) (search ". " entschlüsselt))
				   (values summe entschlüsselt)
				   nil)))
		   (entschlüssle-alles ()
			 (let* ((datei (drakma:http-request "https://projecteuler.net/project/resources/p059_cipher.txt"))
                    (crypto-text (erstelle-zahlenliste datei)))
			   (do ((i 97 (1+ i)))
				   ((> i 122))
				 (do ((j 97 (1+ j)))
					 ((> j 122))
				   (do ((k 97 (1+ k)))
					   ((> k 122))
					 (let ((pd (mögliche-entschlüsselung i j k crypto-text)))
					   (when pd
						 (format t "~a~%" pd)))))))))
	(entschlüssle-alles)))


(defun problem-60 (&optional (max 8400))
  (labels ((kombiniere (m n)
			 (parse-integer (format nil "~d~d" m n)))
		   (kombinierbare-primzahlen-p (m n)
			 (and (primzahlp (kombiniere m n))
				  (primzahlp (kombiniere n m))))
		   (alle-primzahlen-kombinierbar-p (&optional lst)
			 (labels ( (teste-liste (&optional lst)
						 (cond ((null lst)
								nil)
							   ((null (rest lst))
								(primzahlp (first lst)))
							   (t
								(let ((a (first lst)))
								  (dolist (b (rest lst) 't)
									(unless (kombinierbare-primzahlen-p a b)
									  (return-from teste-liste nil))))))))
			   (unless (null lst)
				 (notany #'null (maplist #'teste-liste lst))))))
	(let* ((primzahlen (sieb-des-eratosthenes max))
		   lst
		   (primzahlen2 (dolist (i primzahlen (sort lst #'<))
						  (do ((j (nächste-primzahl i) (nächste-primzahl j)))
							  ((or (kombinierbare-primzahlen-p i j) (> j max))
							   (when (< j max)
								 (pushnew i lst)
								 (pushnew j lst)))))))
	  (dolist (i primzahlen2)
		(dolist (j (rest (member i primzahlen2)))
		  (dolist (k (rest (member j primzahlen2)))
			(when (alle-primzahlen-kombinierbar-p (list i j k))
			  (dolist (l (rest (member k primzahlen2)))
				(when (alle-primzahlen-kombinierbar-p (list i j k l))
				  (dolist (m (rest (member l primzahlen2)))
					(when (alle-primzahlen-kombinierbar-p (list i j k l m))
					  (return-from problem-60 (values (+ i j k l m) (list i j k l m))))))))))))))


(defun problem-61 ()
  (let ((h (make-hash-table))
		(to (make-hash-table)))
	(labels ((p-funktion (typ n)
			   (case typ
				 (3 (* n (1+ n) 1/2))
				 (4 (* n n))
				 (5 (* n (1- (* 3 n)) 1/2))
				 (6 (* n (1- (* 2 n))))
				 (7 (* n (- (* 5 n) 3) 1/2))
				 (8 (* n (- (* 3 n) 2)))))
			 (dostep (start check target path)
			   (if (= (length check) 5)
				   (let* ((final (+ (* start 100) target))
						  (fk (gethash final h)))
					 (when (and fk (not (intersection fk check)))
					   (return-from problem-61
						 (values (reduce #'+ (cons final path))
								 (reverse (cons final path))))))
				   (loop for x in (gethash start to)
					  for ks = (gethash x h)
					  for freek = (set-difference ks check)
					  when freek do (loop for k in freek
									   do (dostep (mod x 100)
												  (cons k check)
												  target (cons x path)))))))
	  (loop for typ from 3 to 8
		 do (loop for n from 1
			   for p = (p-funktion typ n)
			   when (> p 9999) do (return)
			   when (and (> p 1000)
						 (> (mod p 100) 9))
			   do (push typ (gethash p h))
				 (pushnew p (gethash (floor p 100) to))))
	  (loop for start from 10 to 99 do (dostep start nil start nil)))))


(defun problem-62 ()
  (flet ((finde-permutationen (gesuchte-anzahl)
		   (let ((hash-tabelle (make-hash-table :test 'equal)))
			 (do ((i 1 (1+ i)))
				 (nil)
			   (let* ((ziffern (sortiere-ziffern (expt i 3)))
					  (neueintrag (push i (gethash ziffern hash-tabelle))))
				 (when (>= (length neueintrag) gesuchte-anzahl)
					 (return (expt (first (last neueintrag)) 3))))))))
	(finde-permutationen 5)))


(defun problem-63 ()
  (flet ((finde-anzahl ()
		   (do ((i 1 (1+ i))
				(anzahl 0))
			   ((> i 100)
				anzahl)
			 (do ((j 1 (1+ j)))
				 ((> j 100))
			   (when (= j (length (zahl->liste (expt i j))))
				 (incf anzahl))))))
	(finde-anzahl)))


(defun problem-64 (&optional (limit 10000))
  (flet ((finde-ungerade-periode (x)
           (let* ((a (isqrt x)))
             (when (= (expt a 2) x)
               (return-from finde-ungerade-periode 'nil))
             (let ((a0 a)
                   (m 0)
                   (d 1)
                   (n nil))
               (while (/= a (* a0 2))
                 (setf n (not n))
                 (setf m (- (* d a) m))
                 (setf d (truncate (/ (- x (expt m 2)) d)))
                 (setf a (truncate (/ (+ a0 m) d))))
               n))))
    (loop for i from 2 to limit
       count (finde-ungerade-periode i))))


(defun problem-65 (&optional (limit 100))
  (labels ((annäherung-an-e (von bis)
             (cond ((= von bis)
                    0)
                   ((zerop von)
                    (+ 2 (annäherung-an-e (+ 1 von) bis)))
                   ((= 2 (mod von 3))
                    (/ 1 (+ (* 2 (/ (+ 1 von) 3))
                            (annäherung-an-e (+ 1 von) bis))))
                   (t
                    (/ 1 (+ 1 (annäherung-an-e (+ 1 von) bis)))))))
    (quersumme (numerator (annäherung-an-e 0 limit)))))


(defun problem-66 (&optional (limit 1000))
  (flet ((pellsche-gleichung (n)
		   (do* ((b0 (isqrt n))
				 (l b0 (- (* b p) l))
				 (p (- n (expt b0 2)) (/ (- n (expt l 2)) p))
				 (b (if (zerop p)
						(return 0)
						(floor (+ b0 l) p))
					(floor (+ b0 l) p))
				 (x-1 b0)
				 (x (1+ (* b b0)) (+ (shiftf x-1 x) (* b x)))
				 (y-1 1)
				 (y b (+ (shiftf y-1 y) (* b y))))
				((= 1 (- (expt x 2) (* n (expt y 2))))
				 x))))
	(do ((i 2 (1+ i))
		 (max-x 0)
		 (d 0))
		((> i limit)
		 d)
	  (let ((x (pellsche-gleichung i)))
		(when (> x max-x)
		  (setf max-x x
				d i))))))

  
(defun problem-67 ()
  (flet ((erstelle-zahlenpyramide (stream-name)
		   (let (zahlenliste)
			 (with-input-from-string (stream stream-name)
			   (do ((i (read-line stream nil)
					   (read-line stream nil)))
				   ((null i)
					zahlenliste)
				 (push (mapcar #'parse-integer (string-aufteilen i))  zahlenliste))))))
    (let ((datei (drakma:http-request "https://projecteuler.net/project/resources/p067_triangle.txt")))
      (first (route-dreieck (erstelle-zahlenpyramide datei))))))


(defun problem-68 ()
  (flet ((finde-lösung ()
		   (let ((liste '(10 9 8 7 6 5 4 3 2 1)))
			 (dolist (d1 liste)
			   (dolist (d2 (set-difference liste (list d1)))
				 (dolist (d3 (set-difference liste (list d1 d2)))
				   (dolist (d4 (set-difference liste (list d1 d2 d3)))
					 (dolist (d5 (set-difference liste (list d1 d2 d3 d4)))
					   (dolist (d6 (set-difference liste (list d1 d2 d3 d4 d5)))
						 (dolist (d7 (set-difference liste (list d1 d2 d3 d4 d5 d6)))
						   (dolist (d8 (set-difference liste (list d1 d2 d3 d4 d5 d6 d7)))
							 (dolist (d9 (set-difference liste (list d1 d2 d3 d4 d5 d6 d7 d8)))
							   (dolist (d10 (set-difference liste (list d1 d2 d3 d4 d5 d6 d7 d8 d9)))
								 (when (and (< d1 d5 d4 d3 d2)
											(= (+ d1 d6 d7)
											   (+ d2 d7 d8)
											   (+ d3 d8 d9)
											   (+ d4 d9 d10)
											   (+ d5 d10 d6)))
								   (return-from finde-lösung (list (list d1 d6 d7)
																   (list d2 d7 d8)
																   (list d3 d8 d9)
																   (list d4 d9 d10)
																   (list d5 d10 d6)))))))))))))))))
	(finde-lösung)))


(defun problem-69 ()
  (flet ((finde-maximales-n (l)
		   (do ((p (nächste-primzahl) (nächste-primzahl p))
				(n 1))
			   (nil)
			 (when (> (* n p) l)
			   (return-from finde-maximales-n n))
			 (setf n (* n p)))))
	(finde-maximales-n 1000000)))


(defun problem-70 (&optional (limit (expt 10 7)))
  (let ((best 6)
        (ratio 3.0)
		(primzahlen (sieb-des-eratosthenes limit)))
	(labels ((ist-permutation-p (a b)
			   (equal (sort (zahl->liste a) #'<) (sort (zahl->liste b) #'<)))
			 (finde-ratio (limit)
			   (loop for a in primzahlen
				  do (loop for b in primzahlen
						as n = (* b a)
						as phi = (1+ (- n a b))
						while (< b a)
						while (< n limit)
						if (and (< (/ n phi) ratio) (ist-permutation-p n phi))
						do (progn
							 (setf ratio (/ n phi)
								   best n)))
				  finally (return best))))
	  (finde-ratio limit))))


(defun problem-71 ()
  (1- (truncate (* 3/7 (expt 10 6)))))


(defun problem-72 ()
  (- (summe-der-farey-folge (expt 10 6)) 2))


(defun problem-73 ()
  (let ((ratiohash (make-hash-table)))
	(labels ((fill-ratio-hash (limit)
			   (loop for d from 1 to limit
				  do (loop for n from (ceiling (/ d 3)) to (floor (/ d 2))
						do (incf (gethash (/ n d) ratiohash 0)))))
			 (zähle-brüche (limit)
			   (fill-ratio-hash limit)
			   (remhash (/ 1 3) ratiohash)
			   (remhash (/ 1 2) ratiohash)
			   (hash-table-count ratiohash)))
	  (zähle-brüche 12000))))


(defun problem-74 ()
  (let ((kette (make-hash-table)))
	(labels ((speichere-längen (liste &optional (zusatz 0))
			   (do ((i 0 (1+ i))
					(l (length liste)))
				   ((= i l))
				 (setf (gethash (elt liste i) kette 0) (+ (- l i) zusatz))))
			 (faktor-ziffer-summe (n)
			   (reduce #'+ (mapcar #'faktor (zahl->liste n))))
			 (ziffer-faktoren-kette (n &optional liste)
			   (when (member n liste)
				 (speichere-längen (reverse liste))
				 (return-from ziffer-faktoren-kette (length liste)))
			   (let ((n-bekannt (gethash n kette 0)))
				 (unless (zerop n-bekannt)
				   (speichere-längen (reverse liste) n-bekannt)
				   (return-from ziffer-faktoren-kette (+ (length liste) n-bekannt))))
			   (push n liste)
			   (ziffer-faktoren-kette (faktor-ziffer-summe n) liste))
			 (zähle-treffer (limit)
			   (do ((i 1 (1+ i))
					(anzahl 0))
				   ((> i limit)
					anzahl)
				 (when (= (ziffer-faktoren-kette i) 60)
				   (incf anzahl)))))
	  (zähle-treffer (1- (expt 10 6))))))


(defun problem-75 ()
  (flet ((zähle-dreiecke (limit)
		   (let ((dreieck (make-array (1+ limit)))
				 (mlimit (isqrt (/ limit 2))))
			 (do ((m 2 (1+ m))
				  (anzahl 0))
				 ((>= m mlimit)
				  anzahl)
			   (do ((n 1 (1+ n)))
				   ((>= n m))
				 (when (and (= 1 (mod (+ n m) 2))
							(= 1 (gcd n m)))
				   (let* ((m² (expt m 2))
						  (n² (expt n 2))
						  (a (+ m² n²))
						  (b (- m² n²))
						  (c (* 2 m n))
						  (p (+ a b c)))
					 (do ()
						 ((> p limit))
					   (incf (elt dreieck p))
					   (when (= 1 (elt dreieck p))
						 (incf anzahl))
					   (when (= 2 (elt dreieck p))
						 (decf anzahl))
					   (incf p (+ a b c))))))))))
	(zähle-dreiecke 1500000)))


(defun problem-76 ()
  (let ((partitionen (make-hash-table :test #'equal)))
	(labels ((füll-helfer (n maximum)
			   (let ((memo (gethash (list n maximum) partitionen)))
				 (if memo
					 memo
					 (loop for i from 1 to maximum
						sum (füll-helfer (- n i) (min i (- n i) maximum)) into p
						finally (return (setf (gethash (list n maximum) partitionen) p))))))
			 (fülle-partitionen (maximum)
			   (setf (gethash (list 0 0) partitionen) 1
					 (gethash (list 1 1) partitionen) 1)
			   (loop for n from 1 to maximum
				  do (füll-helfer n n))))
	  (fülle-partitionen 200)
	  (gethash (list 100 99) partitionen))))  


(defun problem-77 ()
  (let ((partitionen (make-hash-table :test #'equal))
		(primzahlen  (sieb-des-eratosthenes 100)))
	(labels ((füll-helfer (n maximum)
			   (let ((memo (gethash (list n maximum) partitionen)))
				 (if memo memo (loop for i in primzahlen while (<= i maximum)
								  sum (füll-helfer (- n i) (min i (- n i) maximum)) into p finally
									(return (setf (gethash (list n maximum) partitionen) p))))))
			 (fülle-partitionen (maximum)
			   (setf (gethash (list 0 0) partitionen) 1)
			   (setf (gethash (list 1 1) partitionen) 0)
			   (loop for n from 1 to maximum do (füll-helfer n n))))
	  (fülle-partitionen 10)
	  (loop for i from 10 do (füll-helfer i i)
		 if (> (gethash (list i i) partitionen) 5000) return i))))


(defun problem-79 ()
  (labels ((erstelle-keylogliste (stream-name)
			 (let (zahlenliste)
			   (with-input-from-string (stream stream-name)
				 (do ((i (read stream nil)
						 (read stream nil)))
					 ((null i)
					  zahlenliste)
				   (push i zahlenliste)))))
		   (teste-ziffer-p (ziffer liste)
			 (let (vorhanden)
			   (dolist (i liste vorhanden)
				 (when (eql ziffer (first i))
				   (setf vorhanden t))
				 (when (or (eql ziffer (second i)) (eql ziffer (third i)))
				   (return nil)))))
		   (entferne-ziffer (ziffer liste)
			 (let (neue-liste)
			   (dolist (i liste neue-liste)
				 (push (delete ziffer i) neue-liste))))
		   (suche-ziffer (kandidaten liste)
			 (dolist (i kandidaten nil)
			   (when (teste-ziffer-p i liste)
				 (return i)))))
	(let* ((datei (drakma:http-request "https://projecteuler.net/project/resources/p079_keylog.txt"))
           (keylog (mapcar #'zahl->liste (erstelle-keylogliste datei)))
		  (kandidaten '(1 2 3 4 5 6 7 8 9 0)))
	  (do ((i 1 (1+ i))
           lösung)
		  ((> i 10)
		   (liste->zahl (delete nil (reverse lösung))))
		(push (suche-ziffer kandidaten keylog) lösung)
		(setf kandidaten (delete (first lösung) kandidaten))
		(setf keylog (entferne-ziffer (first lösung) keylog))))))


(defun problem-81 ()
  (labels ((read-matrix (zeilen spalten dateiname)
		   (let ((matrix (make-array (list zeilen spalten)))
				 (*readtable* (copy-readtable)))
			 (set-syntax-from-char #\, #\Space)
			 (with-input-from-string (stream dateiname)
			   (do ((i 0 (1+ i)))
				   ((>= i 80)
					matrix)
				 (do ((j 0 (1+ j)))
					 ((>= j 80))
				   (setf (aref matrix i j) (read stream)))))))
		 (filtere-kosten (zeilen spalten matrix)
		   (let ((kosten (make-array (list zeilen spalten))))
			 (setf (aref kosten 0 0) (aref matrix 0 0))
			 (do ((j 1 (1+ j)))
				 ((>= j spalten))
			   (setf (aref kosten 0 j) (+ (aref kosten 0 (1- j)) (aref matrix 0 j))))
			 (do ((i 1 (1+ i)))
				 ((>= i zeilen))
			   (setf (aref kosten i 0) (+ (aref kosten (1- i) 0) (aref matrix i 0))))
			 (do ((j 1 (1+ j)))
				 ((>= j spalten)
				  kosten)
			   (do ((i 1 (1+ i)))
				   ((>= i zeilen))
				 (setf (aref kosten i j)
					   (+ (aref matrix i j)
						  (min (aref kosten (1- i) j) (aref kosten i (1- j)))))))))
		 (berechne-minimale-kosten (zeilen spalten dateiname)
		   (let ((matrix (read-matrix zeilen spalten dateiname)))
			 (aref (filtere-kosten zeilen spalten matrix) (1- zeilen) (1- spalten)))))
	(let ((datei (drakma:http-request "https://projecteuler.net/project/resources/p081_matrix.txt")))
     (berechne-minimale-kosten 80 80 datei))))


(defun problem-84 ()
  (flet ((monopoly (&optional (max 1000000) (würfel 4))
		   (let* ((feldnamen '(go a1 cc1 a2 t1 r1 b1 ch1 b2 b3 jail c1 u1 c2 c3 r2 d1 cc2 d2 d3 fp e1 ch2 e2 e3 r3 f1 f2 u2 f3 g2j g1 g2 cc3 g3 r4 ch3 h1 t2 h2))
				  (felder (make-hash-table))
				  (anzahl-felder (length feldnamen))
				  (pos 0)
				  (pasch 0)
				  (ziehe-gemeinschaftskarte (let ((zähler 0))
											  #'(lambda ()
												  (incf zähler)
												  (when (> zähler 16)
													(setf zähler 1))
												  zähler)))
				  (ziehe-ereigniskarte (let ((zähler 0))
										 #'(lambda ()
											 (incf zähler)
											 (when (> zähler 16)
											   (setf zähler 1))
											 zähler))))
			 (labels ((setze-pos (wert)
						(if (>= wert anzahl-felder)
							(decf wert anzahl-felder))
						(if (< wert 0)
							(incf wert anzahl-felder))
						(setf pos wert)
						(case (elt feldnamen pos)
						  ((cc1 cc2 cc3) (gemeinschaftskarte))
						  ((ch1 ch2 ch3) (ereigniskarte))
						  ((g2j) (setze-ort 'jail))))
					  (setze-ort (wert)
						(setze-pos (position wert feldnamen)))
					  (setze-nächster-bahnhof ()
						(cond ((or (< pos (position 'r1 feldnamen)) (> pos (position 'r4 feldnamen)))
							   (setze-ort 'r1))
							  ((< pos (position 'r2 feldnamen))
							   (setze-ort 'r2))
							  ((< pos (position 'r3 feldnamen))
							   (setze-ort 'r3))
							  (t
							   (setze-ort 'r4))))
					  (setze-nächstes-werk ()
						(if (or (< pos (position 'u1 feldnamen)) (> pos (position 'u2 feldnamen)))
							(setze-ort 'u1)
							(setze-ort 'u2)))
					  (setze-drei-felder-zurück ()
						(setze-pos (- pos 3)))
					  (gemeinschaftskarte ()
						(case (funcall ziehe-gemeinschaftskarte)
						  (1 (setze-ort 'go))
						  (2 (setze-ort 'jail))
						  (3 (setze-ort 'c1))
						  (4 (setze-ort 'e3))
						  (5 (setze-ort 'h2))
						  (6 (setze-ort 'r1))
						  (7 (setze-nächster-bahnhof))
						  (8 (setze-nächster-bahnhof))
						  (9 (setze-nächstes-werk))
						  (10 (setze-drei-felder-zurück))))
					  (ereigniskarte ()
						(case (funcall ziehe-ereigniskarte)
						  (1 (setze-ort 'go))
						  (2 (setze-ort 'jail)))))
			   (dotimes (i max)
				 (let ((wurf1 (würfelwurf würfel))
					   (wurf2 (würfelwurf würfel)))
				   (if (= wurf1 wurf2)
					   (incf pasch)
					   (setf pasch 0))
				   (if (= pasch 3)
					   (setze-ort 'jail)
					   (setze-pos (+ pos wurf1 wurf2))))
				 (incf (gethash (elt feldnamen pos) felder 0)))
			   (let (lst)
				 (dotimes (i anzahl-felder)
				   (push (list i (elt feldnamen i) (* (/ (gethash (elt feldnamen i) felder 0) max 1.0) 100)) lst))
				 (sort lst  #'> :key #'third))))))
		 (monopoly)))
  

(defun problem-89 ()
  (labels ((erstelle-ziffernliste (stream-name)
			 (let (ziffernliste)
			   (with-input-from-string (stream stream-name)
				 (do ((i (read stream nil)
						 (read stream nil)))
					 ((null i)
					  ziffernliste)
				   (push (prin1-to-string i) ziffernliste)))))
		   (kleiner-als-4000 (römische-zahl)
			 (- (length römische-zahl) (length (arabisch->römisch (römisch->arabisch römische-zahl)))))
		   (berechne-wert (römische-zahl)
			 (let ((zahl (römisch->arabisch römische-zahl)))
			   (if (< zahl 4000)
				   (kleiner-als-4000 römische-zahl)
				   (kleiner-als-4000 (remove #\M römische-zahl :count 1)))))) 
	(let* ((datei (drakma:http-request "https://projecteuler.net/project/resources/p089_roman.txt"))
           (lang-liste (erstelle-ziffernliste datei))
		  (gespart 0))
	  (dolist (i lang-liste gespart)
		(incf gespart (berechne-wert i))))))


(defun problem-92 (&optional (limit 10000000))
  (labels ((prüfe-zahl (zahl)
			 (cond ((= zahl 1)
					1)
				   ((= zahl 89)
					89)
				   (t
					(prüfe-zahl (reduce #'+ (mapcar #'(lambda (x) (expt x 2))
													(zahl->liste zahl)))))))
		   (zähle-quadrat-ziffern-ketten (limit)
			 (let ((anzahl 0))
			   (do ((i 1 (1+ i)))
				   ((> i limit)
					anzahl)
				 (when (= 89 (prüfe-zahl i))
				   (incf anzahl))))))
	(zähle-quadrat-ziffern-ketten limit)))


(defun problem-95 (&optional (max (expt 10 6)) &aux (max-halbe (truncate (/ max 2))))
  (let ((teiler (make-array (1+ max) :initial-element 1)))
	(labels ((fülle-teiler ()
			   (do ((i 2 (1+ i)))
				   ((> i max-halbe))
				 (do ((j (+ i i) (+ j i)))
					 ((> j max))
				   (setf (aref teiler j) (+ i (aref teiler j))))))
			 (entferne-zu-große ()
			   (do ((i 1 (1+ i)))
				   ((> i max))
				 (when (> (aref teiler i) max)
				   (setf (aref teiler i) 1))))
			 (bearbeite (schritt sprung)
			   (cond ((> schritt max) t)
					 ((and (zerop sprung)
						   (<= (aref teiler schritt) 1))
					  (bearbeite (1+ schritt) sprung))
					 ((zerop sprung)
					  (setf (aref teiler schritt) (list (aref teiler schritt)))
					  (bearbeite schritt (first (aref teiler schritt))))
					 ((= sprung schritt)
					  (bearbeite (1+ schritt) 0))
					 ((or (< sprung schritt)
						  (> (length (member (aref teiler sprung)
											 (aref teiler schritt)))
							 1))
					  (setf (aref teiler schritt) 1) (bearbeite (1+ schritt) 0))
					 (t
					  (setf (aref teiler schritt) (cons (aref teiler sprung)
													 (aref teiler schritt)))
					  (bearbeite schritt (first (aref teiler schritt)))))))
	  (fülle-teiler)
	  (entferne-zu-große)
	  (bearbeite 1 0)
	  (do ((i 0 (1+ i)))
		  ((> i max))
		(unless (consp (aref teiler i))
		  (setf (aref teiler i) (list 1))))
	  (reduce #'(lambda (x y)
				  (if (> (length x) (length y))
					  x
					  y))
			  teiler))))


(defun problem-96 ()
  (labels ((möglichkeiten (tab spalte zeile)
			 "Gibt eine Liste aller Möglichkeiten einer Position zurück"
			 (flet ((zeile-nachbarn (zeile spalte &aux nachbarn)
					  (dotimes (i 9 nachbarn)
						(let ((x (aref tab zeile i)))
						  (unless (or (zerop x) (= i spalte))
							(push x nachbarn)))))
					(spalte-nachbarn (zeile spalte &aux nachbarn)
					  (dotimes (i 9 nachbarn)
						(let ((x (aref tab i spalte)))
						  (unless (or (zerop x) (= i zeile))
							(push x nachbarn)))))
					(box-nachbarn (zeile spalte &aux nachbarn)
					  (let* ((zeile-min (* 3 (floor zeile 3)))    (zeile-max (+ zeile-min 3))
							 (spalte-min (* 3 (floor spalte 3))) (spalte-max (+ spalte-min 3)))
						(do ((r zeile-min (1+ r))) ((= r zeile-max) nachbarn)
						  (do ((c spalte-min (1+ c))) ((= c spalte-max))
							(let ((x (aref tab r c)))
							  (unless (or (zerop x) (= r zeile) (= c spalte))
								(push x nachbarn))))))))
			   (nset-difference
				(list 1 2 3 4 5 6 7 8 9)
				(nconc (zeile-nachbarn zeile spalte)
					   (spalte-nachbarn zeile spalte)
					   (box-nachbarn zeile spalte)))))
		   (löse-sudoku (tab &optional (zeile 0) (spalte 0) &aux (max 9))
			 "Löst ein Sudoku"
			 (cond ((= zeile max)
					tab)
				   ((= spalte max)
					(löse-sudoku tab (1+ zeile) 0))
				   ((plusp (aref tab zeile spalte))
					(löse-sudoku tab zeile (1+ spalte)))
				   (t (dolist (auswahl (möglichkeiten tab spalte zeile) (setf (aref tab zeile spalte) 0))
						(setf (aref tab zeile spalte) auswahl)
						(when (eq tab (löse-sudoku tab zeile (1+ spalte)))
						  (return tab))))))
		   (erstelle-sudokuliste (stream-name)
			 "Einleseformat: 1.Zeile: Sudokuname, 2.-10. Zeile je 9 Ziffern pro Zeile"
			 (let (sudokuliste)
			   (with-input-from-string (stream stream-name)
				 (dotimes (i 50 (nreverse sudokuliste))
				   (read-line stream nil)
				   (let ((sudoku (make-array '(9 9))))
					 (dotimes (x 9)
					   (let ((zeile (read-line stream nil)))
						 (dotimes (y 9)
						   (setf (aref sudoku x y) (parse-integer (subseq zeile y (1+ y)))))))
					 (push sudoku sudokuliste))))))
		   (löse-sudokuliste ()
			 "Löst alles Sudokus"
			 (let* ((datei (drakma:http-request "https://projecteuler.net/project/resources/p096_sudoku.txt"))
                    (sudoku-liste (erstelle-sudokuliste datei))
				   gelöste-sudoku)
			   (dolist (sudoku sudoku-liste (nreverse gelöste-sudoku))
				 (push (löse-sudoku sudoku) gelöste-sudoku))))
		   (addiere-sudoku-zahlen ()
			 "Addiert die Ziffern der Sudokus"
			 (let ((summe 0)
				   (gelöste-sudokus (löse-sudokuliste)))
			   (dolist (sudoku gelöste-sudokus summe)
				 (incf summe (+ (* 100 (aref sudoku 0 0)) (* 10 (aref sudoku 0 1)) (aref sudoku 0 2)))))))
	(addiere-sudoku-zahlen)))


(defun problem-97 ()
  (mod (1+ (* 28433 (expt 2 7830457))) (expt 10 10)))


(defun problem-99 ()
  (let ((datei (drakma:http-request "https://projecteuler.net/project/resources/p099_base_exp.txt")))
    (with-input-from-string (stream datei)
      (let ((max-zahl 1)
            (richtige-zeile))
        (do ((i (read-line stream nil)
                (read-line stream nil))
             (zeile 1 (1+ zeile)))
            ((null i)
             richtige-zeile)
          (let* ((basis (parse-integer (subseq i 0 (position #\, i))))
                 (exponent (parse-integer (subseq i (1+ (position #\, i)))))
                 (zahl (* (log basis) exponent)))
            (when (> zahl max-zahl)
              (setf max-zahl zahl
                    richtige-zeile zeile))))))))


(defun problem-102 ()
  (let ((datei (drakma:http-request "https://projecteuler.net/project/resources/p102_triangles.txt")))
    (with-input-from-string (stream datei)
      (let ((anzahl 0))
        (do ((i (read-line stream nil)
                (read-line stream nil)))
            ((null i)
             anzahl)
          (let* ((lst (mapcar #'parse-integer (string-aufteilen i)))
                 (ax (first lst))
                 (ay (second lst))
                 (bx (third lst))
                 (by (fourth lst))
                 (cx (fifth lst))
                 (cy (sixth lst))
                 (a (plusp (- (* ax by) (* ay bx))))
                 (b (plusp (- (* bx cy) (* by cx))))
                 (c (plusp (- (* cx ay) (* cy ax)))))
            (when (or (and a b c) (and (not a) (not b) (not c)))
              (incf anzahl))))))))


(defun problem-104 ()
  (flet ((pandigitalp (str)
		   (let ((p (search (sort str #'char<) "123456789")))
			 (if (and (integerp p) (zerop p)) t nil))))
	(do ((i 1 (1+ i))
		 (a 0 b)
		 (b 1 (+ a b))) ;; die Fibonacci-Zahl
		(nil)
	  (when (> i 2749)
		(when (and (pandigitalp (write-to-string (mod b 1000000000)))
				   (pandigitalp (subseq (write-to-string b) 0 9)))
		  (return-from problem-104 i))))))
  
  
(defun problem-125 ()
  (flet ((finde-summe (limit)
		   (let ((sqrt-limit (truncate (sqrt limit)))
				 liste)
			 (do ((i 1 (1+ i)))
				 ((> i (1- sqrt-limit))
				  (reduce #'+ (remove-duplicates liste)))
			   (let ((x (expt i 2)))
				 (do ((j (1+ i) (1+ j)))
					 ((> j (1- sqrt-limit)))
				   (incf x (expt j 2))
				   (when (>= x limit)
					 (return))
				   (when (palindromp x)
					 (push x liste))))))))
	(finde-summe 100000000)))


(defun problem-211 (&optional (max 64000000))
  (let ((sieb (make-array (+ max 1) :initial-element 1)))
	(1+ (loop for i from 2 to max
           for wert = (expt i 2)
           do (loop for j from i to max by i
                 do (incf (svref sieb j) wert))
           when (quadratzahlp (svref sieb i))
           sum i))))


(defun problem-243 (&optional (r 15499/94744))
  (flet ((finde-d (r)
		   (do ((p (nächste-primzahl) (nächste-primzahl p))
				(d 1 (* d p))
				(s 1 (* s (1- p))))
			   (nil)
			 (do ((i 2 (1+ i)))
				 ((> i p))
			   (when (< (/ (* s i) (1- (* d i))) r)
				 (return-from finde-d (* d i)))))))
	(finde-d r)))


;;; #######################
;;; # 3. Die Testumgebung #
;;; #######################


(defun teste-problem (fn wert)
  (format t "~&~A: " fn)
  (let ((ergebnis (funcall fn)))
    (lösche-alle-memos)
    (if (= wert ergebnis)
        (format t "... bestätigt~%")
        (cerror "~&FEHLER in ~A:  ~A statt ~A~%" fn ergebnis wert))))

(defun teste-alle-probleme ()
  (teste-problem #'problem-1 233168)
  (teste-problem #'problem-2 4613732)
  (teste-problem #'problem-3 6857)
  (teste-problem #'problem-4 906609)
  (teste-problem #'problem-5 232792560)
  (teste-problem #'problem-6 25164150)
  (teste-problem #'problem-7 104743)
  (teste-problem #'problem-8 23514624000)
  (teste-problem #'problem-9 31875000)
  (teste-problem #'problem-10 142913828922)
  (teste-problem #'problem-11 70600674)
  (teste-problem #'problem-12 76576500)
  (teste-problem #'problem-13 5537376230)
  (teste-problem #'problem-14 837799)
  (teste-problem #'problem-15 137846528820)
  (teste-problem #'problem-16 1366)
  (teste-problem #'problem-17 21124)
  (teste-problem #'problem-18 1074)
  (teste-problem #'problem-19 171)
  (teste-problem #'problem-20 648)
  (teste-problem #'problem-21 31626)
  (teste-problem #'problem-22 871198282)
  (teste-problem #'problem-23 4179871)
  (teste-problem #'problem-24 2783915460)
  (teste-problem #'problem-25 4782)
  (teste-problem #'problem-26 983)
  (teste-problem #'problem-27 -59231)
  (teste-problem #'problem-28 669171001)
  (teste-problem #'problem-29 9183)
  (teste-problem #'problem-30 443839)
  (teste-problem #'problem-31 73682)
  (teste-problem #'problem-32 45228)
  (teste-problem #'problem-33 100)
  (teste-problem #'problem-34 40730)
  (teste-problem #'problem-35 55)
  (teste-problem #'problem-36 872187)
  (teste-problem #'problem-37 748317)
  (teste-problem #'problem-38 932718654)
  (teste-problem #'problem-39 840)
  (teste-problem #'problem-40 210)
  (teste-problem #'problem-41 7652413)
  (teste-problem #'problem-42 162)
  (teste-problem #'problem-43 16695334890)
  (teste-problem #'problem-44 5482660)
  (teste-problem #'problem-45 1533776805)
  (teste-problem #'problem-46 5777)
  (teste-problem #'problem-47 134043)
  (teste-problem #'problem-48 9110846700)
  (teste-problem #'problem-49 296962999629)
  (teste-problem #'problem-50 997651)
  (teste-problem #'problem-51 121313)
  (teste-problem #'problem-52 142857)
  (teste-problem #'problem-53 4075)
  (teste-problem #'problem-54 376)
  (teste-problem #'problem-55 249)
  (teste-problem #'problem-56 972)
  (teste-problem #'problem-57 153)
  (teste-problem #'problem-58 26241)
  (teste-problem #'problem-59 107359)
  (teste-problem #'problem-60 26033)
  (teste-problem #'problem-61 28684)
  (teste-problem #'problem-62 127035954683)
  (teste-problem #'problem-63 49)
  (teste-problem #'problem-64 1322)
  (teste-problem #'problem-65 272)
  (teste-problem #'problem-66 661)
  (teste-problem #'problem-67 7273)
  (teste-problem #'problem-68 6531031914842725)
  (teste-problem #'problem-69 510510)
  (teste-problem #'problem-70 8319823)
  (teste-problem #'problem-71 428570)
  (teste-problem #'problem-72 303963552391)
  (teste-problem #'problem-73 7295372)
  (teste-problem #'problem-74 402)
  (teste-problem #'problem-75 161667)
  (teste-problem #'problem-76 190569291)
  (teste-problem #'problem-77 71)
  (teste-problem #'problem-79 73162890)
  (teste-problem #'problem-81 427337)
  (teste-problem #'problem-84 101524)
  (teste-problem #'problem-89 743)
  (teste-problem #'problem-92 8581146)
  (teste-problem #'problem-95 14316)
  (teste-problem #'problem-96 24702)
  (teste-problem #'problem-97 8739992577)
  (teste-problem #'problem-99 709)
  (teste-problem #'problem-102 228)
  (teste-problem #'problem-104 329468)
  (teste-problem #'problem-125 2906969179)
  (teste-problem #'problem-211 1922364685)
  (teste-problem #'problem-243 892371480))
  

  

