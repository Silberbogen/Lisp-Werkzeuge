;-*- coding: utf-8 -*-
;;;; Dateiname: euler.lisp
;;;; Beschreibung: Mögliche Lösungen zu den Aufgaben des Projekt Eulers, die
;;;;               ich in Common Lisp geschrieben habe.
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
;;;; (load "euler.lisp")
;;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;;; (compile-file "euler.lisp")


(load "~/lisp/hilfsroutinen.lisp")


;;; ----------------------------------------
;;;  Die Lösungen zu den einzelnen Aufgaben
;;; ----------------------------------------



(defun euler-1 ()
  "Vielfache von 3 und 5
Aufgabe 1
Wenn wir alle natürlichen Zahlen unter 10 auflisten, die Vielfache von 3 oder 5 sind, so erhalten wir 3, 5, 6 und 9. Die Summe dieser Vielfachen ist 23.
Finden Sie die Summe aller Vielfachen von 3 oder 5 unter 1000.
Antwort: 233168"
  (do ((i 1 (1+ i))
	   (summe 0))
	  ((= i 1000)
	   summe)
	(when (or (zerop (mod i 3)) (zerop (mod i 5)))
	  (incf summe i))))

		 


(defun euler-2 ()
  "Gerade Fibonacci-Zahlen
Aufgabe 2
Jeder neue Term in der Fibonacci-Reihe wird gebildet, indem die beiden vorherigen Zahlen addiert werden. Wenn man mit 1 und 2 beginnt, sind die ersten 10 Terme wie folgt:
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
Finden Sie die Summe aller geraden Terme der Fibonacci-Reihe, die 4 Millionen nicht überschreiten.
Antwort: 4613732"
  (do* ((i 1 (1+ i))
		(x (fibonacci-rang i) (fibonacci-rang i))
		(summe 0))
	   ((>= x 4000000)
		summe)
	(when (evenp x)
	  (incf summe x))))



(defun euler-3 (&optional (limit 600851475143))
  "Größter Primfaktor
Aufgabe 3
Die Primfaktoren von 13195 sind 5,7,13 und 29.
Welcher ist der größte Primfaktor der Zahl 600851475143?
Antwort: 6857"
  (apply #'max (faktorisiere limit)))



(defun euler-4 ()
  "Größtes Palindrom-Produkt
Aufgabe 4
Eine Palindrom-Zahl liest sich rückwärts so wie vorwärts. Das größte Palindrom, das ein Produkt von 2 zweistelligen Zahlen ist, ist 9009=91*99.
Finden sie das größte Palindrom, das das Produkt von 2 dreistelligen Zahlen ist.
Antwort: 906609"
  (do ((i 999 (1- i))
	   (max-palindrom 0))
	  ((< i 100)
	   max-palindrom)
	(do* ((j i (1- j)))
		 ((< j 100))
	  (let ((n (* i j)))
		(when (and (> n max-palindrom) (palindromp n))
		  (setf max-palindrom n))))))



(defun euler-5 ()
  "Kleinstes Vielfaches
Aufgabe 5
2520 ist die kleinste Zahl, die ohne Rest durch jede Zahl von 1 bis 10 teilbar ist.
Was ist die kleinste positive Zahl, die durch alle Zahlen von 1 bis 20 ohne Rest teilbar ist?
Antwort: 232792560"
  (lcm 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))



(defun euler-6 ()
  "Summe-Quadrat-Differenz
Aufgabe 6
Die Summe der Quadrate der ersten 10 natürlichen Zahlen ist
1² + 2² + ... + 10² = 385
Das Quadrat der Summe der ersten 10 natürlichen Zahlen ist
(1 + 2 + ... + 10)² = 55² = 3025
Somit ist die Differenz aus der Summe der Quadrate der ersten 10 natürlichen Zahlen und dem Quadrat der Summe 3025 - 385 = 2640.
Finden Sie die Differenz aus der Summe der Quadrate der ersten 100 natürlichen Zahlen und dem Quadrat der Summe.
Antwort: 25164150"
  (do ((i 1 (1+ i))
	   (summe 0)
	   (summe-der-quadrate 0))
	  ((> i 100)
	   (- (expt summe 2) summe-der-quadrate))
	(incf summe i)
	(incf summe-der-quadrate (expt i 2))))



(defun euler-7 ()
  "10001te Primzahl
Aufgabe 7
Wenn wir die ersten 6 Primzahlen auflisten: 2, 3, 5, 7, 11 und 13, können wir sehen, dass 13 die 6. Primzahl ist.
Welches ist die 10001te Primzahl?
Antwort: 104743"
  (primzahl-rang 10001))



(defun euler-8 (&optional (limit 13))
  "Größtes Produkt in einer Reihe
Aufgabe 8
Die vier aufeinanderfolgenden Ziffern in der 1000-stelligen Zahl, die das größte Produkt haben, sind 9 × 9 × 8 × 9 = 5832.
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
Finden Sie die dreizehn aufeinanderfolgenden Ziffern in der 1000-stelligen Zahl, die das größte Produkt haben. Was ist der Wert dieses Produkts?
Alte Antwort: 40824 (mit 5 Stellen)
Antwort: 23514624000"
  (labels ((wert (x s)
			 (digit-char-p (char s x)))
		   (berechne-produkt (index limit zahl &aux (wert 1))
			 (do ((i 0 (1+ i)))
				 ((= i limit)
				  wert)
			   (let ((m (wert (+ index i) zahl)))
				 (if (zerop m)
					 (return-from berechne-produkt 0)
					 (setf wert (* wert m)))))))
	(let* ((zahl "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")
		   (zahl-länge (length zahl)))
	  (do ((i 0 (1+ i))
		   (max-produkt 0))
		  ((= i (- zahl-länge limit))
		   max-produkt)
		(let ((produkt (berechne-produkt i limit zahl)))
		  (when (> produkt max-produkt)
			(setf max-produkt produkt)))))))



(defun euler-9 (&optional (wert 1000))
  "Spezielles pythagoreisches Tripel
Aufgabe 9
Ein pythagoreisches Tripel ist eine Menge von 3 natürlichen Zahlen, a < b < c, für die gilt:
a² + b² = c²
Beispiel: 3² + 4² = 9 + 16 = 25 = 5².
Es existiert genau ein pythagoreisches Tripel, für das a + b + c = 1000 gilt.
Finden Sie das Produkt abc.
Antwort: 31875000"
  (flet ((finde-tripel (wert)
		   (let ((halb-wert (/ wert 2)))
			 (do ((a 1 (1+ a)))
				 ((>= a halb-wert))
			   (do ((b a (1+ b)))
				   ((>= b halb-wert))
				 (let ((c (- wert a b)))
				   (when (and (> c b)
							  (= (+ (expt a 2) (expt b 2))
								 (expt c 2)))
					 (return-from finde-tripel (* a b c)))))))))
	(finde-tripel wert)))



(defun euler-10 ()
  "Summierung von Primzahlen
Aufgabe 10
Die Summe aller Primzahlen unter 10 ist 2 + 3 + 5 + 7 = 17.
Finden Sie die Summe aller Primzahlen unter 2 Millionen.
Antwort: 142913828922"
  (let ((summe 0))
	(dolist (i (sieb-des-eratosthenes 1999999) summe)
	  (incf summe i))))



(defun euler-11 ()
  "Größtes Produkt in einem Gitter
Aufgabe 11
Im 20x20 Gitter unten wurden vier Zahlen, die eine diagonale Linie bilden, rot markiert.
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10[26] 8 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95[63]94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17[78]78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35[14]00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
Das Produkt dieser Zahlen ist 26 x 63 x 78 x 14 = 1788696.
Was ist das größte Produkt von vier in irgendeiner Richtung (hoch, runter, links, rechts oder diagonal) benachbarten Zahlen im 20x20 Gitter?
Antwort: 70600674"
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



(defun euler-12 ()
  "Höchst teilbare Dreieckszahl
Aufgabe 12
Die Folge der Dreieckszahlen wird gebildet, indem die natürlichen Zahlen addiert werden. Somit ist die 7. Dreieckszahl 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. Die ersten zehn Glieder sind:
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
Lassen Sie uns die Teiler der ersten sieben Dreieckszahlen auflisten:
     1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28
Wir können sehen, dass 28 die erste Dreieckszahl mit mehr als 5 Teilern ist.
Was ist der Wert der ersten Dreieckszahl, die mehr als 500 Teiler hat?
Antwort: 76576500"
  (let ((n 0))
	(do ((i 1 (1+ i)))
		((> (length (sammle-divisoren n)) 500)
		 n)
	  (setf n (/ (* i (1+ i)) 2)))))


	  
(defun euler-13 ()
  "Große Summe
Aufgabe 13
Berechnen Sie die ersten zehn Ziffern der Summe der folgenden einhundert 50-stelligen Zahlen.
37107287533902102798797998220837590246510135740250
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
53503534226472524250874054075591789781264330331690
Antwort: 5537376230"
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
					   53503534226472524250874054075591789781264330331690))
		 (summe (apply #'+ zahlen))
		 (zeichenkette (princ-to-string summe))
		 x)
	(do ((i 9 (1- i)))
		((< i 0)
		 x)
	  (push (digit-char-p (char zeichenkette i)) x))))



(defun euler-14 ()
  "Längste Collatz-Folge
Aufgabe 14
Die folgende sich wiederholende Folge ist definiert für die Menge der natürlichen Zahlen:
n → n/2 (n ist gerade)
n → 3n + 1 (n ist ungerade)
Wenn wir die Regeln oben benutzen und mit 13 beginen, erhalten wir die folgende Folge:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
Es ist zu sehen, dass diese Folge (beginnend bei 13 und endend bei 1) 10 Terme enthält. Obwohl es bisher nicht bewiesen wurde (Collatz-Problem), wird vermutet, dass alle Anfangszahlen bei 1 enden.
Welche Anfangszahl unter 1 Million erzeugt die längste Folge?
HINWEIS: Sobald die Folge begonnen hat, dürfen die Terme auch 1 Million überschreiten.
Antwort: 837799"
  (do ((i 1 (1+ i))
	   (gesuchte-zahl 0)
	   (maximale-länge 0))
	  ((>= i 1000000)
	   gesuchte-zahl)
	(let ((länge (collatz-rang i)))
	  (when (> länge maximale-länge)
		(setf maximale-länge länge
			  gesuchte-zahl i)))))



(defun euler-15 ()
  "Gitter-Wege
Aufgabe 15
Beginnend in der linken, oberen Ecke eines 2x2 Gitters gibt es 6 Wege (ohne Zurückgehen) zur rechten, unteren Ecke.
Wie viele solche Wege gibt es in einem 20x20 Gitter?
Antwort: 137846528820"
  (let ((faktor40 (faktor 40))
		(faktor20 (faktor 20)))
	(/ faktor40 faktor20 faktor20)))



(defun euler-16 ()
  "Potenz-Quersumme
Aufgabe 16
2^15 = 32768, und die Quersumme ist 3 + 2 + 7 + 6 + 8 = 26.
Was ist die Quersumme der Zahl 2^1000?
Antwort: 1366"
  (addiere-ziffern (expt 2 1000)))



(defun euler-17 ()
  "Zahlen-Buchstaben-Anzahlen
Aufgabe 17
Wenn wir die Zahlen von 1 bis 5 als Worte ausschreiben (auf Englisch): one, two, three, four, five, dann haben wir insgesamt 3 + 3 + 5 + 4 + 4 = 19 Buchstaben benutzt.
Wie viele Buchstaben wären nötig, um alle Zahlen von 1 bis 1000 auf Englisch als Wörter zu schreiben?
HINWEIS: Zählen Sie weder Leerzeichen noch Bindestriche. Beispiel: 342 (three hundred and forty-two) enthält 23 Buchstaben und 115 (one hundred and fifteen) enthält 20 Buchstaben. Die Benutzung von \"and\" beim Ausschreiben der Zahlen ist Teil der britischen Schreibweise.
Antwort: 21124"
  (do ((i 1 (1+ i))
	   (anzahl 0)
	   (fehlende-and (* 3 9 99)))		; die britischen and
	  ((> i 1000)
	   (+ anzahl fehlende-and))
	(incf anzahl (zähle-buchstaben (format nil "~R" i)))))



(defun euler-18 ()
  "Maximale Wegsumme I
Aufgabe 18
Wenn wir an der Spitze des Dreiecks starten und uns zu angrenzenden Zahlen der Reihe darunter bewegen, ist die maximale Summe von der Spitze zum Boden 23.
3
7 4
2 4 6
8 5 9 3
Das ergibt 3 + 7 + 4 + 9 = 23.
Finden Sie die maximale Summe von der Spitze zum Boden des Dreiecks unten:
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
HINWEIS: Da nur 16384 Wege möglich sind, lässt sich die Aufgabe lösen, indem jeder Weg getestet wird. Allerdings ist Aufgabe 67 die selbe Aufgabe mit einem Dreieck mit einhundert Reihen; es kann nicht per Brute-Force gelöst werden und benötigt somit eine clevere Methode! ;o)
Antwort: 1074"
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



(defun euler-19 ()
  "Sonntage zählen
Aufgabe 19
Ihnen werden die folgenden Informationen gegeben, es steht Ihnen aber frei, selbst zu recherchieren.
    Der 1. Januar 1900 war ein Montag.
    September, April, Juni und November
    haben dreißig Tage.
    Alle anderen haben einunddreißig,
    Außer Februar,
    der achtundzwanzig hat.
    Und in Schaltjahren neunundzwanzig.
    Schaltjahr ist, wenn das Jahr ohne Rest durch 4 teilbar ist, aber nicht zu den Jahrhundertwenden, es sei denn, es ist durch 400 teilbar.
Wie viele Sonntage fielen im 20. Jahrhundert (1. Januar 1901 bis 31. Dezember 2000) auf den Ersten des Monats?
Antwort: 171"
  (flet ((sonntagp (tag monat jahr)
		   (= (wochentag tag monat jahr) 6)))
	(do ((jahr 1901 (1+ jahr))
		 (anzahl 0))
		((> jahr 2000)
		 anzahl)
	  (do ((monat 1 (1+ monat)))
		  ((> monat 12))
		(when (sonntagp 1 monat jahr)
		  (incf anzahl))))))



(defun euler-20 ()
  "Fakultäts-Quersumme
Aufgabe 20
n! bedeutet n × (n − 1) × ... × 3 × 2 × 1
Zum Beispiel, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
und die Summe der Ziffern in der Zahl 10! ist 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
Finden Sie die Quersumme von 100!
Antwort: 648"
  (addiere-ziffern (faktor 100)))



(defun euler-21 ()
  "Freundliche Zahlen
Aufgabe 21
Wir definieren d(n) als die Summe der echten Teiler von n (Zahlen kleiner als n, die n ohne Rest teilen).
Wenn d(a) = b und d(b) = a, wobei a ≠ b, dann sind a und b ein freundliches Zahlenpaar, und sowohl a als auch b werden freundliche Zahlen genannt.
Beispiele:
- Die echten Teiler von 220 sind 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 und 110; somit gilt d(220) = 284.
- Die echten Teiler von 284 sind 1, 2, 4, 71 und 142; also ist d(284) = 220.
Bilden Sie die Summe aller freundlichen Zahlen unter 10000.
Antwort: 31626"
  (do ((i 1 (1+ i))
	   (summe 0))
	  ((>= i 10000)
	   summe)
	(let ((befreundete-zahl (befreundete-zahl-p i)))
	  (when (and befreundete-zahl (/= befreundete-zahl i) (< befreundete-zahl 10000))
		(incf summe i)))))



(defun euler-22 ()
  "Namenswerte
Aufgabe 22
Benutzen Sie names.txt (Rechtsklick und \"Ziel speichern unter...\"), eine 46K Textdatei, die über fünftausend Vornamen enthält, und beginnen Sie, indem Sie die Namen alphabetisch sortieren. Berechnen Sie dann den alphabetischen Wert jedes Namens, und multiplizieren Sie diesen Wert mit der alphabetischen Position des Namens in der Liste, um den Namenswert zu erhalten.
Beispiel: Nachdem die Liste alphabetisch geordnet wurde, ist COLIN, was einen Namenswert von 3 + 15 + 12 + 9 + 14 = 53 hat, der 938. Name in der Liste. Also hätte COLIN einen Namenswert von 938 × 53 = 49714.
Welches ist die Summe aller Namenswerte der Datei?
Antwort: 871198282"
  (let* ((namensliste (erstelle-wortliste "~/lisp/p022_names.txt"))
		 (länge (length namensliste)))
	(do ((i 1 (1+ i))
		 (summe 0))
		((> i länge)
		 summe)
	  (incf summe (* i (alphabetischer-wert (pop namensliste)))))))
	


(defun euler-23 (&optional (maximum 28123))
  "Nicht-abundante Summen
Aufgabe 23
Eine perfekte Zahl ist eine Zahl, für die die Summe seiner echten Teiler gleich der Zahl selbst ist. Beispiel: Die Summe der echten Teiler von 28 ist 1 + 2 + 4 + 7 + 14 = 28, also ist 28 eine vollkommene Zahl.
Eine Zahl n wird unzulänglich genannt, wenn die Summe seiner echten Teiler kleiner ist als n, und sie wird abundant genannt, wenn diese Summe n übersteigt.
Da 12 die kleinste abundante Zahl ist, 1 + 2 + 3 + 4 + 6 = 16, ist die kleinste Zahl, die als die Summe von zwei abundanten Zahlen geschrieben werden kann, 24. Durch mathematische Analyse kann gezeigt werden, dass alle ganzen Zahlen, die größer als 28123 sind, als Summe von zwei abundanten Zahlen geschrieben werden können. Jedoch kann diese obere Begrenzung nicht durch Analyse weiter verringert werden, obwohl bekannt ist, dass die größte Zahl, die nicht als Summe von zwei abundanten Zahlen ausgedrückt werden kann, kleiner als diese Begrenzung ist.
Finden Sie die Summe aller natürlichen Zahlen, die nicht als die Summe von zwei abundanten Zahlen geschrieben werden können.
Antwort: 4179871"
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



(defun euler-24 ()
  "Lexikographische Permutationen
Aufgabe 24
Eine Permutation ist eine geordnete Anordnung von Objekten. Beispiel: 3124 ist eine mögliche Permutation der Ziffern 1, 2, 3 und 4. Wenn alle Permutationen numerisch oder alphabetisch geordnet sind, nennen wir es lexikographische Reihenfolge. Die lexikographischen Permutationen von 0, 1 und 2 sind:
012   021   102   120   201   210
Was ist die millionste lexikographische Permutation der Ziffern 0, 1, 2, 3, 4, 5, 6, 7, 8 und 9?
Antwort: 2783915460"
  (permutations-rang 1000000 '(0 1 2 3 4 5 6 7 8 9)))



(defun euler-25 ()
  "1000-stellige Fibonacci-Zahl
Aufgabe 25
Die Fibonacci-Reihe ist definiert durch den rekursiven Zusammenhang:
    Fn = Fn−1 + Fn−2, wobei F1 = 1 und F2 = 1.
Somit sind die ersten 12 Glieder:
    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144
Das 12. Glied, F12, ist das erste, das dreistellig ist.
Was ist das erste Glied der Fibonacci-Reihe, das 1000 Stellen hat?
Antwort: 4782"
  (do ((i 1 (1+ i)))
	  ((= 1000 (length (zahl->liste (fibonacci-rang i))))
	   i)))



(defun euler-26 ()
  "Kehrwert-Perioden
Aufgabe 26
Ein Stammbruch hat eine 1 im Zähler. Die dezimale Darstellung von Stammbrüchen mit den Nennern 2 bis 10 ist hier dargestellt:
    1/2	= 	0,5
    1/3	= 	0,(3)
    1/4	= 	0,25
    1/5	= 	0,2
    1/6	= 	0,1(6)
    1/7	= 	0,(142857)
    1/8	= 	0,125
    1/9	= 	0,(1)
    1/10	= 	0,1
Dabei bedeutet 0,1(6) 0,166666..., und hat eine 1-stellige Periode. Es ist zu sehen, dass 1/7 eine 6-stellige Periode hat.
Finden Sie den Wert von d < 1000, für den 1/d die längste Periode im Dezimalbruch hat.
Antwort: 983"
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



(defun euler-27 ()
  "Quadratische Primzahlen
Aufgabe 27
Euler veröffentlichte eine bemerkenswerte quadratische Formel:
n² + n + 41
Es stellt sich heraus, dass die Formel für die aufeinanderfolgenden Werte n = 0 bis 39 40 Primzahlen produziert.
Jedoch, wenn n = 40, ist 402 + 40 + 41 = 40(40 + 1) + 41 teilbar durch 41, und besonders wenn n = 41, ist 41² + 41 + 41 eindeutig teilbar durch 41.
Mithilfe von Computern wurde die unglaubliche Formel  n² − 79n + 1601 entdeckt, die 80 Primzahlen für die aufeinanderfolgenden Werte n = 0 bis 79 produziert. Das Produkt der Koeffizienten, -79 und 1601, ist -126479.
Wir betrachten quadratische Formeln der Form:
    n² + an + b, wobei |a| < 1000 und |b| < 1000
    wobei |n| der absolute Wert von n ist
    z.B. |11| = 11 and |−4| = 4
Finden Sie das Produkt der Koeffizienten, a und b, für die quadratische Formel, die die maximale Anzahl von Primzahlen für aufeinanderfolgende Werte von n produziert, beginnend mit n = 0.
Antwort: -59231"
  (labels ((primzahl-reihe (a b &optional (n 0))
			 (if (primzahlp (+ (expt n 2) (* a n) b))
				 (primzahl-reihe a b (1+ n))
				 n)))
	(let ((zahl1 0)
		  (zahl2 0)
		  (gezählte-primzahlen 0)
		  aktuelle-primzahlen)
	  (do ((a -999 (1+ a)))
		  ((zerop a)
		   (* zahl1 zahl2))
		(dolist (b (sieb-des-eratosthenes 999))
		  (when (> (setf aktuelle-primzahlen (primzahl-reihe a b))
				   gezählte-primzahlen)
			(setf zahl1 a
				  zahl2 b
				  gezählte-primzahlen aktuelle-primzahlen)))))))



(defun euler-28 ()
  "Zahlen-Spiralen-Diagonalen
Aufgabe 28
Wir beginnen mit der Zahl 1 und bewegen uns nach rechts im Uhrzeigersinn, so entsteht eine 5 x 5 Spirale wie folgt:
[21]  22   23   24  [25]
 20  [ 7]   8  [ 9]  10
 19    6  [ 1]   2   11
 18  [ 5]   4  [ 3]  12
[17]  16   15   14  [13]
Wir können sehen, dass die Summe der Diagonalen 101 beträgt.
Was ist die Summe beider Diagonalen in einer auf die gleiche Weise geformten 1001 x 1001 Spirale?
Antwort: 669171001"
  (let ((diagonal 1)
		(start 1)
		(inkrement 0)
		(gezählt 0))
	(do ((breite 3 (+ breite 2)))
		((> breite 1001)
		 diagonal)
	  (setf inkrement (1- breite)
			gezählt (* inkrement 4))
	  (incf diagonal (+ (* start 4) (* inkrement 10)))
	  (incf start gezählt))))		



(defun euler-29 ()
  "Verschiedene Potenzen
Aufgabe 29
Wir betrachten alle ganzzahligen Kombinationen ab mit 2 ≤ a ≤ 5 und 2 ≤ b ≤ 5:
    2²=4,  2³=8,   2^4=16,  2^5=32
    3²=9,  3³=27,  3^4=81,  3^5=243
    4²=16, 4³=64,  4^4=256, 4^5=1024
    5²=25, 5³=125, 5^4=625, 5^5=3125
Wenn wir diese numerisch ordnen und alle Wiederholungen entfernen, erhalten wir die folgenden 15 verschiedenen Terme:
4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
Wie viele verschiedene Terme sind in der Folge ab mit 2 ≤ a ≤ 100 und 2 ≤ b ≤ 100?
Antwort: 9183"
  (let (menge)
	(do ((a 2 (1+ a)))
		((> a 100)
		 (length menge))
	  (do ((b 2 (1+ b)))
		  ((> b 100))
		(pushnew (expt a b) menge)))))



(defun euler-30 ()
  "Fünfte Potenzen der Ziffern
Aufgabe 30
Überraschenderweise gibt es nur 3 Zahlen, die als Summe der vierten Potenzen ihrer Ziffern geschrieben werden können:
    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4
Da 1 = 1^4 keine Summe ist, ist es nicht enthalten.
Die Summe dieser Zahlen ist 1634 + 8208 + 9474 = 19316.
Finden Sie die Summe aller Zahlen, die als Summe der 5. Potenzen ihrer Ziffern geschrieben werden können.
Antwort: 443839"
  (labels ((expt-ziffern (n p &optional (sum 0))
			 (if (zerop n)
				 sum
				 (expt-ziffern (truncate (/ n 10)) p (+ sum (expt (rem n 10) p))))))
	(do ((i 2 (1+ i))
		 (summe 0))
		((>= i 200000)
		 summe)
	  (if (= i (expt-ziffern i 5))
		  (incf summe i)))))



(defun euler-31 (&optional (ziel 200))
  "Münzsummen
Aufgabe 31
In England besteht die Währung aus Pfund, £, und Pence, p, und es sind acht Münzen in Umlauf:
    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) und £2 (200p).
Es ist möglich, 2 £ auf die folgende Art darzustellen:
    1×1£ + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
Auf wie viele verschiedene Arten können 2 £ mit einer beliebigen Anzahl an Münzen dargestellt werden?
Antwort: 73682"
  (let ((münzen #(200 100 50 20 10 5 2 1)))
	(labels ((zähle-möglichkeiten (&optional (ziel 0)
											 (münztyp 0)
											 (möglichkeiten 0))
			   (if (= münztyp 7)
				   (incf möglichkeiten)
				   (do ((i münztyp (1+ i)))
					   ((> i 7)
						möglichkeiten)
					 (let ((hand (- ziel (svref münzen i))))
					   (when (zerop hand)
						 (incf möglichkeiten))
					   (when (plusp hand)
						 (incf möglichkeiten (zähle-möglichkeiten hand i 0))))))))
	  (zähle-möglichkeiten ziel))))



(defun euler-32 ()
  "Pandigitale Produkte
Aufgabe 32
Wir definieren, dass eine n-stellige Zahl pandigital ist, wenn in ihr alle Zahlen von 1 bis n genau einmal enthalten sind; Beispiel: Die 5-stellige Zahl 15234 ist mit den Ziffern 1 bis 5 pandigital.
Das Produkt 7254 ist ungewöhnlich, da die Gleichung 39 × 186 = 7254, die Multiplikand und Multiplikator enthält, ein Produkt mit den Ziffern 1 bis 9 pandigital ist.
Finden Sie die Summe aller Produkte, deren Multiplikand/Multiplikator/Produkt-Gleichung mit den Ziffern 1 bis 9 pandigital ist.
HINWEIS: Einige Produkte können auf mehr als nur eine Weise pandigital dargestellt werden, also stellen Sie sicher, dass Sie diese nur einmal zählen.
Antwort: 45228"
  (flet ((alle-pandigitalen-produkte ()
		   (let (liste
				 (produkt 0))
			 (do ((i 1 (1+ i)))
				 ((> i 99)
				  liste)
			   (do ((j 100 (1+ j)))
				   ((> j 9999))
				 (setf produkt (* i j))
				 (if (and (< produkt 9999)
						  (pandigitalp (append (zahl->liste i)
											   (zahl->liste j)
											   (zahl->liste produkt))))
					 (pushnew produkt liste)))))))
	(reduce #'+ (alle-pandigitalen-produkte))))



(defun euler-33 ()
  "Ziffern kürzende Brüche
Aufgabe 33
Der Bruch 49/98 ist ein seltsamer Bruch, denn ein unerfahrener Mathematiker, der den Bruch kürzen möchte, könnte fälschlicherweise glauben, dass 49/98 = 4/8, was richtig ist, erhalten wurde, indem die Neunen weggestrichen wurden.
Wir zählen Brüche wie 30/50 = 3/5 als triviale Beispiele.
Es gibt genau 4 nicht-triviale Beispiele solcher Brüche, die kleiner als 1 sind und sowohl einen zweistelligen Zähler als auch einen zweistelligen Nenner haben.
Wenn das Produkt dieser vier Brüche so weit wie möglich gekürzt ist, finden Sie den Wert des Nenners.
Antwort: 100"
  (let (liste)
    (do ((zähler 11 (1+ zähler)))
		((> zähler 98))
      (do ((nenner (1+ zähler) (1+ nenner)))
		  ((> nenner 99))
        (when (and (not (zerop (mod zähler 10)))
                   (not (zerop (mod nenner 10)))
                   (= (mod zähler 10)
                      (floor nenner 10))
                   (= (/ (/ (- zähler (mod zähler 10)) 10)
                         (mod nenner 10))
                      (/ zähler nenner)))
          (push (/ zähler nenner) liste))))
    (denominator (reduce #'* liste))))



(defun euler-34 ()
  "Ziffern-Fakultäten
Aufgabe 34
145 ist eine merkwürdige Zahl, denn 1! + 4! + 5! = 1 + 24 + 120 = 145.
Finden Sie die Summe aller Zahlen, die gleich der Summe der Fakultäten ihrer Ziffern sind.
HINWEIS: Da 1! = 1 und 2! = 2 keine Summen sind, werden sie nicht dazugezählt.
Antwort: 40730"
  (do ((i 3 (1+ i))
	   (summe 0))
	  ((> i 50000)
	   summe)
	(if (= i (reduce #'+ (mapcar #'faktor (zahl->liste i))))
		(incf summe i))))



(defun euler-35 ()
  "Kreisförmige Primzahlen
Aufgabe 35
Die Zahl 197 wird kreisförmige Primzahl genannt, denn alle Rotationen ihrer Ziffern: 197, 971 und 719, sind selbst Primzahlen.
Es gibt 13 solche Primzahlen unter 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, und 97.
Wie viele kreisförmige Primzahlen unter 1 Million gibt es?
Antwort: 55"
  (let (liste)
	(dolist (i (sieb-des-eratosthenes 999999) (length liste))
	  (when (kreisförmige-primzahl-p i) 
		(push i liste)))))



(defun euler-36 ()
  "Zweibasige Palindrome
Aufgabe 36
Die Dezimalzahl 585 = 10010010012 (binär) ist ein Palindrom zu beiden Basen.
Finden Sie die Summe aller Zahlen unter 1 Million, die sowohl zur Basis 10 als auch zur Basis 2 ein Palindrom sind.
\(Bitte beachten Sie, dass das Palindrom in keiner der Basen mit einer 0 beginnen darf.\)
Antwort: 872187"
  (flet ((zweibasiges-palindrom-p (zahl)
		   (and (palindromp zahl) (palindromp (format nil "~B" zahl)))))
	(do ((i 1 (1+ i))
		 (summe 0))
		((>= i 1000000)
		 summe)
	  (when (zweibasiges-palindrom-p i)
		(incf summe i)))))



(defun euler-37 ()
  "Trunkierbare Primzahlen
Aufgabe 37
Die Zahl 3797 hat eine interessante Eigenschaft. Neben der Tatsache, dass sie selbst eine Primzahl ist, kann man ständig Ziffern von links nach rechts entfernen, und trotzdem bleiben jedes Mal Primzahlen stehen: 3797, 797, 97 und 7. Auf die gleiche Weise können wir uns von rechts nach links arbeiten: 3797, 379, 37 und 7.
Finden Sie die Summe aller elf Primzahlen, die sowohl von links als auch von rechts trunkierbar sind.
HINWEIS: 2, 3, 5 und 7 zählen nicht als trunkierbare Primzahlen.
Antwort: 748317"
  (let ((summe 0)
		(anzahl 0))
	(dolist (i (sieb-des-eratosthenes 1000000) summe)
	  (when (trunkierbare-primzahl-p i)
		(incf anzahl)
		(incf summe i)))))



(defun euler-38 ()
  "Pandigitale Vielfache
Aufgabe 38
Nehmen Sie die Zahl 192 und multiplizieren Sie diese mit 1, 2 und 3:
    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576
Wenn wir diese Produkte miteinander verketten, erhalten wir die 1 bis 9 pandigitale Zahl 192384576. Wir nennen 192384576 das verkettete Produkt von 192 und (1,2,3).
Dasselbe kann erreicht werden, indem wir mit 9 beginnen und mit 1, 2, 3, 4 und 5 multiplizieren. Wir erhalten die pandigitale Zahl 918273645, die das verkettete Produkt von 9 und (1,2,3,4,5) ist.
Was ist die größte 9-stellige Zahl, die 1 bis 9 pandigital ist und als verkettetes Produkt einer Zahl und (1,2,...,n) mit n > 1 geschrieben werden kann?
Antwort: 932718654"
  (do ((i 9999 (1- i)))
	  ((< i 1)
	   nil)
	(if (pandigitalp (append (zahl->liste i) (zahl->liste (* 2 i))))
		(return (list i (* 2 i))))))



(defun euler-39 ()
  "Ganzzahlige rechtwinklige Dreiecke
Aufgabe 39
Wenn p der Umfang eines rechtwinkligen Dreiecks mit ganzzahligen Seitenlängen {a, b, c} ist, gibt es genau 3 Lösungen für p = 120.
{20,48,52}, {24,45,51}, {30,40,50}
Für welchen Wert p ≤ 1000 ist die Anzahl an Lösungen maximiert?
Antwort: 840"
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



(defun euler-40 ()
  "Champernowne's Konstante
Aufgabe 40
Ein irrationaler Dezimalbruch wird gebildet, indem die natürlichen Zahlen aneinandergereiht werden:
0.123456789101112131415161718192021...
Es ist zu sehen, dass die 12. Stelle des Bruchteils 1 ist.
Wenn dn für die nte Stelle des Bruchteils steht, finden Sie den Wert des folgenden Terms.
d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
Antwort: 210"
  (let ((digits (loop for i from 0 to 200000 append (zahl->liste i))))
	(apply #'* (mapcar #'(lambda (n) (nth n digits)) '(1 10 100 1000 10000 100000 1000000)))))



(defun euler-41 ()
  "Pandigitale Primzahl
Aufgabe 41
Wir bezeichnen eine n-stellige Zahl als pandigital, wenn in ihr alle Ziffern von 1 bis n genau einmal vorkommen. Beispiel: 2143 ist eine 4-stellige pandigitale Zahl und zudem eine Primzahl.
Welches ist die größte existierende n-stellige pandigitale Primzahl?
Antwort: 7652413"
  (let ((maximum 0))
	(dolist (i (sieb-des-eratosthenes 7654321) maximum)
	  (if (pandigitalp i)
		  (setf maximum i)))))



(defun euler-42 ()
  "Codierte Dreieckszahlen
Aufgabe 42
Der nte Term der Folge der Dreieckszahlen ist definiert durch tn = ½n(n+1), also sind die ersten 10 Dreieckszahlen:
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
Indem wir jeden Buchstaben eines Wortes zu einer Zahl entsprechend seiner Position im Alphabet umwandeln und diese addieren, erhalten wir einen Wortwert. Beispiel: Der Wortwert von SKY ist 19 + 11 + 25 = 55 = tn. Wenn der Wortwert eine Dreieckszahl ist, nennen wir das Wort ein Dreieckswort.
Benutzen Sie words.txt (Rechtsklick und 'Ziel speichern unter...'), eine 16K Datei, die nahezu 2000 englische Wörter enthält, und finden Sie die Anzahl von Dreieckswörtern.
Antwort: 162"
  (let* ((wortliste (erstelle-wortliste "~/lisp/p042_words.txt"))
		 (länge (length wortliste)))
	(do ((i 1 (1+ i))
		 (anzahl 0))
		((> i länge)
		 anzahl)
	  (when (dreieckszahlp (alphabetischer-wert (pop wortliste)))
		(incf anzahl)))))



(defun euler-43 ()
  "Teilstring-Teilbarkeit
Aufgabe 43
Die Zahl 1406357289 ist 0 bis 9 pandigital, da sie aus den Ziffern 0 bis 9 in einer beliebigen Reihenfolge besteht, aber sie hat auch eine interessante Teilstring-Teilbarkeits-Eigenschaft.
Sei d1 die 1. Ziffer, d2 die 2. Ziffer und so weiter. Auf diese Weise bemerken wir folgendes:
    d2d3d4=406 ist teilbar durch 2
    d3d4d5=063 ist teilbar durch 3
    d4d5d6=635 ist teilbar durch 5
    d5d6d7=357 ist teilbar durch 7
    d6d7d8=572 ist teilbar durch 11
    d7d8d9=728 ist teilbar durch 13
    d8d9d10=289 ist teilbar durch 17
Finden Sie die Summe aller 0 bis 9 pandigitalen Zahlen mit dieser Eigenschaft.
Antwort: 16695334890"
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


		
(defun euler-44 ()
  "Pentagonale Zahlen
Aufgabe 44
Pentagonale Zahlen werden mit der Formel Pn=n(3n-1)/2 gebildet. Die zehn ersten pentagonalen Zahlen sind:
1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
Es ist zu sehen, dass P4 + P7 = 22 + 70 = 92 = P8. Jedoch ist ihre Differenz, 70 - 22 = 48, nicht pentagonal.
Finden Sie das Paar pentagonaler Zahlen, Pj und Pk, für das ihre Summe und Differenz pentagonal sind und D = |Pj - Pk| minimiert ist; was ist der Wert von D?
Antwort: 5482660"
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
			  


(defun euler-45 ()
  "Dreiecks-, Fünfecks- und Sechseckszahlen
Aufgabe 45
Dreiecks-, Fünfecks- und Sechseckszahlen werden mit den folgenden Formeln gebildet:
Dreieck 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
Fünfeck 	  	Pn=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
Sechseck 	  	Hn=n(2n−1) 	  	1, 6, 15, 28, 45, ...
Es kann gezeigt werden, dass T285 = P265 = H143 = 40755.
Finden Sie die nächste Dreieckszahl, die auch Fünfecks- und Sechseckszahl ist.
Antwort: 1533776805"
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



(defun euler-46 ()
  "Goldbachs andere Vermutung
Aufgabe 46
Es wurde von Christian Goldbach vermutet, dass jede ungerade zusammengesetzte Zahl als Summe einer Primzahl und dem Doppelten einer Quadratzahl geschrieben werden kann.
9 = 7 + 2×1²
15 = 7 + 2×2²
21 = 3 + 2×3²
25 = 7 + 2×3²
27 = 19 + 2×2²
33 = 31 + 2×1²
Es stellte sich heraus, dass diese Vermutung falsch war.
Was ist die kleinste ungerade zusammengesetzte Zahl, die nicht als Summe einer Primzahl und dem Doppelten einer Quadratzahl geschrieben werden kann?
Antwort: 5777"
  (flet ((goldbach-aufgliedern (n)
		   (let ((max (isqrt n)))
			 (do ((i 1 (1+ i)))
				 ((> i max))
			   (let ((p (- n (* 2 (expt i 2)))))
				 (when (primzahlp p)
				   (return (list p i))))))))
	(do ((i 3 (+ 2 i)))
		((> i 10000))
	  (unless (primzahlp i)
		(when (null (goldbach-aufgliedern i))
		  (return i))))))



(defun euler-47 (&optional (fortlaufend 4) (zahl 645))
  "Bestimmte Primfaktoren
Aufgabe 47
Die ersten zwei aufeinanderfolgenden Zahlen, die jeweils zwei verschiedene Primfaktoren haben, sind:
14 = 2 × 7
15 = 3 × 5
Die ersten drei aufeinanderfolgenden Zahlen, die jeweils drei verschiedene Primfaktoren haben, sind:
644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.
Finden Sie die ersten vier aufeinanderfolgenden Zahlen, die jeweils vier verschiedene Primfaktoren haben. Was ist die erste dieser Zahlen?
Antwort: 134043"
  (if (= fortlaufend
         (length (remove-duplicates (faktorisiere zahl)))
         (length (remove-duplicates (faktorisiere (+ zahl 1))))
         (length (remove-duplicates (faktorisiere (+ zahl 2))))
         (length (remove-duplicates (faktorisiere (+ zahl 3)))))
      zahl
      (euler-47 fortlaufend (1+ zahl))))



(defun euler-48 ()
  "Selbst-Potenzen
Aufgabe 48
Die Reihe 1¹ + 2² + ... + 10¹° = 10405071317.
Finden Sie die letzten zehn Stellen der Reihe 1¹ + 2² + ... + 1000¹°°°.
Antwort: 9110846700"
  (do ((i 1 (1+ i))
	   (summe 0))
	  ((> i 1000)
	   (last (zahl->liste summe) 10))
	(incf summe (expt i i))))



(defun euler-49 ()
  "Primzahl-Permutationen
Aufgabe 49
Die arithmetische Folge 1487, 4817, 8147, in der jedes Glied um 3330 größer wird, ist durch zwei Dinge ungewöhnlich: 1. Jedes der Glieder ist eine Primzahl, 2. alle der vierstelligen Zahlen sind Permutationen voneinander.
Es gibt keine arithmetischen Folgen mit 1-, 2- oder 3-stelligen Primzahlen, die diese Eigenschaften erfüllen, aber es gibt eine weitere 4-stellige steigende Folge.
Welche 12-stellige Zahl entsteht, wenn Sie die drei Glieder dieser Folge aneinanderreihen?
Antwort: 296962999629"
  (do ((i 1489 (+ i 2)))
	  ((= i 4817)
	   nil)
	(let ((i2 (+ i 3330))
		  (i3 (+ i 6660)))
	  (when (and (primzahlp i) (primzahlp i2) (primzahlp i3))
		(let ((l1 (remove-duplicates (zahl->liste i)))
			  (l2 (remove-duplicates (zahl->liste i2)))
			  (l3 (remove-duplicates (zahl->liste i3))))
		  (when (and (subsetp l1 l2) (subsetp l1 l3) (subsetp l2 l1) (subsetp l3 l1))
			(return (list i (+ i 3330) (+ i 6660)))))))))



(defun euler-50 ()
  "Aufeinanderfolgende Primzahlsummen Aufgabe 50
Die Primzahl 41 kann als Summe von sechs aufeinanderfolgenden Primzahlen geschrieben werden:
   41 = 2 + 3 + 5 + 7 + 11 + 13
Dies ist die längste Summe von aufeinanderfolgenden Primzahlen, die zusammen eine Primzahl unter 100 bilden.
Die längste Summe aufeinanderfolgender Primzahlen, die zusammen eine Primzahl unter 1000 bilden, enthält 21 Glieder und entspricht der Zahl 953.
Welche Primzahl unter 1 Million kann als die Summe der meisten aufeinanderfolgenden Zahlen geschrieben werden?
Antwort: 997651"
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



(defun euler-51 ()
  "Primzahl-Ziffer-Ersetzungen
Aufgabe 51
Wenn wir die 1. Ziffer von *3 ersetzen, stellen wir fest, dass sechs der neun möglichen Werte, 13, 23, 43, 53, 73 und 83, Primzahlen sind.
Wenn wir die 3. und 4. Ziffer von 56**3 mit derselben Ziffer ersetzen, ist diese 5-stellige Zahl das erste Beispiel, das 7 Primzahlen unter den zehn gebildeten Zahlen sind: 56003, 56113, 56333, 56443, 56663, 56773, und 56993. Folglich ist 56003, die erste Zahl dieser Reihe, die kleinste Primzahl mit dieser Eigenschaft.
Finden Sie die kleinste Primzahl, die, wenn Teile der Zahl (nicht zwingend aufeinanderfolgende Ziffern) mit derselben Ziffer ersetzt werden, acht Primzahlen bildet.
Antwort: 121313"
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
			  (return-from euler-51 i)))))))



(defun euler-52 ()
  "Permutierte Vielfache
Aufgabe 52
Es ist zu sehen, dass die Zahl 125874 und ihr Doppeltes 251748 die selben Ziffern enthalten, nur in einer anderen Reihenfolge.
Finden Sie die kleinste natürliche Zahl x, für die 2x, 3x, 4x, 5x und 6x die selben Ziffern in beliebiger Reihenfolge enthalten.
Antwort: 142857"
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



(defun euler-53 (&optional (limit 100) (minimum 1000000))
  "Kombinatorische Auswahlen
Aufgabe 53
Es gibt genau 10 Möglichkeiten, drei aus fünf (12345) auszuwählen:
123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
In der Kombinatorik benutzen wir die Schreibweise 5C3 = 10.
Im Allgemeinen gilt
nCr = 	
n!
r!(n−r)!
	,wobei r ≤ n, n! = n×(n−1)×...×3×2×1, und 0! = 1.
Bis n = 23 überschreitet kein Wert 1 Million: 23C10 = 1144066.
Wie viele, nicht zwingend verschiedene, Werte von C(n,r) mit 1 ≤ n ≤ 100 sind größer als 1 Million?
Antwort: 4075"
  (labels ((möglichkeit (n r)
			 (/ (faktor n) (faktor r) (faktor (- n r)))))
	(do ((i 1 (1+ i))
		 (summe 0))
		((> i limit)
		 summe)
	  (dotimes (j i)
		(when (> (möglichkeit i j) minimum)
		  (incf summe))))))



(defun euler-54 ()
  "Poker-Blätter
Aufgabe 54
Im Kartenspiel Poker besteht ein Blatt aus 5 Karten, und die Blätter sind, vom schlechtesten zum besten, auf die folgende Weise geordnet:
    Höchste Karte: Karte mit dem höchsten Wert.
    Ein Paar: Zwei Karten mit dem gleichen Wert.
    Zwei Paare: Zwei verschiedene Paare.
    Drilling: Drei Karten mit dem gleichen Wert.
    Straße: Alle Karten haben aufeinanderfolgende Werte.
    Flush: Alle Karten haben die gleiche Farbe.
    Full House: Ein Drilling und ein Paar.
    Vierling: Vier Karten mit dem gleichen Wert.
    Straight Flush: Alle Karten haben aufeinanderfolgende Werte und die gleiche Farbe.
    Royal Flush: Zehn, Bube, Dame, König, Ass, in der gleichen Farbe.
Die Kartenwerte sind wie folgt sortiert:
2, 3, 4, 5, 6, 7, 8, 9, 10, Bube, Dame, König, Ass.
Wenn zwei Spieler Blätter mit dem selben Rang haben, gewinnt der Rang mit dem höchsten Kartenwert. Beispiel: Ein Achter-Paar schlägt ein Fünfer-Paar (siehe Beispiel 1 unten). Wenn diese Ränge auch gleichwertig sind, wenn beispielsweise beide Spieler ein Damen-Paar haben, zählt die höchste Karte des gesamten Blatts (siehe Beispiel 4 unten); wenn diese höchsten Karten gleich groß sind, zählen die nächsten höchsten Karten, und so weiter.
Beachten Sie die folgenden 5 Blätter, die an die zwei Spieler ausgeteilt wurden:
Blatt	 	Spieler 1	 	Spieler 2	 	Gewinner
1	 	5H 5C 6S 7S KD
Fünfer-Paar
	 	2C 3S 8S 8D TD
Achter-Paar
	 	Spieler 2
2	 	5D 8C 9S JS AC
Höchste Karte Ass
	 	2C 5C 7D 8S QH
Höchste Karte Dame
	 	Spieler 1
3	 	2D 9C AS AH AC
Drei Asse
	 	3D 6D 7D TD QD
Flush mit Karo
	 	Spieler 2
4	 	4D 6S 9H QH QC
Damen-Paar
Höchste Karte Neun
	 	3D 6D 7H QD QS
Damen-Paar
Höchste Karte Sieben
	 	Spieler 1
5	 	2H 2D 4C 4D 4S
Full House
Mit drei Vieren
	 	3C 3D 3S 9S 9D
Full House
mit drei Dreien
	 	Spieler 1
Die Datei poker.txt enthält eintausend zufällige Blätter, die den zwei Spielern ausgeteilt werden. Jede Zeile enthält zehn Karten (mit einer Leertaste getrennt): Die ersten 5 Karten sind die Karten von Spieler 1, die letzten 5 die von Spieler 2. Sie können voraussetzen, dass alle Blätter gültig sind (keine ungültigen Zeichen oder doppelte Karten), die Blätter von jedem Spieler sind in keiner bestimmten Reihenfolge, und bei jedem Blatt gibt es einen klaren Gewinner.
Bei wie vielen der Blätter gewinnt Spieler 1?
HINWEIS: Da projecteuler.net englisch ist, sind auch die Abkürzungen der Karten auf Englisch; Bei den Kartennamen bedeutet J(Jack) Bube, Q(Queen) Dame, K(King) König und A(Ace) Ass. Bei den Farben bedeutet C(Club) Kreuz, H(Heart) Herz, D(Diamond) Karo und S(Spade) Pik. Wichtig ist auch, dass die Karte Zehn nicht etwa 10, sondern T(Ten) lautet.
Antwort: 376"
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
				 (blatt-test (lambda () t) #'höchste-karte<))))
	
		   (erstelle-kartenliste (stream-name)
			 "Einleseformat: 10 durch Leerzeichen getrennte Daten je Zeile"
			 (let (kartenliste)
			   (with-open-file (stream stream-name)
				 (do ((i (read-line stream nil)
						 (read-line stream nil)))
					 ((null i)
					  (reverse kartenliste))
				   (push (string-aufteilen i) kartenliste))))))
	;; ---------- ENDE der Unterprogramme ----------------------------------------
	(let ((kartenliste (erstelle-kartenliste "/home/sascha/lisp/p054_poker.txt")))
	  (loop for blatt-paar in kartenliste
		 when (blatt< (nthcdr 5 blatt-paar) (butlast blatt-paar 5))
		 sum 1))))



(defun euler-55 ()
  "Lychrel-Zahlen Aufgabe 55
Wenn wir 47 nehmen, es umdrehen und beides addieren, erhalten wir 47 + 74 = 121, was ein Palindrom ist.
Nicht alle Zahlen produzieren so schnell Palindrome. Beispiel:
349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337
Wir sehen, dass 349 drei Wiederholungen brauchte, um bei einem Palindrom anzukommen.
Obwohl es bisher niemand bewiesen hat, wird vermutet, dass einige Zahlen, wie 196, niemals ein Palindrom produzieren. Eine Zahl, die niemals ein Palindrom durch den Umkehren-und-Addieren-Prozess bildet, wird Lychrel-Zahl genannt. Durch die Eigenarten dieser Zahlen und für den Zweck dieses Problems setzen wir voraus, dass eine Zahl Lychrel-Zahl ist, bis das Gegenteil bewiesen wird. Zusätzlich ist Ihnen gegeben, dass jede Zahl entweder nach weniger als 50 Wiederholugen ein Palindrom wird oder noch niemand bisher mit sämtlicher Rechenkraft ein Palindrom daraus bilden konnte. Tatsächlich ist 10677 die erste Zahl, die mehr als 50 Wiederholgen benötigt, um zu einem Palindrom zu führen: 4668731596684224866951378664 (53 Wiederholungen, 28-stellig).
Überraschenderweise gibt es Palindrom-Zahlen, die selbst Lychrel-Zahlen sind; das erste Beispiel ist 4994.
Wie viele Lychrel-Zahlen unter 10000 gibt es?
HINWEIS: Die Formulierung der Aufgabe auf projecteuler.net wurde am 24. April 2007 leicht verändert, um die besonderen Eigenschaften von Lychrel-Zahlen zu betonen.
Antwort: 249"
  (let ((anzahl 0))
	(dotimes (i 10000 anzahl)
	  (when (lychrel-zahl-p i)
		(incf anzahl)))))



(defun euler-56 ()
  "Quersumme mit Potenzial
Aufgabe 56
Ein Googol (10¹°°) ist eine gewaltige Zahl: eine 1 gefolgt von 100 Nullen; 100¹°° ist nahezu unvorstellbar groß, eine 1 gefolgt von 200 Nullen. Trotz ihrer Größe ist die Quersumme jeder der Zahlen nur 1.
Wir betrachten natürliche Zahlen der Form ab mit a, b < 100, was ist die maximale Quersumme?
Antwort: 972"
  (do ((a 1 (1+ a))
	   (maximum 0))
	  ((>= a 100)
	   maximum)
	(do ((b 1 (1+ b)))
		((>= b 100))
	  (let ((wert (ziffer-summe (expt a b))))
		(when (> wert maximum)
		  (setf maximum wert))))))



(defun euler-57 ()
  "Quadratwurzel-Konvergenzen
Aufgabe 57
Es ist möglich, die Quadratwurzel aus 2 als unendlichen Kettenbruch darzustellen.
√ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
Wenn wir die ersten vier Iterationen erweitern, erhalten wir:
1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
Die nächsten drei Erweiterungen sind 99/70, 239/169 und 577/408, aber die achte Erweiterung, 1393/985, ist das erste Beispiel, bei dem die Anzahl an Ziffern im Zähler größer als die Anzahl der Ziffern im Nenner ist.
In den ersten 1000 Erweiterungen, wie viele Brüche enthalten einen Zähler mit mehr Ziffern als im Nenner?
Antwort: 153"
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



(defun euler-58 ()
  "Spiral-Primzahlen
Aufgabe 58
Wir bilden eine quadratische Spirale mit einer Seitenlänge von 7, indem wir mit 1 beginnen und gegen den Uhrzeigersinn wie folgt weitergehen:
[37]  36   35   34   33   32  [31]
 38  [17]  16   15   14  [13]  30
 39   18  [ 5]   4  [ 3]  12   29
 40   19    6    1    2   11   28
 41   20  [ 7]   8    9   10   27
 42   21   22   23   24   25   26
[43]  44   45   46   47   48   49
Es ist interessant zu bemerken, dass die ungeraden Quadratzahlen auf der rechten unteren Diagonale liegen, aber was interessanter ist, ist, dass von den 13 Zahlen auf den Diagonalen 8 Primzahlen sind; das ist ein Schnitt von 8/13 ≈ 62%.
Wenn eine neue Ebene um die Spirale oben gelegt wird, entsteht eine quadratische Spirale mit der Seitenlänge 9. Wenn dieser Prozess fortgesetzt wird, was ist die Seitenlänge der quadratischen Spirale, dessen Anteil von Primzahlen auf beiden Diagonalen erstmals unter 10% fällt?
Antwort: 26241"
  (loop 
     for side = 0 then (if (= side 3) 0 (1+ side))
     for size = 3 then (if (zerop side) (+ 2 size) size)
     for no = 3 then (+ (1- size) no)
     count (primzahlp no) into primes
     count no into total
     until (and (zerop side) (<= (/ primes total) 1/10))
     finally (return (- size 2))))



(defun euler-59 ()
  "XOR-Entschlüsselung
Aufgabe 59
Jedes Zeichen auf einem Computer hat einen einzigartigen Code, und der bevorzugte Standard ist ASCII (American Standard Code for Information Interchange - Amerikanischer Standard Code für den Austausch von Informationen). Beispiel: Der Großbuchstabe A = 65, das Sternchen (*) = 42, und das kleine k =107.
Es ist eine moderne Verschlüsselungsmethode, eine Textdatei zu nehmen, die Bytes in ASCII zu konvertieren, und dann jedes Byte mit einem gegebenen Wert, der in einem geheimen Schlüssel steht, XOR zu rechnen. Der Vorteil der XOR-Funktion ist, dass wenn man den selben Schlüssel auf den verschlüsselten Text anwendet, der ursprüngliche Text entsteht; Beispiel: 65 XOR 42 = 107, dann ist 107 XOR 42 = 65.
Für unknackbare Verschlüsselung muss der Schlüssel die selbe Länge wie der Originaltext haben, und der Schlüssel muss aus zufälligen Bytes bestehen. Der Benutzer würde den verschlüsselten Text und den Schlüssel an verschiedenen Orten aufbewahren, und ohne beide 'Hälften' ist es unmöglich, die Nachricht zu entschlüsseln.
Unglücklicherweise ist diese Technik für die meisten Benutzer unpraktisch, also gibt es die abgewandelte Methode, ein Passwort als Schlüsssel zu verwenden. Wenn das Passwort kürzer als die Nachricht ist, was oft der Fall ist, wird der Schlüssel innerhalb der Nachricht zyklisch wiederholt. Das Gleichgewicht der Methode ist es, einen ausreichend langen Passwort-Schlüssel zur Sicherheit zu verwenden, der aber kurz genug ist, um sich merken zu lassen.
Ihnen wurde Ihre Aufgabe einfach gemacht, denn der Schlüssel besteht aus drei Kleinbuchstaben. Benutzen Sie cipher1.txt (Rechtsklick und 'Ziel speichern unter...'), eine Datei, die die verschlüsselten ASCII-Codes enthält, und das Wissen, dass der Originaltext allgemein verbreitete englische Wörter enthalten muss; entschlüsseln Sie die Nachricht und finden Sie die Summe aller ASCII-Werte im Original-Text.
Antwort: 107359"
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
				   (list summe entschlüsselt)
				   nil)))
		   (entschlüssle-alles ()
			 (let ((crypto-text (erstelle-zahlenliste "~/lisp/p059_cipher.txt")))
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



(defun euler-60 ()
  "Primzahl-Paar-Mengen
Aufgabe 60
Die Primzahlen 3, 7, 109 und 673 sind ziemlich erstaunlich. Wenn man beliebige 2 dieser Primzahlen nimmt und sie in beliebiger Reihenfolge verbindet, wird das Ergebnis auch immer eine Primzahl sein. Beispiel: Wenn wir 7 und 109 nehmen, sind sowohl 7109 als auch 1097 Primzahlen. Die Summe dieser 4 Primzahlen, 792, repräsentiert die kleinste mögliche Summe für eine Menge von 4 Primzahlen mit dieser Eigenschaft.
Finden Sie die kleinste mögliche Summe für eine Menge von 5 Primzahlen, für die zwei beliebige Primzahlen miteinander verbunden eine weitere Primzahl produzieren.
Antwort: 26033"
  )



(defun euler-61 ()
  "Zyklische figurierte Zahlen
Aufgabe 61
Dreiecks-, Quadrat-, Fünfecks-, Sechsecks-, Siebenecks- und Achteckszahlen sind alles figurierte (Polygonal-) Zahlen und werden mit den folgenden Formeln gebildet:
Dreieck 	  	P3,n=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
Quadrat 	  	P4,n=n2 	  	1, 4, 9, 16, 25, ...
Fünfeck 	  	P5,n=n(3n− 1)/2 	  	1, 5, 12, 22, 35, ...
Sechseck 	  	P6,n=n(2n−1) 	  	1, 6, 15, 28, 45, ...
Siebeneck 	  	P7,n=n(5n− 3)/2 	  	1, 7, 18, 34, 55, ...
Achteck 	  	P8,n=n(3n−2) 	  	1, 8, 21, 40, 65, ...
Die geordnete Menge von 3 vierstelligen Zahlen: 8128, 2882, 8281, hat drei interessante Eigenschaften.
    Die Menge ist zyklisch, denn die letzten beiden Ziffern jeder Zahl sind die ersten zwei Ziffern der nächsten Zahl (das gilt auch für die letzte und erste Zahl).
    Jeder Polygonal-Typ: Dreieck (P3,127=8128), Quadrat (P4,91=8281) und Fünfeck (P5,44=2882), ist durch eine verschiedene Zahl der Menge vertreten.
    Dies ist die einzige Menge mit vierstelligen Zahlen mit dieser Eigenschaft.
Finden Sie die Summe der einzigen geordneten Menge mit sechs zyklischen 4-stelligen Zahlen, von denen alle Polygonal-Typen (Dreieck, Quadrat, Fünfeck, Sechseck, Siebeneck und Achteck) durch verschiedene Zahlen vertreten sind.
Antwort: 28684"
  )



(defun euler-62 ()
  "Kubische Permutationen
Aufgabe 62
Die Kubikzahl 41063625 (345³) kann permutiert werden, um zwei andere Kubikzahlen zu produzieren: 56623104 (384³) and 66430125 (405³). In der Tat ist 41063625 die kleinste Kubikzahl, die genau drei Permutationen ihrer Ziffern hat, die auch Kubikzahlen sind.
Finden Sie die kleinste Kubikzahl, für die genau 5 Permutationen ihrer Ziffern selbst Kubikzahlen sind.
Antwort: 127035954683 (5027³)"
  (flet ((finde-permutationen (gesuchte-anzahl)
		   (let ((hash-tabelle (make-hash-table :test 'equal)))
			 (do ((i 1 (1+ i)))
				 (nil)
			   (let* ((ziffern (sortiere-ziffern (expt i 3)))
					  (neueintrag (push i (gethash ziffern hash-tabelle))))
				 (when (>= (length neueintrag) gesuchte-anzahl)
					 (return (expt (first (last neueintrag)) 3))))))))
	(finde-permutationen 5)))



(defun euler-63 ()
  "Ziffern-Anzahlen mit Potenzial
Aufgabe 63
Die 5-stellige Zahl 16807=75 ist auch eine fünfte Potenz. Auf die gleiche Weise ist die 9-stellige Zahl 134217728=89 eine neunte Potenz.
Wie viele n-stellige positive Zahlen gibt es, die auch eine nte Potenz sind?
Antwort: 49"
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



(defun euler-64 ()
  "Quadratwurzeln mit ungerader Periode
Aufgabe 64
...
Es ist zu sehen, dass sich die Folge wiederholt. Für Prägnanz benutzen wir die Schreibweise √23 = [4;(1,3,1,8)], um zu zeigen, dass der Block (1,3,1,8) sich unendlich wiederholt.
Die ersten zehn Kettenbruchdarstellungen von (irrationalen) Quadratwurzeln sind:
√2=[1;(2)], Periode=1
√3=[1;(1,2)], Periode=2
√5=[2;(4)], Periode=1
√6=[2;(2,4)], Periode=2
√7=[2;(1,1,1,4)], Periode=4
√8=[2;(1,4)], Periode=2
√10=[3;(6)], Periode=1
√11=[3;(3,6)], Periode=2
√12= [3;(2,6)], Periode=2
√13=[3;(1,1,1,1,6)], period=5
Genau 4 Kettenbrüche, für N ≤ 13, haben eine ungerade Periodenlänge.
Wie viele Kettenbrüche, für N ≤ 10000, haben eine ungerade Periode?
Antwort: 1322"
  )



(defun euler-65 ()
  "Annäherungen von e
Aufgabe 65
Die Quadratwurzel von 2 kann als unendlicher Kettenbruch geschrieben werden.
...
Somit ist die Folge der ersten 10 Annäherungen an √2:
1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
Es ist sehr überraschend, dass für die wichtige mathematische Konstante gilt:
e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
Die ersten zehn Terme der Folge der Annäherungen an e sind:
2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
Die Quersumme des Zählers der 10. Annäherung ist 1+4+5+7=17.
Finden Sie die Quersumme der 100. Annäherung des Kettenbruchs für e.
Antwort: 272"
  )



(defun euler-66 (&optional (limit 1000))
  "Diophantische Gleichung
Aufgabe 66
Betrachten wir die quadratische Diophantische Gleichung der Form:
   x² − Dy² = 1
Beispiel: Wenn D=13, dann ist die Lösung mit minimalem x 649² - 13×180² = 1.
Es kann angenommen werden, dass es keine positiven, ganzzahligen Lösungen gibt, wenn D eine Quadratzahl ist.
Wenn wir die Lösungen mit minimalem x für D = {2,3,5,6,7} suchen, erhalten wir folgendes:
3² - 2×2² = 1
2² - 3×1² = 1
9² - 5×4² = 1
5² - 6×2² = 1
8² - 7×3² = 1
Somit, wenn wir die Lösungen mit minimalem x für D ≤ 7 betrachten, erhalten wir bei D=5 das größte x.
Finden Sie den Wert von D ≤ 1000, für den das minimale x den größten Wert produziert.
Antwort: 661"
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


  
(defun euler-67 ()
  "Maximale Wegsumme II
Aufgabe 67
Wenn wir an der Spitze des Weges starten und uns zu angrenzenden Zahlen der Reihe darunter bewegen, ist die maximale Summe von der Spitze zum Boden 23.
3
7 4
2 4 6
8 5 9 3
Das ist 3 + 7 + 4 + 9 = 23.
Finden Sie die maximale Summe von der Spitze bis zum Boden in triangle.txt (Rechtsklick und 'Ziel speichern unter...'), einer 15K Textdatei, die ein Dreieck mit 100 Reihen enthält.
HINWEIS: Dies ist eine deutlich schwerere Version von Aufgabe 18. Es ist nicht möglich, jede einzelne Route auszuprobieren, um diese Aufgabe zu lösen, da es insgesamt 299 Routen gibt! Wenn man eine Billion (1012) Routen pro Sekunde berechnen könnte, würde es über 20 Milliarden Jahre dauern, um diese alle zu berechnen. Es gibt einen effizienten Algorithmus, um das Problem zu lösen. ;o)
Antwort: 7273"
  (flet ((erstelle-zahlenpyramide (stream-name)
		   (let (zahlenliste)
			 (with-open-file (stream stream-name)
			   (do ((i (read-line stream nil)
					   (read-line stream nil)))
				   ((null i)
					zahlenliste)
				 (push (mapcar #'parse-integer (string-aufteilen i))  zahlenliste))))))
	(first (route-dreieck (erstelle-zahlenpyramide "/home/sascha/lisp/p067_triangle.txt")))))



(defun euler-68 ()
  "Magischer 5er-Ring
Aufgabe 68
Betrachten Sie den folgenden 'magischen' 3er-Ring, ausgefüllt mit den Zahlen 1 bis 6, und jede Linie hat als Summe 9.
Wenn wir im Uhrzeigersinn arbeiten und bei der Gruppe mit dem kleinsten Außenknoten (4,3,2 in diesem Beispiel) beginnen, können wir jede Lösung eindeutig beschreiben. Beispiel: Die oben gezeigte Lösung kann durch die folgende Gruppe beschrieben werden: 4,3,2; 6,2,1; 5,1,3.
Es ist möglich, diesen Ring mit 4 verschiedenen Summen zu vervollständigen: 9, 10, 11 und 12. Es gibt insgesamt 8 Lösungen.
Summe	Lösungsmenge
9	4,2,3; 5,3,1; 6,1,2
9	4,3,2; 6,2,1; 5,1,3
10	2,3,5; 4,5,1; 6,1,3
10	2,5,3; 6,3,1; 4,1,5
11	1,4,6; 3,6,2; 5,2,4
11	1,6,4; 5,4,2; 3,2,6
12	1,5,6; 2,6,4; 3,4,5
12	1,6,5; 3,5,4; 2,4,6
Wenn wir jede Gruppe aneinanderketten, können wir 9-stellige Strings bilden; der maximale String für einen 3er-Ring ist 432621513.
Wir benutzen die Zahlen 1 bis 10 , und abhängig von der Anordnung ist es möglich, 16- und 17-stellige Strings zu bilden. Was ist der maximale 16-stellige String für einen 'magischen' 5er-Ring?
Antwort: 6531031914842725"
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



(defun euler-69 ()
  "Phi-Maximum
Aufgabe 69
Die Eulersche φ-Funktion φ(n) [manchmal auch Phi-Funktion genannt] wird benutzt, um die Anzahl von Zahlen kleiner als n zurückzugeben, die teilerfremd zu n sind. Beispiel: Da 1, 2, 4, 5, 7 und 8 kleiner als neun sind und teilerfremd zu neun sind, ist φ(9)=6.
n 	Teilerfremd 	φ(n) 	n/φ(n)
2 	1 	1 	2
3 	1,2 	2 	1.5
4 	1,3 	2 	2
5 	1,2,3,4 	4 	1.25
6 	1,5 	2 	3
7 	1,2,3,4,5,6 	6 	1.1666...
8 	1,3,5,7 	4 	2
9 	1,2,4,5,7,8 	6 	1.5
10 	1,3,7,9 	4 	2.5
Es ist zu sehen, dass n=6 ein Maximum von n/φ(n) für n ≤ 10 produziert.
Finden Sie den Wert von n ≤ 1,000,000, für den n/φ(n) ein Maximum ist.
Antwort: 510510"
  (flet ((finde-maximales-n (l)
		   (do ((p (nächste-primzahl) (nächste-primzahl p))
				(n 1))
			   (nil)
			 (when (> (* n p) l)
			   (return-from finde-maximales-n n))
			 (setf n (* n p)))))
	(finde-maximales-n 1000000)))



(defun euler-70 (&optional (limit (expt 10 7)))
  "Phi-Permutation
Aufgabe 70
Die Eulersche φ-Funktion φ(n) [manchmal auch Phi-Funktion genannt] wird benutzt, um die Anzahl von Zahlen kleiner als n zurückzugeben, die teilerfremd zu n sind. Beispiel: Da 1, 2, 4, 5, 7 und 8 kleiner als neun sind und teilerfremd zu neun sind, ist φ(9)=6.
Die Zahl 1 wird als teilerfremd zu jeder positiven Zahl angenommen, also ist φ(1)=1.
Interessanterweise ist φ(87109)=79180, und es ist zu sehen, dass 79180 eine Permutation von 87109 ist.
Finden Sie den Wert von n 1 < n < 107, für den φ(n) eine Permutation von n ist und der Bruch n/φ(n) ein Minimum bildet.
Antwort: 8319823"
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



(defun euler-71 ()
  "Geordnete Brüche
Aufgabe 71
Betrachten Sie den Bruch n/d, wobei n und d natürliche Zahlen sind. Wenn n<d und n und d teilerfremd sind, spricht man von einem gekürzten echten Bruch.
Wenn wir die Menge aller gekürzten echten Brüche mit d≤8 aufsteigend der Größe nach ordnen, erhalten wir:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
Es ist zu sehen, dass 2/5 der Bruch direkt links von 3/7 ist.
Indem Sie alle gekürzten echten Brüche für d ≤ 1000000 aufsteigend der Größe nach sortieren, finden Sie den Zähler des Bruches, der direkt links von 3/7 ist.
Antwort: 428570"
  (1- (truncate (* 3/7 (expt 10 6)))))



(defun euler-72 ()
  "Brüche zählen
Aufgabe 72
Betrachten Sie den Bruch n/d, wobei n und d natürliche Zahlen sind. Wenn n<d und n und d teilerfremd sind, spricht man von einem gekürzten echten Bruch.
Wenn wir die Menge aller gekürzten echten Brüche mit d≤8 aufsteigend der Größe nach ordnen, erhalten wir:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
Es ist zu sehen, dass in dieser Menge 21 Elemente enthalten sind.
Wieviele Elemente enthält die Menge der gekürzten echten Brüche mit d ≤ 1000000?
Antwort: 303963552391"
  (- (summe-der-farey-folge (expt 10 6)) 2))



(defun euler-73 ()
  "Brüche in einem Bereich zählen
Aufgabe 73
Betrachten Sie den Bruch n/d, wobei n und d natürliche Zahlen sind. Wenn n<d und n und d teilerfremd sind, spricht man von einem gekürzten echten Bruch.
Wenn wir die Menge aller gekürzten echten Brüche mit d≤8 aufsteigend der Größe nach ordnen, erhalten wir:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
Es ist zu sehen, dass 3 Brüche zwischen 1/3 und 1/2 liegen.
Wie viele Brüche liegen zwischen 1/3 und 1/2 in einer geordneten Menge von gekürzten echten Brüchen mit d ≤ 12000?
HINWEIS: Die obere Grenze wurde auf projecteuler.net kürzlich geändert.
Antwort: 7295372"
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



(defun euler-74 ()
  "Ketten von Ziffer-Fakultäten
Aufgabe 74
Die Zahl 145 ist bekannt für die Eigenschaft, dass die Summe der Fakultäten ihrer Ziffern 145 beträgt:
1! + 4! + 5! = 1 + 24 + 120 = 145
Vielleicht weniger bekannt ist die Zahl 169 dafür, dass sie die längste Kette von Zahlen bildet, die wieder zu 169 führen; es stellt sich heraus, dass nur drei solcher Schleifen existieren:
169 → 363601 → 1454 → 169
871 → 45361 → 871
872 → 45362 → 872
Es ist nicht schwer zu beweisen, dass JEDE Anfangszahl letztendlich in einer Schleife stecken bleibt. Beispiele:
69 → 363600 → 1454 → 169 → 363601 (→ 1454)
78 → 45360 → 871 → 45361 (→ 871)
540 → 145 (→ 145)
Wenn man mit 69 beginnt, erhält man eine Kette mit 5 wiederholungsfreien Werten, aber die längste wiederholungsfreie Kette mit einer Anfangszahl unter 1 Million enthält 60 Terme.
Wie viele Ketten mit einer Anfangszahl unter 1 Million enthalten genau 60 wiederholungsfreie Terme?
Antwort: 402"
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



(defun euler-75 ()
  "Einzigartige ganzzahlige rechtwinklige Dreiecke
Aufgabe 75
Es stellt sich heraus, dass 12 cm die kleinste Länge eines Seils ist, das durch Verbiegen auf genau eine Weise ein rechtwinkliges Dreieck mit ganzzahligen Seitenlängen formen kann, aber es gibt viele weitere Beispiele.
12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)
Im Gegensatz dazu gibt es Seillängen, wie 20 cm, die nicht zu einem rechtwinkligen Dreieck mit ganzzahligen Seitenlängen geformt werden können, und andere Seillängen erlauben mehr als eine Lösung; Beispiel: mit der Seillänge 120 cm ist es möglich, genau drei verschiedene rechtwinklige Dreiecke mit ganzzahligen Seitenlängen zu formen.
120 cm: (30,40,50), (20,48,52), (24,45,51)
Wenn L die Länge des Seils ist, für wie viele Werte von L ≤ 1500000 kann genau ein rechtwinkliges Dreieck mit ganzzahligen Seitenlängen geformt werden?
HINWEIS: Diese Aufgabe wurde auf projecteuler.net kürzlich verändert, bitte überprüfen Sie, ob sie die richtigen Parameter verwenden.
Antwort: 161667"
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



(defun euler-76 ()
  "Summen zählen
Aufgabe 76
Es ist möglich, 5 auf genau 6 Weisen als Summe zu schreiben:
4 + 1
3 + 2
3 + 1 + 1
2 + 2 + 1
2 + 1 + 1 + 1
1 + 1 + 1 + 1 + 1
Auf wie viele Weisen kann 100 als Summe von mindestens zwei positiven ganzen Zahlen geschrieben werden?
Antwort: 190569291"
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



(defun euler-77 ()
  "Primzahl-Summen
Aufgabe 77
Es ist möglich, zehn auf genau fünf Weisen als Summe von Primzahlen zu schreiben:
7 + 3
5 + 5
5 + 3 + 2
3 + 3 + 2 + 2
2 + 2 + 2 + 2 + 2
Was ist der erste Wert, der auf mehr als 5000 Weisen als Summe von Primzahlen geschrieben werden kann?
Antwort: 71"
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



(defun euler-78 ()
  "Münz-Zerlegungen
Aufgabe 78
p(n) steht für die Anzahl verschiedener Möglichkeiten, wie n Münzen in Haufen geteilt werden können. Beispiel: fünf Münzen können auf genau sieben Weisen in Haufen geteilt werden, also ist p(5)=7.
OOOOO
OOOO   O
OOO   OO
OOO   O   O
OO   OO   O
OO   O   O   O
O   O   O   O   O
Finden Sie den kleinsten Wert von n, für den p(n) durch 1 Million teilbar ist.
Antwort: 55374"
  )



(defun euler-79 ()
  "Passcode-Ableitung
Aufgabe 79
Eine bekannte Sicherheitsmethode, die beim Online-Banking benutzt wird, ist es, den Benutzer nach drei zufälligen Zeichen eines Passcodes zu fragen. Beispiel: wenn der Passcode 531278 ist, könnten sie nach dem 2., 3. und 5. Zeichen fragen; die erwartete Antwort wäre: 317.
Die Textdatei keylog.txt enthält fünfzig erfolgreiche Login-Versuche.
Gegeben ist, dass die drei Zeichen in ihrer Reihenfolge abgefragt werden; Analysieren Sie die Datei, um den kürzesten möglichen geheimen Passcode mit unbekannter Länge zu bestimmen.
Antwort: 73162890"
  (labels ((erstelle-keylogliste (stream-name)
			 (let (zahlenliste)
			   (with-open-file (stream stream-name)
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
	(let ((keylog (mapcar #'zahl->liste (erstelle-keylogliste "/home/sascha/lisp/p079_keylog.txt")))
		  (kandidaten '(1 2 3 4 5 6 7 8 9 0)))
	  (do ((i 1 (1+ i))
		   lösung)
		  ((> i 10)
		   (delete nil (reverse lösung)))
		(push (suche-ziffer kandidaten keylog) lösung)
		(setf kandidaten (delete (first lösung) kandidaten))
		(setf keylog (entferne-ziffer (first lösung) keylog))))))



(defun euler-81 ()
  "Wegsumme: 2 Richtungen
Aufgabe 81
In der 5 x 5 Matrix unten ist die minimale Wegsumme von der oberen linken zur unteren rechten Ecke, indem nur nach rechts und nach unten gezogen wird, dick und blau dargestellt und enspricht 2427.
131 673	234	103	18
201	96	342	965	150
630	803	746	422	111
537	699	497	121	956
805	732	524	37	331
Finden Sie die minimale Wegsumme in matrix.txt (Rechtsklick und 'Ziel speichern unter...'), einer 31K Textdatei, die eine 80 x 80 Matrix enthält, von der oberen linken zur unteren rechten Ecke, indem sich nur nach rechts und nach unten bewegt wird.
Antwort: 427337"
  (labels ((read-matrix (zeilen spalten dateiname)
		   (let ((matrix (make-array (list zeilen spalten)))
				 (*readtable* (copy-readtable)))
			 (set-syntax-from-char #\, #\Space)
			 (with-open-file (stream dateiname)
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
	(berechne-minimale-kosten 80 80 "~/lisp/p081_matrix.txt")))



(defun euler-84 ()
  "Monopoly-Wahrscheinlichkeiten
Aufgabe 84
Im Spiel Monopoly ist das Standard-Spielfeld wie folgt aufgebaut:
GO A1 CC1 A2 T1 R1 B1 CH1 B2 B3	JAIL
H2 	  	                        C1
T2 	  	                        U1
H1 	  	                        C2
CH3 	  	                    C3
R4 	  	                        R2
G3 	  	                        D1
CC3 	  	                    CC2
G2 	  	                        D2
G1 	  	                        D3
G2J F3 U2 F2 F1 R3 E3 E2 CH2 E1 FP
Ein Spieler beginnt auf dem GO-Feld (Los-Feld) und addiert die Werte von zwei 6-seitigen Würfeln, um die Zahl an Feldern zu erhalten, die er sich im Uhrzeigersinn fortbewegt. Ohne weitere Regeln würden wir erwarten, dass jedes Feld mit der gleichen Wahrscheinlichkeit betreten wird: 2.5%. Jedoch ändern G2J (Go To Jail - Gehe ins Gefängnis), CC (community chest - Gemeinschaftsfeld) und CH (chance - Ereignisfeld) diese Verteilung.
Zusätzlich zum Feld G2J und jeweils einer Karte in CC und CH, die den Spieler auffordert, direkt ins Gefängnis zu gehen, wird, wenn ein Spieler drei aufeinanderfolgende Pasche würft, sein dritter Wurf nicht ausgeführt. Stattdessen wandert er direkt ins Gefängnis.
Zu Beginn des Spiels werden die CC und CH Karten gemischt. Wenn ein Spieler auf CC oder CH landet, nimmt er eine Karte von der Spitze des jeweiligen Stapels und, nach Befolgen der Anweisungen, steckt sie zum Boden des Stapels zurück. Es gibt sechzehn Karten in jedem Stapel, aber für dieses Problem kümmern wir uns nur um Karten, die eine Bewegung erzwingen; alle Anweisungen, die keine Bewegung betreffen, können ignoriert werden, und der Spieler bleibt auf dem CC/CH Feld.
    Gemeinschaftsfeld (2/16 Karten):
        Gehe auf GO (Los)
        Gehe auf JAIL (Gefängnis)
    Ereignisfeld (10/16 Karten):
        Gehe auf GO (Los)
        Gehe auf JAIL (Gefängnis)
        Gehe auf C1
        Gehe auf E3
        Gehe auf H2
        Gehe auf R1
        Gehe zum nächsten R (railway company - Bahnhof)
        Gehe zum nächsten R
        Gehe zum nächsten U (utility company - Kraftwerk)
Das Herz dieses Problems betrifft die Wahrscheinlichkeit, ein bestimmtes Quadrat zu betreten. Dies ist die Wahrscheinlichkeit, nach einer Runde auf diesem Feld zu stehen. Aus diesem Grunde sollte klar sein, dass, mit der Ausnahme von G2J, für die die Wahrscheinlichkeit 0 ist, die CH-Felder die kleinste Wahrscheinlichkeiten haben werden, denn 5/8 der Karten erfordern ein Bewegen auf ein anderes Feld, und uns interessiert das Feld, auf dem der Spieler seinen Zug beendet. Wir machen keinen Unterschied zwischen 'Nur zu Besuch' und im Gefängnis sein, genau so wie wir die Regel ignorieren, dass ein Pasch nötig ist, um aus dem Gefängnis zu kommen; wir nehmen an, er zahlt die Kaution, um im nächsten Zug rauszukommen.
Wenn wir bei GO beginnen und alle Felder von 00 bis 39 nummerieren, können wir diese zweistelligen Zahlen verbinden, um Strings zu erhalten, die Mengen von Feldern darstellen.
Statistisch kann gezeigt werden, dass die drei häufigsten Felder in ihrer Reihenfolge JAIL (6.24%) = Feld 10, E3 (3.18%) = Feld 24 und GO (3.09%) = Feld 00 sind. Diese drei häufigsten Felder können als 6-stelliger String dargestellt werden: 102400,
Wenn statt zwei 6-seitigen Würfeln zwei 4-seitige Würfel benutzt werden, finden Sie den 6-stelligen String der drei häufigsten Felder."
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
  

(defun euler-89 ()
  "Römische Zahlen
Aufgabe 89
Die Regeln zum Schreiben von römischen Zahlen erlauben viele Möglichkeiten, eine Zahl zu schreiben (siehe Über römische Zahlen...). Jedoch gibt es immer einen 'besten' Weg, eine bestimmte Zahl zu schreiben.
Zum Beispiel sind im folgenden alle legitimen Wege, die Zahl sechzehn zu schreiben:
IIIIIIIIIIIIIIII
VIIIIIIIIIII
VVIIIIII
XIIIIII
VVVI
XVI
Das letzte Beispiel wird als am effizientesten betrachet, da es am wenigsten Zeichen benötigt.
Die 11K Textdatei roman.txt (Rechtsklick und 'Ziel speichern unter...'), enthält eintausend Zahlen, geschrieben als gültige, aber nicht zwingend minimale römische Zahlen; das heißt, sie sind in absteigenden Einheiten angeordnet und erfüllen die Subtraktionsregel (siehe Über römische Zahlen... für die definitiven Regeln für diese Aufgabe).
Finden Sie die Anzahl von gesparten Zeichen, wenn an jede davon in ihrer minimalen Form schreibt.
HINWEIS: Sie können annehmen, dass all die römischen Zahlen in der Datei nicht mehr als vier aufeinanderfolgende identische Einheiten enthalten."
  (labels ((erstelle-ziffernliste (stream-name)
			 (let (ziffernliste)
			   (with-open-file (stream stream-name)
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
	(let ((lang-liste (erstelle-ziffernliste "~/lisp/p089_roman.txt"))
		  (gespart 0))
	  (dolist (i lang-liste gespart)
		(incf gespart (berechne-wert i))))))



(defun euler-92 (&optional (limit 10000000))
  "Quadrat-Ziffern-Ketten
Aufgabe 92
Eine Zahlenkette wird gebildet, indem ständig die Quadrate der Ziffern einer Zahl addiert werden und diese eine neue Zahl bilden, bis diese schon vorher einmal aufgetreten ist.
Beispiel:
44 → 32 → 13 → 10 → 1 → 1
85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
Folglich wird jede Kette, die bei 1 oder 89 ankommt, in einer endlosen Schleife stecken bleiben. Was am erstaunlichsten ist, ist, dass JEDE Anfangszahl irgendwann bei 1 oder 89 ankommt.
Wie viele Anfangszahlen kleiner als 10 Millionen werden bei 89 ankommen?
Antwort: 8581146"
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



(defun euler-95 (&optional (max (expt 10 6)) &aux (max-halbe (truncate (/ max 2))))
  "Freundliche Ketten
Aufgabe 95
Die echten Teiler einer Zahl sind alle Teiler außer der Zahl selbst. Beispiel: Die echten Teiler von 28 sind 1, 2, 4, 7 und 14. Da die Summe dieser Teiler 28 beträgt, nennen wir sie eine perfekte Zahl.
Interessanterweise ist die Summe der echten Teiler von 220 284, und die Summe der echten Teiler von 284 ist 220, was eine Kette von zwei Zahlen bildet. Deshalb werden 220 und 284 als freundliches Paar bezeichnet.
Wahrscheinlich unbekannter sind längere Ketten. Beispiel: Wenn man mit 12496 beginnt, bilden wir eine Kette von 5 Zahlen:
12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)
Da die Kette zu ihrem Anfangspunkt zurückkehrt, wird sie freundliche Kette genannt.
Finden Sie das kleinste Glied der längsten freundlichen Kette, bei der kein Element 1 Million überschreitet.
Antwort: 14316"
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



(defun euler-96 ()
  "Su Doku
Aufgabe 96
Su Doku (japanisch für Zahlen platzieren) ist der Name eines bekannten Puzzle-Konzepts. Sein Ursprung ist unklar, aber Anerkennung muss Leonhard Euler gezollt werden, der eine ähnliche und deutlich schwierigere Puzzle-Idee namens Lateinische Quadrate erfand. Aber das Ziel bei Su-Doku-Rätseln ist es, alle Leerstellen (oder Nullen) in einem 9 mal 9 Gittter so zu ersetzen, dass jede Reihe, Spalte und 3 mal 3 Box jede der Ziffern von 1 bis 9 enthält. Unten ist ein Beispiel eines typischen Anfangs-Puzzle-Gitters und sein Lösungs-Gitter.
Ein gut konstruiertes Su-Doku-Rätsel hat eine eindeutige Lösung und kann mit Logik gelöst werden, obwohl es notwendig sein kann, 'Raten und testen'-Methoden zu benutzen, um Möglichkeiten zu eliminieren (es gibt viele umstrittene Meinungen dazu). Die Komplexität der Suche bestimmt die Schwierigkeit des Puzzle; das Beispiel oben wird als einfach betrachtet, da es durch direkte Ableitung gelöst werden kann.
Die 6K Textdatei sudoku.txt (Rechtsklick und 'Ziel speichern unter...'), enthält fünfig verschiedene Su-Doku-Puzzles in verschiedenen Schwierigkeiten, aber alle mit eindeutigen Lösungen (Das erste Puzzle in der Datei ist das Beispiel oben).
Lösen Sie alle fünfzig Puzzles und finden Sie die Summe der dreistelligen Zahlen, die in der linken oberen Ecke jedes Lösungs-Gitters zu finden sind; Beispiel: 483 ist die dreistellige Zahl, die in der linken oberen Ecke im Lösungs-Gitter oben gefunden werden kann.
Antwort: 24702"
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
			   (with-open-file (stream stream-name)
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
			 (let ((sudoku-liste (erstelle-sudokuliste "/home/sascha/lisp/p096_sudoku.txt"))
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



(defun euler-97 ()
  "Große Nicht-Mersenne-Primzahl
Aufgabe 97
Die erste bekannte Primzahl mit mehr als 1 Million Ziffern wurde 1999 entdeckt, und sie ist eine Mersenne-Primzahl in der Form 26972593−1; sie hat genau 2.098.960 Ziffern. Anschließend wurden andere Mersenne-Primzahlen der Form 2p−1 gefunden, die mehr Ziffern haben.
Jedoch wurde 2004 eine gewaltige Nicht-Mersenne-Primzahl gefunden, die 2.357.207 Stellen hat: 28433×27830457 + 1.
Finden Sie die letzten zehn Ziffern dieser Primzahl.
Antwort: 8739992577"
  (mod (1+ (* 28433 (expt 2 7830457))) (expt 10 10)))



(defun euler-99 (&optional (stream-name "/home/sascha/lisp/p099_base_exp.txt"))
  "Größte Potenz
Aufgabe 99
Es ist nicht schwer, zwei Zahlen in Indexform wie 211 und 37 ist nicht schwer, denn jeder Taschenrechner kann bestätigen, dass 211 = 2048 < 37 = 2187.
Jedoch wäre es deutlich schwieriger, zu bestätigen, dass 632382518061 > 519432525806 denn beide Zahlen bestehen aus über drei Millionen Ziffern.
Benutzen Sie base_exp.txt (Rechtsklick und 'Ziel speichern unter...'), eine 22K Textdatei, welche eintausend Zeilen mit einem Basis/Exponenten-Paar in jeder Zeile enthält, und bestimmen Sie, welche Zeile den größten numerischen Wert hat.
HINWEIS: Die ersten zwei Zeilen in der Datei repräsentieren die Zahlen von oben.
Antwort: 709"
  (with-open-file (stream stream-name)
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
				  richtige-zeile zeile)))))))



(defun euler-102 (&optional (stream-name "/home/sascha/lisp/p102_triangles.txt"))
  "Dreiecks-Beinhaltung
Aufgabe 102
Drei verschieden Punkte werden zufällig in einer Kartesischen Ebene platziert, für die -1000 ≤ x, y ≤ 1000, sodass ein Dreieck gebildet wird.
Betrachten Sie die folgenden zwei Dreiecke:
A(-340,495), B(-153,-910), C(835,-947)
X(-175,41), Y(-421,-714), Z(574,-645)
Es kann bestätigt werden, dass das Dreieck ABC den Ursprung enthält, wohingegen das Dreieck XYZ dies nicht tut.
Benutzen Sie triangles.txt (Rechtsklick und 'Ziel speichern unter...'), eine 27K Textdatei, welche die Koordinaten von eintausend 'zufälligen' Dreiecken enthält, und finden Sie die Anzahl von Dreiecken, für welche das Innere den Koordinatenursprung enthält.
Antwort: "
  (with-open-file (stream stream-name)
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
			(incf anzahl)))))))



(defun euler-104 ()
  "Pandigitale Fibonacci-Enden
Aufgabe 104
Die Finobacci-Folge ist durch die folgende rekursive Relation definiert:
    Fn = Fn−1 + Fn−2, wobei F1 = 1 und F2 = 1.
Es stellt sich heraus, dass F541, welches aus 113 Stellen besteht, die erste Fibonaccizahl ist, für welche die letzten 9 Ziffern 1-9 pandigital sind (enthalten alle Ziffern von 1 bis 9, aber nicht zwingend geordnet). Und F2749, welche aus 575 Ziffern besteht, ist die erste Fibonaccizahl, für die die ersten 9 Ziffern 1-9 pandigital sind.
Gegeben, dass Fk die erste Fibonaccizahl ist, für welche die ersten 9 Ziffern UND die letzten 9 Ziffern 1-9 pandigital sind, finden Sie k.
Antwort: "
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
		  (return-from euler-104 i))))))
  
  
  
(defun euler-125 ()
  "Palindromic sums
Problem 125
The palindromic number 595 is interesting because it can be written as the sum of consecutive squares: 62 + 72 + 82 + 92 + 102 + 112 + 122.
There are exactly eleven palindromes below one-thousand that can be written as consecutive square sums, and the sum of these palindromes is 4164. Note that 1 = 02 + 12 has not been included as this problem is concerned with the squares of positive integers.
Find the sum of all the numbers less than 108 that are both palindromic and can be written as the sum of consecutive squares.
Antwort: 2906969179"
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



(defun euler-243 (&optional (r 15499/94744))
  "Resilience
Problem 243
A positive fraction whose numerator is less than its denominator is called a proper fraction.
For any denominator, d, there will be d−1 proper fractions; for example, with d = 12:
1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .
We shall call a fraction that cannot be cancelled down a resilient fraction.
Furthermore we shall define the resilience of a denominator, R(d), to be the ratio of its proper fractions that are resilient; for example, R(12) = 4/11 .
In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10 .
Find the smallest denominator d, having a resilience R(d) < 15499/94744 .
Answer:	892371480"
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


