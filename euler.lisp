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
  (let ((summe 0))
	(do ((i 1 (1+ i)))
		((>= i 1000)
		 summe)
	  (when (or (zerop (mod i 3)) (zerop (mod i 5)))
		(incf summe i)))))

		 


(defun euler-2 ()
  "Gerade Fibonacci-Zahlen
Aufgabe 2
Jeder neue Term in der Fibonacci-Reihe wird gebildet, indem die beiden vorherigen Zahlen addiert werden. Wenn man mit 1 und 2 beginnt, sind die ersten 10 Terme wie folgt:
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
Finden Sie die Summe aller geraden Terme der Fibonacci-Reihe, die 4 Millionen nicht überschreiten.
Antwort: 4613732"
  (let ((summe 0))
	(do* ((i 1 (1+ i))
		  (x (fibonacci-folge i) (fibonacci-folge i)))
		 ((>= x 4000000)
		  summe)
	  (when (evenp x)
		(incf summe x)))))



(defun euler-3 ()
  "Größter Primfaktor
Aufgabe 3
Die Primfaktoren von 13195 sind 5,7,13 und 29.
Welcher ist der größte Primfaktor der Zahl 600851475143?
Antwort: 6857"
  (let ((x (faktorisiere 600851475143)))
	(apply #'max x)))



(defun euler-4 ()
  "Größtes Palindrom-Produkt
Aufgabe 4
Eine Palindrom-Zahl liest sich rückwärts so wie vorwärts. Das größte Palindrom, das ein Produkt von 2 zweistelligen Zahlen ist, ist 9009=91*99.
Finden sie das größte Palindrom, das das Produkt von 2 dreistelligen Zahlen ist.
Antwort: 906609"
  (let ((n 0) ; die zu probierende Zahl
		(x 0)) ; die gesuchte Zahl
	(do ((i 999 (1- i)))
		((< i 100)
		 x)
	  (do ((j i (1- j)))
		  ((< j 100))
		(setf n (* i j))
		(when (and (> n x) (palindromp n))
		  (setf x n))))))



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
  (let ((summe 0) ; enthält am Ende (1+2+...+100)
		(quadratsumme 0) ; enthält am Ende 1²+2²+...+100²
		(summenquadrat 0)) ; enthält am Ende (1+2+...+100)²
	(do ((i 1 (1+ i)))
		((> i 100)
		 (setf summenquadrat (expt summe 2))
		 (- summenquadrat quadratsumme))
	  (incf summe i)
	  (incf quadratsumme (expt i 2)))))



(defun euler-7 ()
  "10001te Primzahl
Aufgabe 7
Wenn wir die ersten 6 Primzahlen auflisten: 2, 3, 5, 7, 11 und 13, können wir sehen, dass 13 die 6. Primzahl ist.
Welches ist die 10001te Primzahl?
Antwort: 104743"
  (primzahl-rang 10001))



(defun euler-8 ()
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
Antwort: 23514624000"
  (flet ((wert (x s) (digit-char-p (char s x))))
	(let* ((zahl "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")
		   (n (* (wert 0 zahl) (wert 1 zahl) (wert 2 zahl) (wert 3 zahl) (wert 4 zahl))) ; das temporäre Produkt
		   (x n) ; das gesuchte Produkt
		   (zlength (length zahl))) ; die Länge von zahl
	  (do ((i 5 (1+ i)))
		  ((>= i zlength)
		   x)
		(let ((l (wert (- i 5) zahl)) ; zu entfernende Ziffer
			  (m (wert i zahl))) ; hinzuzufügende Ziffer
		  (unless (zerop l)
			(setf n (/ n l)))
		  (unless (zerop m)
			(setf n (* n m)))
		  (when (> n x)
			(setf x n)))))))



(defun euler-9 ()
  "Spezielles pythagoreisches Tripel
Aufgabe 9
Ein pythagoreisches Tripel ist eine Menge von 3 natürlichen Zahlen, a < b < c, für die gilt:
a² + b² = c²
Beispiel: 3² + 4² = 9 + 16 = 25 = 5².
Es existiert genau ein pythagoreisches Tripel, für das a + b + c = 1000 gilt.
Finden Sie das Produkt abc.
Antwort: 31875000"
  (let ((c 0))
	(do ((a 1 (1+ a)))
		((>= a 500))
	  (do ((b a (1+ b)))
		  ((>= b 500))
		(setf c (- 1000 a b))
		(when (and (> c b)
				   (= (+ (expt a 2) (expt b 2))
					  (expt c 2)))
		  (return-from euler-9 (list a b c)))))))



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
  (let ((zahlen nil)
		(n 0)
		(maximum 0))
	(setf zahlen
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
	(labels ((zelle (x y) (aref zahlen x y))
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
			 (test-n ()
			   (when (> n maximum)
				 (setf maximum n))))
	  (do ((i 0 (1+ i)))
		  ((>= i 20)
		   maximum)
		(do ((j 0 (1+ j)))
			((>= j 20))
		  (when (<= i 16)
			(setf n (sequenz-horizontal i j))
			(test-n))
		  (when (<= j 16)
			(setf n (sequenz-vertikal i j))
			(test-n))
		  (when (and (<= i 16) (<= j 16))
			(setf n (sequenz-lu-ro i j))
			(test-n)
			(setf n (sequenz-lo-ru i j))
			(test-n)))))))



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
  (let ((zahlen (list 37107287533902102798797998220837590246510135740250 
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
		(summe 0)
		(zeichenkette nil)
		(x nil))
	(setf summe (apply #'+ zahlen))
	(setf zeichenkette (princ-to-string summe))
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
  (let ((x 0)				  ; gesuchte Zahl
		(länge 0)			  ; Länge der Collatz-Sequenz
		(n nil))			  ; Zwischenspeicher der aktuellen Sequenz
	(do ((i 1 (1+ i)))
		((>= i 1000000)
		 x)
	  (setf n (collatz-sequenz i))
	  (when (> (length n) länge)
		(setf länge (length n)
			  x (first n))))))



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
  (let ((x 0)
		(fehlende-and (* 3 9 99)))		; CL kann das von Haus aus, aber auf Amerikanisch. Wir müssen die fehlenden and des Britischen ergänzen
	(do ((i 1 (1+ i)))
		((> i 1000)
		 (+ x fehlende-and))
	  (incf x (zähle-buchstaben (format nil "~R" i))))))



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
  (let ((anzahl 0))
	(do ((jahr 1901 (1+ jahr)))
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
  (let ((summe 0)
		(befreundete-zahl nil))
	(do ((i 1 (1+ i)))
		((>= i 10000)
		 summe)
	  (setf befreundete-zahl (befreundete-zahl-p i))
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
		 (länge (length namensliste))
		 (summe 0))
	(do ((i 1 (1+ i)))
		((> i länge)
		 summe)
	  (incf summe (* i (alphabetischer-wert (pop namensliste)))))))
	


(defun euler-23 ()
  "Nicht-abundante Summen
Aufgabe 23
Eine perfekte Zahl ist eine Zahl, für die die Summe seiner echten Teiler gleich der Zahl selbst ist. Beispiel: Die Summe der echten Teiler von 28 ist 1 + 2 + 4 + 7 + 14 = 28, also ist 28 eine vollkommene Zahl.
Eine Zahl n wird unzulänglich genannt, wenn die Summe seiner echten Teiler kleiner ist als n, und sie wird abundant genannt, wenn diese Summe n übersteigt.
Da 12 die kleinste abundante Zahl ist, 1 + 2 + 3 + 4 + 6 = 16, ist die kleinste Zahl, die als die Summe von zwei abundanten Zahlen geschrieben werden kann, 24. Durch mathematische Analyse kann gezeigt werden, dass alle ganzen Zahlen, die größer als 28123 sind, als Summe von zwei abundanten Zahlen geschrieben werden können. Jedoch kann diese obere Begrenzung nicht durch Analyse weiter verringert werden, obwohl bekannt ist, dass die größte Zahl, die nicht als Summe von zwei abundanten Zahlen ausgedrückt werden kann, kleiner als diese Begrenzung ist.
Finden Sie die Summe aller natürlichen Zahlen, die nicht als die Summe von zwei abundanten Zahlen geschrieben werden können.
Antwort: 4179871"
  (let ((liste-abundanter-zahlen nil)
		(summe 0)
		(maximum 28123)) ; vereinbartes Maximum
	;; Erstellen einer Liste aller abundanter Zahlen und der Summe aller Zahlen von 1 bis 28123
	(do ((i 1 (1+ i)))
		((> i maximum)
		 (setf liste-abundanter-zahlen (reverse liste-abundanter-zahlen)))
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
	  ((= 1000 (length (zahl->liste (fibonacci-folge i))))
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
	(let ((maximallänge 0)
		  (maximales-i 0)
		  (länge-i 0))
	  (do ((i 1 (1+ i)))
		  ((= i 1000)
		   maximales-i)
		(when (and (= 1 (gcd i 10))
				   (< maximallänge (setf länge-i (kehrwert-zyklus-länge i))))
		  (setf maximallänge länge-i
				maximales-i i))))))



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
		  (aktuelle-primzahlen))
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
  (let ((menge nil))
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
  (let ((summe 0))
	(do ((i 2 (1+ i)))
		((>= i 200000)
		 summe)
	  (if (= i (expt-ziffern i 5))
		  (incf summe i)))))



(defun euler-31 (&optional (ziel 200) (münztyp 0) (möglichkeiten 0))
  "Münzsummen
Aufgabe 31
In England besteht die Währung aus Pfund, £, und Pence, p, und es sind acht Münzen in Umlauf:
    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) und £2 (200p).
Es ist möglich, 2 £ auf die folgende Art darzustellen:
    1×1£ + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
Auf wie viele verschiedene Arten können 2 £ mit einer beliebigen Anzahl an Münzen dargestellt werden?
Antwort: 73682"
  (let ((münzen #(200 100 50 20 10 5 2 1)))
	(if (= münztyp 7)
		(incf möglichkeiten)
		(do ((i münztyp (1+ i)))
			((> i 7)
			 möglichkeiten)
		  (let ((hand (- ziel (svref münzen i))))
			(when (zerop hand)
			  (incf möglichkeiten))
			(when (plusp hand)
			  (incf möglichkeiten (euler-31 hand i 0))))))))



(defun euler-32 ()
  "Pandigitale Produkte
Aufgabe 32
Wir definieren, dass eine n-stellige Zahl pandigital ist, wenn in ihr alle Zahlen von 1 bis n genau einmal enthalten sind; Beispiel: Die 5-stellige Zahl 15234 ist mit den Ziffern 1 bis 5 pandigital.
Das Produkt 7254 ist ungewöhnlich, da die Gleichung 39 × 186 = 7254, die Multiplikand und Multiplikator enthält, ein Produkt mit den Ziffern 1 bis 9 pandigital ist.
Finden Sie die Summe aller Produkte, deren Multiplikand/Multiplikator/Produkt-Gleichung mit den Ziffern 1 bis 9 pandigital ist.
HINWEIS: Einige Produkte können auf mehr als nur eine Weise pandigital dargestellt werden, also stellen Sie sicher, dass Sie diese nur einmal zählen.
Antwort: 45228"
  (flet ((alle-pandigitalen-produkte ()
		   (let ((liste nil)
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
  (let ((liste '()))
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
  (let ((summe 0))
	(do ((i 3 (1+ i)))
		((> i 50000)
		 summe)
	  (if (= i (reduce #'+ (mapcar #'faktor (zahl->liste i))))
		  (incf summe i)))))



(defun euler-35 ()
  "Kreisförmige Primzahlen
Aufgabe 35
Die Zahl 197 wird kreisförmige Primzahl genannt, denn alle Rotationen ihrer Ziffern: 197, 971 und 719, sind selbst Primzahlen.
Es gibt 13 solche Primzahlen unter 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, und 97.
Wie viele kreisförmige Primzahlen unter 1 Million gibt es?
Antwort: 55"
  (let ((liste nil))
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
	(let ((summe 0))
	  (do ((i 1 (1+ i)))
		  ((>= i 1000000)
		   summe)
		(when (zweibasiges-palindrom-p i)
		  (incf summe i))))))



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
  (let ((max-anzahl 0)
		(max-p 0))
	(do ((p 12 (+ p 2)))
		((> p 1000)
		 max-p)
	  (let ((anzahl 0))
		(do ((a 1 (1+ a)))
			((> a (/ p 3)))
		  (let ((a-quadrat (expt a 2)))
			(do ((b a (1+ b)))
				((> b (/ (- p a) 2)))
			  (let ((c (- p a b)))
				(when (= (+ a-quadrat (expt b 2)) (expt c 2))
				  (incf anzahl))))))
		(when (> anzahl max-anzahl)
		  (setf max-anzahl anzahl
				max-p p))))))



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
		 (länge (length wortliste))
		 (anzahl 0)
		 (aktueller-wert 0))
	(do ((i 1 (1+ i)))
		((> i länge)
		 anzahl)
	  (setf aktueller-wert (alphabetischer-wert (pop wortliste)))
	  (when (dreieckszahlp aktueller-wert)
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
  (do ((i 3 (+ 2 i)))
	  ((> i 10000))
	(unless (primzahlp i)
	  (when (null (goldbach-aufgliedern i))
		(return i)))))



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
  (let
	  ((summe 0))
	(do ((i 1 (1+ i)))
		((> i 1000)
		 (last (zahl->liste summe) 10))
	  (incf summe (expt i i)))))



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
				 (if (teil-der-liste (tausche-ziffer i j k) kandidaten)
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
  (labels ((sortierte-ziffern (zahl)
			 (sort (zahl->liste zahl) #'<))
		   (gleiche-ziffern (i maxmult)
			 (let ((d (sortierte-ziffern i)))
			   (do ((m 2 (1+ m)))
				   ((> m maxmult)
					(return t))
				 (unless (equal (sortierte-ziffern (* m i)) d)
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
	(let ((summe 0))
	  (do ((i 1 (1+ i)))
		  ((> i limit)
		   summe)
		(dotimes (j i)
		  (when (> (möglichkeit i j) minimum)
			(incf summe)))))))



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
				 (blatt-test (lambda (h) t) #'höchste-karte<))))
	
		   (erstelle-kartenliste (stream-name)
			 "Einleseformat: 10 durch Leerzeichen getrennte Daten je Zeile"
			 (let ((kartenliste nil))
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
  (let ((maximum 0))
	(do ((a 1 (1+ a)))
		((>= a 100)
		 maximum)
	  (do ((b 1 (1+ b)))
		  ((>= b 100))
		(let ((wert (ziffer-summe (expt a b))))
		  (when (> wert maximum)
			(setf maximum wert)))))))



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
  (labels ((nächste-expansion (frac)
			 (+ 1 (/ 1 (+ 1 frac))))
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
					(entschlüsselt
					 (with-output-to-string (sstr)
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
  (let ((dreieck
		 (reverse '((59)
					(73 41)
					(52 40 09)
					(26 53 06 34)
					(10 51 87 86 81)
					(61 95 66 57 25 68)
					(90 81 80 38 92 67 73)
					(30 28 51 76 81 18 75 44)
					(84 14 95 87 62 81 17 78 58)
					(21 46 71 58 02 79 62 39 31 09)
					(56 34 35 53 78 31 81 18 90 93 15)
					(78 53 04 21 84 93 32 13 97 11 37 51)
					(45 03 81 79 05 18 78 86 13 30 63 99 95)
					(39 87 96 28 03 38 42 17 82 87 58 07 22 57)
					(06 17 51 17 07 93 09 07 75 97 95 78 87 08 53)
					(67 66 59 60 88 99 94 65 55 77 55 34 27 53 78 28)
					(76 40 41 04 87 16 09 42 75 69 23 97 30 60 10 79 87)
					(12 10 44 26 21 36 32 84 98 60 13 12 36 16 63 31 91 35)
					(70 39 06 05 55 27 38 48 28 22 34 35 62 62 15 14 94 89 86)
					(66 56 68 84 96 21 34 34 34 81 62 40 65 54 62 05 98 03 02 60)
					(38 89 46 37 99 54 34 53 36 14 70 26 02 90 45 13 31 61 83 73 47)
					(36 10 63 96 60 49 41 05 37 42 14 58 84 93 96 17 09 43 05 43 06 59)
					(66 57 87 57 61 28 37 51 84 73 79 15 39 95 88 87 43 39 11 86 77 74 18)
					(54 42 05 79 30 49 99 73 46 37 50 02 45 09 54 52 27 95 27 65 19 45 26 45)
					(71 39 17 78 76 29 52 90 18 99 78 19 35 62 71 19 23 65 93 85 49 33 75 09 02)
					(33 24 47 61 60 55 32 88 57 55 91 54 46 57 07 77 98 52 80 99 24 25 46 78 79 05)
					(92 09 13 55 10 67 26 78 76 82 63 49 51 31 24 68 05 57 07 54 69 21 67 43 17 63 12)
					(24 59 06 08 98 74 66 26 61 60 13 03 09 09 24 30 71 08 88 70 72 70 29 90 11 82 41 34)
					(66 82 67 04 36 60 92 77 91 85 62 49 59 61 30 90 29 94 26 41 89 04 53 22 83 41 09 74 90)
					(48 28 26 37 28 52 77 26 51 32 18 98 79 36 62 13 17 08 19 54 89 29 73 68 42 14 08 16 70 37)
					(37 60 69 70 72 71 09 59 13 60 38 13 57 36 09 30 43 89 30 39 15 02 44 73 05 73 26 63 56 86 12)
					(55 55 85 50 62 99 84 77 28 85 03 21 27 22 19 26 82 69 54 04 13 07 85 14 01 15 70 59 89 95 10 19)
					(04 09 31 92 91 38 92 86 98 75 21 05 64 42 62 84 36 20 73 42 21 23 22 51 51 79 25 45 85 53 03 43 22)
					(75 63 02 49 14 12 89 14 60 78 92 16 44 82 38 30 72 11 46 52 90 27 08 65 78 03 85 41 57 79 39 52 33 48)
					(78 27 56 56 39 13 19 43 86 72 58 95 39 07 04 34 21 98 39 15 39 84 89 69 84 46 37 57 59 35 59 50 26 15 93)
					(42 89 36 27 78 91 24 11 17 41 05 94 07 69 51 96 03 96 47 90 90 45 91 20 50 56 10 32 36 49 04 53 85 92 25 65)
					(52 09 61 30 61 97 66 21 96 92 98 90 06 34 96 60 32 69 68 33 75 84 18 31 71 50 84 63 03 03 19 11 28 42 75 45 45)
					(61 31 61 68 96 34 49 39 05 71 76 59 62 67 06 47 96 99 34 21 32 47 52 07 71 60 42 72 94 56 82 83 84 40 94 87 82 46)
					(01 20 60 14 17 38 26 78 66 81 45 95 18 51 98 81 48 16 53 88 37 52 69 95 72 93 22 34 98 20 54 27 73 61 56 63 60 34 63)
					(93 42 94 83 47 61 27 51 79 79 45 01 44 73 31 70 83 42 88 25 53 51 30 15 65 94 80 44 61 84 12 77 02 62 02 65 94 42 14 94)
					(32 73 09 67 68 29 74 98 10 19 85 48 38 31 85 67 53 93 93 77 47 67 39 72 94 53 18 43 77 40 78 32 29 59 24 06 02 83 50 60 66)
					(32 01 44 30 16 51 15 81 98 15 10 62 86 79 50 62 45 60 70 38 31 85 65 61 64 06 69 84 14 22 56 43 09 48 66 69 83 91 60 40 36 61)
					(92 48 22 99 15 95 64 43 01 16 94 02 99 19 17 69 11 58 97 56 89 31 77 45 67 96 12 73 08 20 36 47 81 44 50 64 68 85 40 81 85 52 09)
					(91 35 92 45 32 84 62 15 19 64 21 66 06 01 52 80 62 59 12 25 88 28 91 50 40 16 22 99 92 79 87 51 21 77 74 77 07 42 38 42 74 83 02 05)
					(46 19 77 66 24 18 05 32 02 84 31 99 92 58 96 72 91 36 62 99 55 29 53 42 12 37 26 58 89 50 66 19 82 75 12 48 24 87 91 85 02 07 03 76 86)
					(99 98 84 93 07 17 33 61 92 20 66 60 24 66 40 30 67 05 37 29 24 96 03 27 70 62 13 04 45 47 59 88 43 20 66 15 46 92 30 04 71 66 78 70 53 99)
					(67 60 38 06 88 04 17 72 10 99 71 07 42 25 54 05 26 64 91 50 45 71 06 30 67 48 69 82 08 56 80 67 18 46 66 63 01 20 08 80 47 07 91 16 03 79 87)
					(18 54 78 49 80 48 77 40 68 23 60 88 58 80 33 57 11 69 55 53 64 02 94 49 60 92 16 35 81 21 82 96 25 24 96 18 02 05 49 03 50 77 06 32 84 27 18 38)
					(68 01 50 04 03 21 42 94 53 24 89 05 92 26 52 36 68 11 85 01 04 42 02 45 15 06 50 04 53 73 25 74 81 88 98 21 67 84 79 97 99 20 95 04 40 46 02 58 87)
					(94 10 02 78 88 52 21 03 88 60 06 53 49 71 20 91 12 65 07 49 21 22 11 41 58 99 36 16 09 48 17 24 52 36 23 15 72 16 84 56 02 99 43 76 81 71 29 39 49 17)
					(64 39 59 84 86 16 17 66 03 09 43 06 64 18 63 29 68 06 23 07 87 14 26 35 17 12 98 41 53 64 78 18 98 27 28 84 80 67 75 62 10 11 76 90 54 10 05 54 41 39 66)
					(43 83 18 37 32 31 52 29 95 47 08 76 35 11 04 53 35 43 34 10 52 57 12 36 20 39 40 55 78 44 07 31 38 26 08 15 56 88 86 01 52 62 10 24 32 05 60 65 53 28 57 99)
					(03 50 03 52 07 73 49 92 66 80 01 46 08 67 25 36 73 93 07 42 25 53 13 96 76 83 87 90 54 89 78 22 78 91 73 51 69 09 79 94 83 53 09 40 69 62 10 79 49 47 03 81 30)
					(71 54 73 33 51 76 59 54 79 37 56 45 84 17 62 21 98 69 41 95 65 24 39 37 62 03 24 48 54 64 46 82 71 78 33 67 09 16 96 68 52 74 79 68 32 21 13 78 96 60 09 69 20 36)
					(73 26 21 44 46 38 17 83 65 98 07 23 52 46 61 97 33 13 60 31 70 15 36 77 31 58 56 93 75 68 21 36 69 53 90 75 25 82 39 50 65 94 29 30 11 33 11 13 96 02 56 47 07 49 02)
					(76 46 73 30 10 20 60 70 14 56 34 26 37 39 48 24 55 76 84 91 39 86 95 61 50 14 53 93 64 67 37 31 10 84 42 70 48 20 10 72 60 61 84 79 69 65 99 73 89 25 85 48 92 56 97 16)
					(03 14 80 27 22 30 44 27 67 75 79 32 51 54 81 29 65 14 19 04 13 82 04 91 43 40 12 52 29 99 07 76 60 25 01 07 61 71 37 92 40 47 99 66 57 01 43 44 22 40 53 53 09 69 26 81 07)
					(49 80 56 90 93 87 47 13 75 28 87 23 72 79 32 18 27 20 28 10 37 59 21 18 70 04 79 96 03 31 45 71 81 06 14 18 17 05 31 50 92 79 23 47 09 39 47 91 43 54 69 47 42 95 62 46 32 85)
					(37 18 62 85 87 28 64 05 77 51 47 26 30 65 05 70 65 75 59 80 42 52 25 20 44 10 92 17 71 95 52 14 77 13 24 55 11 65 26 91 01 30 63 15 49 48 41 17 67 47 03 68 20 90 98 32 04 40 68)
					(90 51 58 60 06 55 23 68 05 19 76 94 82 36 96 43 38 90 87 28 33 83 05 17 70 83 96 93 06 04 78 47 80 06 23 84 75 23 87 72 99 14 50 98 92 38 90 64 61 58 76 94 36 66 87 80 51 35 61 38)
					(57 95 64 06 53 36 82 51 40 33 47 14 07 98 78 65 39 58 53 06 50 53 04 69 40 68 36 69 75 78 75 60 03 32 39 24 74 47 26 90 13 40 44 71 90 76 51 24 36 50 25 45 70 80 61 80 61 43 90 64 11)
					(18 29 86 56 68 42 79 10 42 44 30 12 96 18 23 18 52 59 02 99 67 46 60 86 43 38 55 17 44 93 42 21 55 14 47 34 55 16 49 24 23 29 96 51 55 10 46 53 27 92 27 46 63 57 30 65 43 27 21 20 24 83)
					(81 72 93 19 69 52 48 01 13 83 92 69 20 48 69 59 20 62 05 42 28 89 90 99 32 72 84 17 08 87 36 03 60 31 36 36 81 26 97 36 48 54 56 56 27 16 91 08 23 11 87 99 33 47 02 14 44 73 70 99 43 35 33)
					(90 56 61 86 56 12 70 59 63 32 01 15 81 47 71 76 95 32 65 80 54 70 34 51 40 45 33 04 64 55 78 68 88 47 31 47 68 87 03 84 23 44 89 72 35 08 31 76 63 26 90 85 96 67 65 91 19 14 17 86 04 71 32 95)
					(37 13 04 22 64 37 37 28 56 62 86 33 07 37 10 44 52 82 52 06 19 52 57 75 90 26 91 24 06 21 14 67 76 30 46 14 35 89 89 41 03 64 56 97 87 63 22 34 03 79 17 45 11 53 25 56 96 61 23 18 63 31 37 37 47)
					(77 23 26 70 72 76 77 04 28 64 71 69 14 85 96 54 95 48 06 62 99 83 86 77 97 75 71 66 30 19 57 90 33 01 60 61 14 12 90 99 32 77 56 41 18 14 87 49 10 14 90 64 18 50 21 74 14 16 88 05 45 73 82 47 74 44)
					(22 97 41 13 34 31 54 61 56 94 03 24 59 27 98 77 04 09 37 40 12 26 87 09 71 70 07 18 64 57 80 21 12 71 83 94 60 39 73 79 73 19 97 32 64 29 41 07 48 84 85 67 12 74 95 20 24 52 41 67 56 61 29 93 35 72 69)
					(72 23 63 66 01 11 07 30 52 56 95 16 65 26 83 90 50 74 60 18 16 48 43 77 37 11 99 98 30 94 91 26 62 73 45 12 87 73 47 27 01 88 66 99 21 41 95 80 02 53 23 32 61 48 32 43 43 83 14 66 95 91 19 81 80 67 25 88)
					(08 62 32 18 92 14 83 71 37 96 11 83 39 99 05 16 23 27 10 67 02 25 44 11 55 31 46 64 41 56 44 74 26 81 51 31 45 85 87 09 81 95 22 28 76 69 46 48 64 87 67 76 27 89 31 11 74 16 62 03 60 94 42 47 09 34 94 93 72)
					(56 18 90 18 42 17 42 32 14 86 06 53 33 95 99 35 29 15 44 20 49 59 25 54 34 59 84 21 23 54 35 90 78 16 93 13 37 88 54 19 86 67 68 55 66 84 65 42 98 37 87 56 33 28 58 38 28 38 66 27 52 21 81 15 08 22 97 32 85 27)
					(91 53 40 28 13 34 91 25 01 63 50 37 22 49 71 58 32 28 30 18 68 94 23 83 63 62 94 76 80 41 90 22 82 52 29 12 18 56 10 08 35 14 37 57 23 65 67 40 72 39 93 39 70 89 40 34 07 46 94 22 20 05 53 64 56 30 05 56 61 88 27)
					(23 95 11 12 37 69 68 24 66 10 87 70 43 50 75 07 62 41 83 58 95 93 89 79 45 39 02 22 05 22 95 43 62 11 68 29 17 40 26 44 25 71 87 16 70 85 19 25 59 94 90 41 41 80 61 70 55 60 84 33 95 76 42 63 15 09 03 40 38 12 03 32)
					(09 84 56 80 61 55 85 97 16 94 82 94 98 57 84 30 84 48 93 90 71 05 95 90 73 17 30 98 40 64 65 89 07 79 09 19 56 36 42 30 23 69 73 72 07 05 27 61 24 31 43 48 71 84 21 28 26 65 65 59 65 74 77 20 10 81 61 84 95 08 52 23 70)
					(47 81 28 09 98 51 67 64 35 51 59 36 92 82 77 65 80 24 72 53 22 07 27 10 21 28 30 22 48 82 80 48 56 20 14 43 18 25 50 95 90 31 77 08 09 48 44 80 90 22 93 45 82 17 13 96 25 26 08 73 34 99 06 49 24 06 83 51 40 14 15 10 25 01)
					(54 25 10 81 30 64 24 74 75 80 36 75 82 60 22 69 72 91 45 67 03 62 79 54 89 74 44 83 64 96 66 73 44 30 74 50 37 05 09 97 70 01 60 46 37 91 39 75 75 18 58 52 72 78 51 81 86 52 08 97 01 46 43 66 98 62 81 18 70 93 73 08 32 46 34)
					(96 80 82 07 59 71 92 53 19 20 88 66 03 26 26 10 24 27 50 82 94 73 63 08 51 33 22 45 19 13 58 33 90 15 22 50 36 13 55 06 35 47 82 52 33 61 36 27 28 46 98 14 73 20 73 32 16 26 80 53 47 66 76 38 94 45 02 01 22 52 47 96 64 58 52 39)
					(88 46 23 39 74 63 81 64 20 90 33 33 76 55 58 26 10 46 42 26 74 74 12 83 32 43 09 02 73 55 86 54 85 34 28 23 29 79 91 62 47 41 82 87 99 22 48 90 20 05 96 75 95 04 43 28 81 39 81 01 28 42 78 25 39 77 90 57 58 98 17 36 73 22 63 74 51)
					(29 39 74 94 95 78 64 24 38 86 63 87 93 06 70 92 22 16 80 64 29 52 20 27 23 50 14 13 87 15 72 96 81 22 08 49 72 30 70 24 79 31 16 64 59 21 89 34 96 91 48 76 43 53 88 01 57 80 23 81 90 79 58 01 80 87 17 99 86 90 72 63 32 69 14 28 88 69)
					(37 17 71 95 56 93 71 35 43 45 04 98 92 94 84 96 11 30 31 27 31 60 92 03 48 05 98 91 86 94 35 90 90 08 48 19 33 28 68 37 59 26 65 96 50 68 22 07 09 49 34 31 77 49 43 06 75 17 81 87 61 79 52 26 27 72 29 50 07 98 86 01 17 10 46 64 24 18 56)
					(51 30 25 94 88 85 79 91 40 33 63 84 49 67 98 92 15 26 75 19 82 05 18 78 65 93 61 48 91 43 59 41 70 51 22 15 92 81 67 91 46 98 11 11 65 31 66 10 98 65 83 21 05 56 05 98 73 67 46 74 69 34 08 30 05 52 07 98 32 95 30 94 65 50 24 63 28 81 99 57)
					(19 23 61 36 09 89 71 98 65 17 30 29 89 26 79 74 94 11 44 48 97 54 81 55 39 66 69 45 28 47 13 86 15 76 74 70 84 32 36 33 79 20 78 14 41 47 89 28 81 05 99 66 81 86 38 26 06 25 13 60 54 55 23 53 27 05 89 25 23 11 13 54 59 54 56 34 16 24 53 44 06)
					(13 40 57 72 21 15 60 08 04 19 11 98 34 45 09 97 86 71 03 15 56 19 15 44 97 31 90 04 87 87 76 08 12 30 24 62 84 28 12 85 82 53 99 52 13 94 06 65 97 86 09 50 94 68 69 74 30 67 87 94 63 07 78 27 80 36 69 41 06 92 32 78 37 82 30 05 18 87 99 72 19 99)
					(44 20 55 77 69 91 27 31 28 81 80 27 02 07 97 23 95 98 12 25 75 29 47 71 07 47 78 39 41 59 27 76 13 15 66 61 68 35 69 86 16 53 67 63 99 85 41 56 08 28 33 40 94 76 90 85 31 70 24 65 84 65 99 82 19 25 54 37 21 46 33 02 52 99 51 33 26 04 87 02 08 18 96)
					(54 42 61 45 91 06 64 79 80 82 32 16 83 63 42 49 19 78 65 97 40 42 14 61 49 34 04 18 25 98 59 30 82 72 26 88 54 36 21 75 03 88 99 53 46 51 55 78 22 94 34 40 68 87 84 25 30 76 25 08 92 84 42 61 40 38 09 99 40 23 29 39 46 55 10 90 35 84 56 70 63 23 91 39)
					(52 92 03 71 89 07 09 37 68 66 58 20 44 92 51 56 13 71 79 99 26 37 02 06 16 67 36 52 58 16 79 73 56 60 59 27 44 77 94 82 20 50 98 33 09 87 94 37 40 83 64 83 58 85 17 76 53 02 83 52 22 27 39 20 48 92 45 21 09 42 24 23 12 37 52 28 50 78 79 20 86 62 73 20 59)
					(54 96 80 15 91 90 99 70 10 09 58 90 93 50 81 99 54 38 36 10 30 11 35 84 16 45 82 18 11 97 36 43 96 79 97 65 40 48 23 19 17 31 64 52 65 65 37 32 65 76 99 79 34 65 79 27 55 33 03 01 33 27 61 28 66 08 04 70 49 46 48 83 01 45 19 96 13 81 14 21 31 79 93 85 50 05)
					(92 92 48 84 59 98 31 53 23 27 15 22 79 95 24 76 05 79 16 93 97 89 38 89 42 83 02 88 94 95 82 21 01 97 48 39 31 78 09 65 50 56 97 61 01 07 65 27 21 23 14 15 80 97 44 78 49 35 33 45 81 74 34 05 31 57 09 38 94 07 69 54 69 32 65 68 46 68 78 90 24 28 49 51 45 86 35)
					(41 63 89 76 87 31 86 09 46 14 87 82 22 29 47 16 13 10 70 72 82 95 48 64 58 43 13 75 42 69 21 12 67 13 64 85 58 23 98 09 37 76 05 22 31 12 66 50 29 99 86 72 45 25 10 28 19 06 90 43 29 31 67 79 46 25 74 14 97 35 76 37 65 46 23 82 06 22 30 76 93 66 94 17 96 13 20 72)
					(63 40 78 08 52 09 90 41 70 28 36 14 46 44 85 96 24 52 58 15 87 37 05 98 99 39 13 61 76 38 44 99 83 74 90 22 53 80 56 98 30 51 63 39 44 30 91 91 04 22 27 73 17 35 53 18 35 45 54 56 27 78 48 13 69 36 44 38 71 25 30 56 15 22 73 43 32 69 59 25 93 83 45 11 34 94 44 39 92)
					(12 36 56 88 13 96 16 12 55 54 11 47 19 78 17 17 68 81 77 51 42 55 99 85 66 27 81 79 93 42 65 61 69 74 14 01 18 56 12 01 58 37 91 22 42 66 83 25 19 04 96 41 25 45 18 69 96 88 36 93 10 12 98 32 44 83 83 04 72 91 04 27 73 07 34 37 71 60 59 31 01 54 54 44 96 93 83 36 04 45)
					(30 18 22 20 42 96 65 79 17 41 55 69 94 81 29 80 91 31 85 25 47 26 43 49 02 99 34 67 99 76 16 14 15 93 08 32 99 44 61 77 67 50 43 55 87 55 53 72 17 46 62 25 50 99 73 05 93 48 17 31 70 80 59 09 44 59 45 13 74 66 58 94 87 73 16 14 85 38 74 99 64 23 79 28 71 42 20 37 82 31 23)
					(51 96 39 65 46 71 56 13 29 68 53 86 45 33 51 49 12 91 21 21 76 85 02 17 98 15 46 12 60 21 88 30 92 83 44 59 42 50 27 88 46 86 94 73 45 54 23 24 14 10 94 21 20 34 23 51 04 83 99 75 90 63 60 16 22 33 83 70 11 32 10 50 29 30 83 46 11 05 31 17 86 42 49 01 44 63 28 60 07 78 95 40)
					(44 61 89 59 04 49 51 27 69 71 46 76 44 04 09 34 56 39 15 06 94 91 75 90 65 27 56 23 74 06 23 33 36 69 14 39 05 34 35 57 33 22 76 46 56 10 61 65 98 09 16 69 04 62 65 18 99 76 49 18 72 66 73 83 82 40 76 31 89 91 27 88 17 35 41 35 32 51 32 67 52 68 74 85 80 57 07 11 62 66 47 22 67)
					(65 37 19 97 26 17 16 24 24 17 50 37 64 82 24 36 32 11 68 34 69 31 32 89 79 93 96 68 49 90 14 23 04 04 67 99 81 74 70 74 36 96 68 09 64 39 88 35 54 89 96 58 66 27 88 97 32 14 06 35 78 20 71 06 85 66 57 02 58 91 72 05 29 56 73 48 86 52 09 93 22 57 79 42 12 01 31 68 17 59 63 76 07 77)
					(73 81 14 13 17 20 11 09 01 83 08 85 91 70 84 63 62 77 37 07 47 01 59 95 39 69 39 21 99 09 87 02 97 16 92 36 74 71 90 66 33 73 73 75 52 91 11 12 26 53 05 26 26 48 61 50 90 65 01 87 42 47 74 35 22 73 24 26 56 70 52 05 48 41 31 18 83 27 21 39 80 85 26 08 44 02 71 07 63 22 05 52 19 08 20)
					(17 25 21 11 72 93 33 49 64 23 53 82 03 13 91 65 85 02 40 05 42 31 77 42 05 36 06 54 04 58 07 76 87 83 25 57 66 12 74 33 85 37 74 32 20 69 03 97 91 68 82 44 19 14 89 28 85 85 80 53 34 87 58 98 88 78 48 65 98 40 11 57 10 67 70 81 60 79 74 72 97 59 79 47 30 20 54 80 89 91 14 05 33 36 79 39)
					(60 85 59 39 60 07 57 76 77 92 06 35 15 72 23 41 45 52 95 18 64 79 86 53 56 31 69 11 91 31 84 50 44 82 22 81 41 40 30 42 30 91 48 94 74 76 64 58 74 25 96 57 14 19 03 99 28 83 15 75 99 01 89 85 79 50 03 95 32 67 44 08 07 41 62 64 29 20 14 76 26 55 48 71 69 66 19 72 44 25 14 01 48 74 12 98 07)
					(64 66 84 24 18 16 27 48 20 14 47 69 30 86 48 40 23 16 61 21 51 50 26 47 35 33 91 28 78 64 43 68 04 79 51 08 19 60 52 95 06 68 46 86 35 97 27 58 04 65 30 58 99 12 12 75 91 39 50 31 42 64 70 04 46 07 98 73 98 93 37 89 77 91 64 71 64 65 66 21 78 62 81 74 42 20 83 70 73 95 78 45 92 27 34 53 71 15)
					(30 11 85 31 34 71 13 48 05 14 44 03 19 67 23 73 19 57 06 90 94 72 57 69 81 62 59 68 88 57 55 69 49 13 07 87 97 80 89 05 71 05 05 26 38 40 16 62 45 99 18 38 98 24 21 26 62 74 69 04 85 57 77 35 58 67 91 79 79 57 86 28 66 34 72 51 76 78 36 95 63 90 08 78 47 63 45 31 22 70 52 48 79 94 15 77 61 67 68)
					(23 33 44 81 80 92 93 75 94 88 23 61 39 76 22 03 28 94 32 06 49 65 41 34 18 23 08 47 62 60 03 63 33 13 80 52 31 54 73 43 70 26 16 69 57 87 83 31 03 93 70 81 47 95 77 44 29 68 39 51 56 59 63 07 25 70 07 77 43 53 64 03 94 42 95 39 18 01 66 21 16 97 20 50 90 16 70 10 95 69 29 06 25 61 41 26 15 59 63 35)
					))))
  (first (route-dreieck dreieck))))






