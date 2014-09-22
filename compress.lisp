;; Frei nach Programm 3.1 von Paul Graham ...
;; Das Komprimieren



(defun n-elemente (element anzahl)
  (if (> anzahl 1)
	  (list anzahl element)
	  element))



(defun komprimiere-element (element anzahl liste)
  (if (null liste)
	  (list (n-elemente element anzahl))
	  (let
		  ((nächstes (first liste)))
		(if (eql nächstes element)
			(komprimiere-element element (1+ anzahl) (rest liste))
			(cons (n-elemente element anzahl)
				  (komprimiere-element nächstes 1 (rest liste)))))))



(defun komprimiere (x)
  (if (consp x)
	  (komprimiere-element (first x) 1 (rest x))
	  x))



;; ... und das Dekomprimieren



(defun liste-von (anzahl element)
  (if (zerop anzahl)
	  nil
	  (cons element (liste-von (1- anzahl) element))))



(defun entkomprimiere (liste)
  (if (null liste)
	  nil
	  (let
		  ((element (first liste))
		   (listen-rest (entkomprimiere (rest liste))))
		(if (consp element)
			(append (apply #'liste-von element)
					listen-rest)
			(cons element listen-rest)))))
