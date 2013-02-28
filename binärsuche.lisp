(defun binäre-suche (objekt vektor)
  "(binäre-suche objekt vektor)
BINÄRE-SUCHE prüft, ob ein Objekt in einem Vektor vorhanden ist. Ist die Suche erfolgreich, wird das Objekt zurückgegeben, ansonsten NIL.
Beispiel: (setf test (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)) => #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
 (binäre-suche 14 test) => 14
 (binäre-suche 19 test) => NIL"
  (let ((länge (length vektor)))
    (and (not (zerop länge))
	 (%finder objekt vektor 0 (1- länge)))))

(defun %finder (objekt vektor anfang ende)
  "(%finder objekt vektor anfang ende)
%FINDER ist eine interne Funktion für BINÄRE-SUCHE. %FINDER ist die Implementation des binären Suchalgorithmus für Vektoren. Ein Aufruf erfolgt über die Funktion BINÄRE-SUCHE."
  (let ((reichweite (- ende anfang)))
    (if (zerop reichweite)
	(if (eql objekt (svref vektor anfang))
	    objekt
	    nil)
	(let* ((mitte (+ anfang (ash reichweite -1)))
	       (tempobjekt (svref vektor mitte)))
	  (cond ((< objekt tempobjekt)
		 (%finder objekt vektor anfang (1- mitte)))
		((> objekt tempobjekt)
		 (%finder objekt vektor (1+ mitte) ende))
		(t objekt))))))
	  