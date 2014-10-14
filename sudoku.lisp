(defparameter *tabelle* (make-array '(9 9) :initial-contents
									'((3 9 4  8 5 2  6 7 1)
									  (2 6 8  3 7 1  4 5 9)
									  (5 7 1  6 9 4  8 2 3)
									  (1 4 5  7 8 3  9 6 2)
									  (6 8 2  9 4 5  3 1 7)
									  (9 3 7  1 2 6  5 8 4)
									  (4 1 3  5 6 7  2 9 8)
									  (7 5 9  2 3 8  1 4 6)
									  (8 2 6  4 1 9  7 3 5))))

(defun gültige-sequenz-p (seq)
  (let* ((sortiert (sort seq #'<))
		 (gültig (apply #'< sortiert))
		 (länge (length sortiert)))
	(format t "Sortiert: ~A, gültig: ~A, Länge: ~A~%" sortiert gültig länge)
	(and gültig (= 9 länge))))


(defun zeile (nr tab)
  (let ((lst '()))
	(dotimes (i 9 lst)
	  (push (aref tab nr i) lst))))

(defun spalte (nr tab)
  (let ((lst '()))
	(dotimes (i 9 lst)
	  (push (aref tab i nr) lst))))

(defun box (nr tab)
  (let ((lst '())
		(x-kor (* 3 (rem nr 3)))
		(y-kor (* 3 (truncate (/ nr 3)))))
	(dotimes (i 3 lst)
	  (dotimes (j 3)
		(push (aref tab (+ i x-kor) (+ j y-kor)) lst)))))

(defun gültige-lösung-p (tab)
  (dotimes (i 9 t)
	  (unless (and (gültige-sequenz-p (zeile i tab))
				   (gültige-sequenz-p (spalte i tab))
				   (gültige-sequenz-p (box i tab)))
		(return-from gültige-lösung-p 'nil))))



(defun erzeuge-rätsel-aus-vorgabe (tab &optional (anz 25) &aux (max 9) (felder 81))
  (let ((rätsel (make-array '(9 9) :initial-element '_)))
	(dotimes (i max rätsel)
	  (dotimes (j max)
		(when (< (random felder) anz)
		  (setf (aref rätsel i j) (aref tab i j)))))))



(defun suche-sudoku ()
  (let ((rätsel (make-array '(9 9) :initial-element '_)))
	(flet ((setze-feld (i j)
			 (let ((lst '(1 2 3 4 5 6 7 8 9)))
			   (setf lst (set-difference lst (zeile i rätsel)))
			   (setf lst (set-difference lst (spalte j rätsel)))
			   (setf lst (set-difference lst (box (+ (truncate (/ i 3))
													 (* (truncate (/ j 3)) 3)) rätsel)))
			   (remove '_ lst)
			   (when (null lst)
				 (return-from suche-sudoku 'nil))
			   (setf (aref rätsel i j) (elt lst (random (length lst)))))))
	  (dotimes (i 9 rätsel)
		(do ((j i (1+ j)))
			((= j 9))
		  (setze-feld i j)
		  (when (eql '_ (aref rätsel j i))
			(setze-feld j i)))))))


(defun erzeuge-sudoku ()
  (let ((rätsel (suche-sudoku)))
	(when rätsel
	  (return-from erzeuge-sudoku rätsel))
	(erzeuge-sudoku)))

