;-*-lisp-*-
;-*- coding: utf-8 -*-

;;; Dateiname: craps.lisp
;;; Beschreibung: Eine kleine Umsetzung des Würfelspiels Craps
;;; Author: Sascha Biermanns (saschakb), <skkd dot h4k1n9 at yahoo dot de>

;;; Lizenz: ISC

;;; Copyright (C) 2012, Sascha Biermanns

;;; Permission to use, copy, modify, and/or distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; -----------------------------------------------------------------
;;; Hinweis:
;;; Das Laden dieser Datei in Common Lisp erfolgt per:
;;; (load "craps")
;;; Zur Verbesserung der Geschwinidkeit bitte vorher compilieren per:
;;; (compile-file "craps")
;;; -----------------------------------------------------------------

(defun werfe-zwei-würfel ()
  "(werfe-zwei-würfel)
WERFE-ZWEI-WÜRFEL dient dazu, eine Liste zurückzugeben, in der 2 Würfelwürfe mit 6-seitigen Würfeln enthalten sind."
  (list (würfelwurf) (würfelwurf)))

(defun schlangenaugen-p (liste)
  "(schlangenaugen-p liste)
SCHLANGENAUGEN-P überprüft, ob eine übergebene Liste den Wurf zweier Einsen, im Spiel Craps auch Schlangenaugen (engl. snake eyes) genannt, enthält."
  (cond ((and (eql (first liste) 1) (eql (second liste) 1)) t)
	(t nil)))

(defun güterwagen-p (liste)
  "(güterwagen-p liste)
GÜTERWAGEN-P überprüft,ob eine übergebene Liste den Wurf zweier Sechsen, im Spiel Craps auch Güterwagen (engl. boxcars) genannt, enthält."
  (cond ((and (eql (first liste) 6) (eql (second liste) 6)) t)
	(t nil)))
(defun craps-sofort-gewinn-p (liste)
  "(craps-sofort-gewinn-p liste) Nach den amerikanischen Casinoregeln ist der Wurf von 7 oder 11 ein Sofortgewinn."
  (let ((wurf (apply #'+ liste)))
    (cond ((or (eql wurf 7) (eql wurf 11)) t)
	  (t nil))))

(defun craps-sofort-verlust-p (liste)
  "(craps-sofort-verlust-p liste) Nach den amerikanischen Casinoregeln ist der Wurf von 2, 3 oder 12 ein Sofortverlust."
  (let ((wurf (apply #'+ liste)))
    (cond ((or (eql wurf 2) (eql wurf 3) (eql wurf 12)) t)
	  (t nil))))  

(defun craps-sage-wurf (liste)
  "(craps-sage-wurf liste)
CRAPS-SAGE-WURF liest das Ergebnis des per Liste übergebenen Wurfs, addiert die Werte und gibt entweder SCHLANGENAUGEN, GÜTERWAGEN oder die Summe der Augenpaare zurück."
  (cond ((schlangenaugen-p liste) 'schlangenaugen)
	((güterwagen-p liste) 'güterwagen)
	(t (apply #'+ liste))))

(defun craps ()
  "(craps) führt eine Partie des Würfelspiels Craps nach den amerikanischen Casinoregeln durch."
  (let* ((wurf (werfe-zwei-würfel))
	 (liste ())
	 (geworfen (list (erster wurf) 'und (zweiter wurf) 'gewürfelt)))
    (cond ((craps-sofort-gewinn-p wurf)
	   (setq liste (list '-- (craps-sage-wurf wurf) '-- 'du 'gewinnst))
	   (append geworfen liste))
	  ((craps-sofort-verlust-p wurf)
	   (setq liste (list '-- (craps-sage-wurf wurf) '-- 'du 'verlierst))
	   (append geworfen liste))
	  (t
	   (setq liste (list '-- 'du 'hast (apply #'+ wurf) 'punkte))
	   (princ (append geworfen liste))
	   (terpri)
	   (craps-versuche-zu-punkten (apply #'+ wurf))))))

(defun craps-versuche-zu-punkten (zahl)
  "(craps-versuche-zu-punkten zahl)
CRAPS-VERSUCHE-ZU-PUNKTEN ermöglicht es zu versuchen, die vorherige Zahl noch einmal zu würfeln und so zu gewinnen."
  (let* ((wurf (werfe-zwei-würfel))
	 (liste ())
	 (geworfen (list (erster wurf) 'und (zweiter wurf) 'gewürfelt)))
    (cond ((or (craps-sofort-gewinn-p wurf) (eql zahl (apply #'+ wurf)))
	   (setq liste (list '-- (craps-sage-wurf wurf) '-- 'du 'gewinnst))
	   (append geworfen liste))
	  ((craps-sofort-verlust-p wurf)
	   (setq liste (list '-- (craps-sage-wurf wurf) '-- 'du 'verlierst))
	   (append geworfen liste))
	  (t
	   (setq liste (list '-- (apply #'+ wurf) '-- 'würfle 'nochmal))
	   (princ (append geworfen liste))
	   (terpri)
	   (craps-versuche-zu-punkten zahl)))))