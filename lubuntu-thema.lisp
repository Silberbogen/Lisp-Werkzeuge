;-*-lisp-*-
;-*- coding: utf-8 -*-
;;;; ============================================================================================
;;;; Lubuntu Thema zum Nachladen durch die StumpWM-RC-Datei
;;;; Version 0.02
;;;; ============================================================================================
(in-package :stumpwm)

(setq *archivmanager* (concatenate 'string "file-roller" " "))

(setq *browser* (concatenate 'string "chromium-browser" " "))

(setq *dateimanager* (concatenate 'string "pcmanfm" " "))

(setq *email-client* (concatenate 'string "sylpheed" " "))

(setq *medien-abspieler* (concatenate 'string "gnome-mplayer" " "))

(setq *nachrichtendienst* (concatenate 'string "pidgin" " "))

(setq *software-center* (concatenate 'string "lubuntu-software-center" " "))

(setq *systerm* "exec uxterm -fg CornflowerBlue -bg DarkSlateGrey -class 'Admin-uxterm' -e 'sudo -i'")

(setq *uxthema* "exec uxterm -fg DodgerBlue -bg NavyBlue")

(setq *uxt* "uxterm -fg DodgerBlue -bg NavyBlue -class 'Userterminal'")

(setq *uxt-neu* "uxterm -fg DodgerBlue -bg NavyBlue")

;; Lubuntu 12.04 Thema
(set-focus-color      "DodgerBlue")
(set-unfocus-color    "black")
(set-win-bg-color     "CornflowerBlue")
(set-fg-color         "NavyBlue")
(set-bg-color         "PowderBlue")

;; Vorbereitungen für das Modeline-Verhalten
(setf *mode-line-background-color*      "DodgerBlue3"
      *mode-line-foreground-color*      "PowderBlue"
      *mode-line-border-color*          "CornflowerBlue"
      *mode-line-timeout*               1
      *mode-line-position*              :bottom
      *time-format-string-default*      "%a %d.%m.%Y %I:%M%p"
      *time-modeline-string*            "%a %d.%m.%Y %I:%M%p"
      *group-format*                    "%n%s%t"
      *window-format*                   "«%n%s%m%10c»")