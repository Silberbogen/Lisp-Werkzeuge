;-*-lisp-*-
;-*- coding: utf-8 -*-
;;;; ============================================================================================
;;;; Ubuntu Thema zum Nachladen durch die StumpWM-RC-Datei
;;;; Version 0.02
;;;; ============================================================================================
(in-package :stumpwm)

(setq *archivmanager* (concatenate 'string "file-roller" " "))

(setq *browser* (concatenate 'string "firefox" " "))

(setq *dateimanager* (concatenate 'string "nautilus" " "))

(setq *email-client* (concatenate 'string "thunderbird" " "))

(setq *medien-abspieler* (concatenate 'string "rhythmbox" " "))

(setq *nachrichtendienst* (concatenate 'string "empathy" " "))

(setq *software-center* (concatenate 'string "software-center" " "))

(setq *systerm* "exec uxterm -fg gainsboro -bg OrangeRed -class 'Admin-uxterm' -e 'sudo -i'")

(setq *uxthema* "exec uxterm -fg lavender -bg maroon4")

(setq *uxt* "uxterm -fg lavender -bg maroon4 -class 'Userterminal'")

(setq *uxt-neu* "uxterm -fg lavender -bg maroon4")

;; Ubuntu 12.04 LTS Thema
(set-focus-color      "orange")
(set-unfocus-color    "black")
(set-win-bg-color     "gainsboro")
(set-fg-color         "maroon4")
(set-bg-color         "gainsboro")

;; Vorbereitungen für das Modeline-Verhalten
(setf *mode-line-background-color*      "OrangeRed" ; "Grey18"
      *mode-line-foreground-color*      "gainsboro" ; "DodgerBlue"
      *mode-line-border-color*          "gainsboro" ; "CornflowerBlue"
      *mode-line-timeout*               1
      *mode-line-position*              :top
      *time-format-string-default*      "%a %d.%m.%Y %I:%M%p"
      *time-modeline-string*            "%a %d.%m.%Y %I:%M%p"
      *group-format*                    "%n%s%t"
      *window-format*                   "«%n%s%m%10c»")