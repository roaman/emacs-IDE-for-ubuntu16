;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; ;; change font to Inconsolata for better looking text
;; ;; remember to install the font Inconsolata first
;; ;; sudo apt-get install fonts-inconsolata
;; ;; check: fc-list
;; (setq default-frame-alist '((font . "Inconsolata-12")))
;; ;; set italic font for italic face, since Emacs does not set italic
;; ;; face automatically
;; (set-face-attribute 'italic nil
;;                    :family "Inconsolata-Italic")

;; set default font
;; (set-default-font "DejaVu Sans Mono-12")
(set-frame-font "Source Code Pro-12")
;; command: M-x describe-fone

;; set default theme
(load-theme 'zenburn t)


(provide 'setup-faces)
