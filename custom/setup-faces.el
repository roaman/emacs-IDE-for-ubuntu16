;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; ;; change font to Inconsolata for better looking text
;; ;; remember to install the font Inconsolata first
;; (setq default-frame-alist '((font . "Inconsolata-11")))
;; ;; set italic font for italic face, since Emacs does not set italic
;; ;; face automatically
;; (set-face-attribute 'italic nil
;;                    :family "Inconsolata-Italic")

;; set default font
;; sudo apt-get install ttf-bitstream-vera
(set-default-font "Bitstream Vera Sans Mono-11")

;; set default theme
(load-theme 'zenburn t)


(provide 'setup-faces)
