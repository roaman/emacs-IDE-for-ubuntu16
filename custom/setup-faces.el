;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; ;; change font to Inconsolata for better looking text
;; ;; remember to install the font Inconsolata first
(setq default-frame-alist '((font . "DejaVu Sans Mono-11")))
;; ;; set italic font for italic face, since Emacs does not set italic
;; ;; face automatically
;; (set-face-attribute 'italic nil
;;                    :family "Inconsolata-Italic")


;; add chinese font
;; (create-fontset-from-fontset-spec "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-fontset-myfontset")
;; (set-fontset-font "fontset-myfontset" 'han "WenQuanYi Micro Hei Mono")
;; (add-to-list 'default-frame-alist '(font . "fontset-myfontset"))


;; set default theme
(load-theme 'zenburn t)


(provide 'setup-faces)
