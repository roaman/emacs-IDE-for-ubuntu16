;; GROUP: Environment -> Initialization
(setq inhibit-startup-screen t)

;; Environment -> frame
;; you won't need any of the bar thingies
;; turn it off to save screen estate
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; taken from prelude-ui.el
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; GROUP: Environment -> Mode Line
;; show point number in mode-line
(setq column-number-mode t)
(setq line-number-mode t)
;; display the dim on the mode-line, and use 24hr.
(display-time-mode 1)
(setq display-time-24hr-format t)

(size-indication-mode t)

;; GROUP: Environment -> Minibuffer
;; (icomplete-mode)

;; GROUP: Environment -> Minibuffer -> Savehist
;; savehist saves minibuffer history by defaults
(setq savehist-additional-variables '(search ring regexp-search-ring) ; also save your regexp search queries
      savehist-autosave-interval 60     ; save every minute
      )

;; GROUP: Environment -> Windows -> Winner ;;
(winner-mode 1)


(provide 'setup-environment)
