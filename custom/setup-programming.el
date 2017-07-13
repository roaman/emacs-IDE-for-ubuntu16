;;; Programming -> common
;;; ------------------------

;; GROUP: Programming -> Tools -> Ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)


;;; Source code navigation
;;; --------------------

;; Package: ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;;;; source code editing
;;;; --------------------

;; hs-minor-mode allow users to fold and hide blocks of text
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; narrow
;; C-x n d/r/n/w

;;; bellowing for  Indentation part

;; c/c++ style
;;; GROUP: Programming -> Languages -> C/C++
(setq c-default-style "linux" ; set style to "linux"
      c-basic-offset 4)

;; redefine the key: RET
(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; whitespace auto-tailling

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; whenever you create useless whitespace, the whitespace is highlighted
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; you can clean whitespace use command:
;; (whitespace-cleanup)

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)
(add-hook 'text-mode 'ws-butler-mode)
(add-hook 'fundamental-mode 'ws-butler-mode)



;;; code template
;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
;; (define-key yas-minor-mode-map [(tab)] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
;; No dropdowns please, yas
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))


;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
;; (show-smartparens-global-mode +1)
;; (smartparens-global-mode 1)


;;; Comment
;; Package: comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;;; search and replace
;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
(require 'anzu)
(global-anzu-mode)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; ;; PACKAGE: duplicate-thing
;; (require 'duplicate-thing)
;; (global-set-key (kbd "M-c") 'duplicate-thing)

;; Package: company
;; how include system header file ? that is todo!
;; company-clang for system header and ggtags for project header ?
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;; GROUP: Programming -> Tools -> Fly-check
;; PACKAGE: flycheck
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)


;;; compile and debug
;;; -------------------

;; GROUP: Programming -> Tools -> Gdb
(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)    ; Non-nil means display source file containing the main routine at startup

;; GROUP: Programming -> Tools -> Compilation
;; Compilation from Emacs
(defun prelude-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
      (interactive)
        ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
        (when (eq major-mode 'compilation-mode)
              (let ((inhibit-read-only t))
                      (ansi-color-apply-on-region (point-min) (point-max)))))

;; setup compilation-mode used by `compile' command
(require 'compile)
(setq compilation-ask-about-save nil          ; Just save before compiling
            compilation-always-kill t               ; Just kill old compile processes before starting the new one
                  compilation-scroll-output 'first-error) ; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; GROUP: Programming -> Tools -> Makefile
;; takenn from prelude-c.el:48: https://github.com/bbatsov/prelude/blob/master/modules/prelude-c.el
(defun prelude-makefile-mode-defaults ()
    (whitespace-toggle-options '(tabs))
      (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                               (run-hooks 'prelude-makefile-mode-hook)))

;;; project management
;; Package: projectile
;; Prefix key of Projectile is C-c p. see help
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; (require 'helm-projectile)
;; (helm-projectile-on)
;; (setq projectile-completion-system 'helm)
;; (setq projectile-indexing-method 'alien)

;;; speedbar
;; Package sr-speedbar
;; sr-speedbar-toggle: toggle speedbar. other C-x 1, C-x o
(setq sr-speedbar-skip-other-window-p t)


;; ;;; version control
;; ;;; ----------------

;; ;; GROUP: Programming -> Tools -> Magit
;; ;; Package: Magit
;; (require 'magit)
;; (set-default 'magit-stage-all-confirm nil)
;; (add-hook 'magit-mode-hook 'magit-load-config-extensions)

;; ;; full screen magit-status
;; (defadvice magit-status (around magit-fullscreen activate)
;;   (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))

;; (global-unset-key (kbd "C-x g"))
;; (global-set-key (kbd "C-x g h") 'magit-log)
;; (global-set-key (kbd "C-x g f") 'magit-file-log)
;; (global-set-key (kbd "C-x g b") 'magit-blame-mode)
;; (global-set-key (kbd "C-x g m") 'magit-branch-manager)
;; (global-set-key (kbd "C-x g c") 'magit-branch)
;; (global-set-key (kbd "C-x g s") 'magit-status)
;; (global-set-key (kbd "C-x g r") 'magit-reflog)
;; (global-set-key (kbd "C-x g t") 'magit-tag)


;; ;; add verson control information to ibuffer
;; Package: ibuffer-vc
;; (add-hook 'ibuffer-hook
;;          (lambda ()
;;           (ibuffer-vc-set-filter-groups-by-vc-root)
;;           (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                   (ibuffer-do-sort-by-alphabetic))))

;; (setq ibuffer-formats
;;       '((mark modified read-only vc-status-mini " "
;;                 (name 18 18 :left :elide)
;;                 " "
;;                 (size 9 -1 :right)
;;                 " "
;;                 (mode 16 16 :left :elide)
;;                 " "
;;                 (vc-status 16 16 :left)
;;                 " "
;;                 filename-and-process)))

(provide 'setup-programming)
