;;; Programming -> common
;;; ------------------------

;; GROUP: Programming -> Tools -> Ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;; source code editing
;;; --------------------

;; hs-minor-mode allow users to fold and hide blocks of text
;; local key binding: C-c @
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Narrowing
;; C-x n d/r/n/w

;; Indentation
;; c/c++ style
;;; GROUP: Programming -> Languages -> C/C++
(setq c-default-style
      `((java-mode . "java")
        (awk-mode . "awk")
        (other . "gnu"))  ; set default style to "gnu" 
      c-basic-offset 4)

;; Redefine the key: RET
(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;;; whitespace auto-tailling
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


;;; Code template
;; Package: yasnippet
;; you can customize the template
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
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; set parents pares by building variable
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)


;;; Comment
;; Package: comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;;; search and replace
;; PACKAGE: anzu
;; GROUP: Editing -> Matchnning -> Isearch -> Anzu
(require 'anzu)
(global-anzu-mode)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; ;; PACKAGE: duplicate-thing
;; (require 'duplicate-thing)
;; (global-set-key (kbd "M-c") 'duplicate-thing)


;;; Code completion
;;; -----------------------

;; General completion using  company
;; company-lang for system header and ggtags for project header, semantic for system header
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Header file completion using company-c-headers
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
;; To enable C++ header completion for standard libraries, you have to add its path, for example, like this:
;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
;; To  complete project local, use company-c-headers-path-user and put it in .dir-locals.el.

;; source code completion using Clang
;; complete for int void,and so on.
;; command:company-clang
;; to include project file, please create .dir-locals.el file in the project root directory
;; (delete 'company-semantic company-backends)
(require 'cc-mode)
(define-key c-mode-map  [(control tab)] 'company-complete) ;use semantics
(define-key c-mode-map  [(control tab)] 'company-complete) ;use semantics
;; (define-key c-mode-map  (kbd "TAB") 'company-complete)
;; (define-key c++-mode-map  (kbd "TAB") 'company-complete)

;; souce code completion using ggtags
;; company-gtags is behind of company-semantic, company-clang in the variable company-backends.

;; use CEDET
;; includes common features such as intelligent completion, source code navigation, project management, code generation with templates
;; does keep track changes in real time;
;; best used with new project, viable choice for small to  moderate-size source files.

(require 'semantic)
;; Semantic automatically includes some default system include paths such as /usr/include, /usr/local/include . and so on,
;; To add more include paths to var:semantic-dependency-system-include-path, for example Boost include paths, like this:
;; (semantic-add-system-include "/usr/include/boost" 'c++-mode)
;; (semantic-add-system-include "~/linux/kernel")
;; (semantic-add-system-include "~/linux/include")
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)


(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; (define-key c-mode-map  (kbd "TAB") 'company-semantic)
;; (define-key c++-mode-map  (kbd "TAB") 'company-semantic)

;; compltion using company-mode
;; command:company-semantic that uses SemanticDB to retrieve completion candidates

;; Source code navigation using Senator
;; key binding prefix to: C-c ,

;;; source code information
;; displays function interface in the minibuffer
(global-semantic-idle-summary-mode 1)
;; shows the function point is currently in at the first line of the current buffer
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (global-semantic-stickyfunc-mode 1)
;; Using ggtags + eldoc, for  displaying function interface at point in minibuffer:
;; (setq-local eldoc-documentation-function 'ggtags-eldoc-function)
;; display the function of the point focus in modeline
;; (which-function-mode 1)

;; GROUP: Programming -> Tools -> Fly-check
;; PACKAGE: flycheck
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)


;;; Source code navigation
;;; --------------------
;; using ggtags
(require 'setup-ggtags)
;; (require 'setup-helm)
;; (require 'setup-helm-gtags)

;; Find Definitions in current buffer
(setq-local imenu-create-index-function 'ggtags-build-imenu-index)

;; Find definitions in project
;; using ggtags, key binding M-. and  M-,
;; If ggtags gives you a list of candidates, you can use M-n to move to next candidate and M-p to move back previous candidate.
;; Use M-g s to invoke Isearch on candidate buffer list.

;; Find files in project
;; using ggtags, M-x ggtags-find-file

;; View visited tags with tag stack
;; using ggtags, jump back pop-tag-mark(M-,),
;; view the history of visited tag, ggtags-view-tag-history(C-c g h)

;; Navigate system include path
;; Using Semantic with semantic-ia-fast-jump command

;; Using generated database from GNU Global
;; see the pre-setup-emacs-ide file

;;; Browse source files tree-down
;; Package sr-speedbar
(require 'sr-speedbar)
;; command:sr-speedbar-toggle.  move focus window, using M-x windmove-left/right/down/up
(setq sr-speedbar-skip-other-window-p t)
;; (setq speedbar-show-unknown-files t)

;;; project management
;;; --------------------

;; Enable EDE only in C/C++
;; Obsolete commands
(require 'ede)
(global-ede-mode)

;; Package: projectile
;; to mark a folder manually as a project just create an empty .projectile file in it
;; Prefix key of Projectile is C-c p. see help
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; (require 'helm-projectile)
;; (helm-projectile-on)
;; (setq projectile-completion-system 'helm)
;; (setq projectile-indexing-method 'alien)



;;; compile and debug
;;; -------------------

;; GROUP: Programming -> Tools -> Makefile
;; takenn from prelude-c.el:48: https://github.com/bbatsov/prelude/blob/master/modules/prelude-c.el
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))

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

;; GROUP: Programming -> Tools -> Gdb
(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)    ; Non-nil means display source file containing the main routine at startup


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
