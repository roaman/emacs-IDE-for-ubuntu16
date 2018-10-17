;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

;; Add and enable MELPA
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/"))
;;  '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar my-packages
  '(
    ;; setup-editing file
    anzu				;todo error
    comment-dwim-2                      ;comment
    volatile-highlights
    duplicate-thing
;;    undo-tree
    clean-aindent-mode
    dtrt-indent
    ws-butler
    iedit
    smartparens
    zygospore
    ;; setup-faces file
    zenburn-theme                      ;theme
    ;; setup-programming file
    flycheck
    yasnippet                           ;template system for Emacs
    ;; IDE for C/C++
    company
    company-c-headers
    ggtags
    ;; IDE for python
    ;; ein
    elpy
    py-autopep8
    ;; magit
    projectile
    sr-speedbar
    ;; helm
    ;; helm-gtags
    ;; helm-projectile
    ;; helm-swoop
    ;; ;; function-args
))

(setq debug-on-error t)

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(package-initialize)
(install-packages)

;; add your modules path
(add-to-list 'load-path "~/.emacs.d/custom/")

;; load your modules
(require 'setup-environment)
(require 'setup-faces)
(require 'setup-convenience)
(require 'setup-files)
(require 'setup-editing)
;; (require 'setup-text)
(require 'setup-data)
(require 'setup-communication)
(require 'setup-external)
(require 'setup-applications)
;; (require 'setup-local)
;; (require 'setup-help)
;; develope
(require 'setup-cedet)
(require 'setup-development)
(require 'setup-programming)            ;for C/C++  programming
(require 'setup-ggtags)
;; (require 'setup-helm)
;; (require 'setup-helm-gtags)
(require 'setup-programming-python)	    ;for python programming

;; (require 'setup-org)                     ;for org-mode
