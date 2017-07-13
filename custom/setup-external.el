;; GROUP: Processes -> Flyspell
;; this require aspell application installed.
(if (executable-find "aspell")
        (progn
                (setq ispell-program-name "aspell")
                (ispell-change-dictionary "american" t)
                (setq ispell-extra-args '("--sug-mode=ultra")))
    (setq ispell-program-name "ispell"))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; GROUP: Processes -> Gud
(setq gud-chdir-before-run nil)


(provide 'setup-external)

