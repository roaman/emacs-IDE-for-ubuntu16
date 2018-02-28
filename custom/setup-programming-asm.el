;;; assembly for nasm
;; nasm-mode
(autoload 'nasm-mode "nasm-mode" "x86 assembly editing mode." t)
   (setq auto-mode-alist (cons '("\\.asm$" . nasm-mode) auto-mode-alist))
;; asm-mode
(add-hook 'asm-mode-set-comment-hook (lambda() (setq asm-comment-char ?\#)))

;; (defun asm-calculate-indentation ()
;;   (or
;;    ;; Flush labels to the left margin.
;;    (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
;;    ;; Same thing for `;;;' comments.
;;    (and (looking-at "\\s<\\s<\\s<") 0)
;;    ;; Simple `;' comments go to the comment-column.
;;    (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
;;    ;; The rest goes at the first tab stop.
;;    (or (indent-next-tab-stop 0))))



(provide 'setup-programming-asm)
