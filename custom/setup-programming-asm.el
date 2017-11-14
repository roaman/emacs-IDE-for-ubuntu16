;;; assembly for nasm

(autoload 'nasm-mode "nasm-mode" "x86 assembly editing mode." t)
   (setq auto-mode-alist (cons '("\\.asm$" . nasm-mode) auto-mode-alist))

(add-hook 'asm-mode-set-comment-hook (lambda() (setq asm-comment-char ?\#)))


(provide 'setup-programming-asm)
