;; -------------------------------
(setq org-startup-indented t)

;; -------------------------
;;;  image for org-mode
; 1. suspend current emacs window
; 2. call scrot to capture the screen and save as a file in $HOME/.emacs.img/
; 3. put the png file reference in current buffer, like this:  [[/home/path/.emacs.img/1q2w3e.png]]


; C-c C-x C-v org-toggle-inline-images

(add-hook 'org-mode-hook 'iimage-mode) ; enable iimage-mode for org-mode
(defun my-screenshot ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  (if (file-accessible-directory-p (concat (file-name-directory (buffer-file-name)) "images/"))
      nil
    (make-directory "images"))
  (setq filename
        (concat (make-temp-name
                 (concat  (file-name-directory (buffer-file-name))  "/images/" )) ".png"))
;;  (suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil " -s " (concat "\"" filename "\"" ))
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images)
 )

(global-set-key (kbd "C-C s") 'my-screenshot)

;; ---------------------------
;; export pdf

(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
(require 'org)
(require 'ox-latex)

;; 使用xelatex一步生成PDF
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f"))

;; code执行免应答（Eval code without confirm）
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-latex-classes
             '("cjk-article"
               "\\documentclass[10pt,a4paper]{article}
\\usepackage{xeCJK}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{listings}
\\usepackage[colorlinks=true,CJKbookmarks=true,
linkcolor=blue,
urlcolor=blue,
menucolor=blue]{hyperref}
\\setCJKmainfont[Scale=0.9]{AR PL SungtiL GB}
\\setCJKmonofont[Scale=0.9]{AR PL SungtiL GB}
\\hypersetup{unicode=true}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                                   ("\\section{%s}" . "\\section*{%s}")
                                   ("\\subsection{%s}" . "\\subsection*{%s}")
                                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



;; (setq org-Latex-packages-alist '(
;;                                         (""   "CJK"   t)
;;                                         (""     "indentfirst"  nil)
;;                                         ("pdftex"     "graphicx"  t)
;;                                         ("CJKbookmarks=true"     "hyperref"  nil)
;;                                         "%% Define a museincludegraphics command, which is
;; %%   able to handle escaped special characters in image filenames.
;; \\def\\museincludegraphics{%
;;   \\begingroup
;;   \\catcode`\\\|=0
;;   \\catcode`\\\\=12
;;   \\catcode`\\\#=12
;;   \\includegraphics[width=0.75\\textwidth]
;; }"))


;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-export-latex-listings t)


(provide 'setup-org)

