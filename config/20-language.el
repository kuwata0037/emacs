;;; --- Language ---
;;; C++ [TODO]
(defun my/c++-mode-hooks ()
  )


;;; Ruby
(use-package ruby-mode
  :commands    ruby-mode
  :interpreter ("rbenv" . ruby-mode)
  :config
  (setq ruby-indent-level 2)
  (setq ruby-indent-tabs-mode nil)
  (setq ruby-deep-indent-paren-style nil)
  ;; rbenv
  (use-package rbenv
    :config
    (global-rbenv-mode)
    (set-face-attribute 'rbenv-active-ruby-face nil
                        :foreground "blue"))
  ;; ruby-block
  (use-package ruby-block
    :config
    (ruby-block-mode)
    (setq ruby-block-highlight-toggle t))
  ;; ruby-end
  (use-package ruby-end)
  ;; inf-ruby
  (use-package inf-ruby
    :config
    (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
    (setq inf-ruby-default-implementation "pry"))
  ;; robe
  (use-package robe
    :if (executable-find "pry")
    :config
    (autoload 'ac-robe-setup  "ac-robe" "auto-complete robe" nil nil)
    (add-hook 'ruby-mode-hook 'robe-mode t)
    (add-hook 'robe-mode-hook 'ac-robe-setup)
    (add-hook 'robe-mode-hook 'robe-start t)))


;;; HTML, CSS
;; web-mode (@refer : http://yanmoo.blogspot.jp/2013/06/html5web-mode.html)
(use-package web-mode
  :mode (("\\.html?$"     . web-mode)
         ("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode))
  :config
  ;; tab
  (setq tab-width 2)
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-css-indent-offset    tab-width)
  (setq web-mode-code-indent-offset   tab-width)
  (setq web-mode-style-padding        tab-width)
  (setq web-mode-script-padding       tab-width)
  (setq web-mode-block-padding        tab-width)
  ;; color
  (custom-set-faces
   '(web-mode-doctype-face              ; doctype
     ((t (:foreground "#82AE46"))))
   '(web-mode-html-tag-face             ; 要素名
     ((t (:foreground "#E6B422" :weight bold))))
   '(web-mode-html-attr-name-face       ; 属性名など　
     ((t (:foreground "#C97586"))))
   '(web-mode-html-attr-value-face      ; 属性値
     ((t (:foreground "#82AE46"))))
   '(web-mode-comment-face              ; コメント
     ((t (:foreground "#D9333F"))))
   '(web-mode-server-comment-face       ; コメント
     ((t (:foreground "#D9333F"))))
   '(web-mode-css-rule-face             ; cssのタグ
     ((t (:foreground "#A0D8EF"))))
   '(web-mode-css-pseudo-class-face     ; css疑似クラス
     ((t (:foreground "#FF7F00"))))
   '(web-mode-css-at-rule-face          ; cssのタグ
     ((t (:foreground "#FF7F00"))))))


;;; Lisp
(defun my/elisp-mode-hooks ()
  "lisp-mode-hooks"
  (use-package eldoc
    :config
    (eldoc-mode)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (use-package eldoc-extension)))
(add-hook 'emacs-lisp-mode-hook       'my/elisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'my/elisp-mode-hooks)


;;; Yaml
(use-package yaml-mode
  :mode   ("\\.ya?ml$" . yaml-mode)
  :config (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))


;;; Latex
(use-package yatex
  :mode (("\\.tex$" . yatex-mode)
         ("\\.ltx$" . yatex-mode)
         ("\\.cls$" . yatex-mode)
         ("\\.sty$" . yatex-mode)
         ("\\.clo$" . yatex-mode)
         ("\\.bbl$" . yatex-mode))
  :config
  (setq YaTeX-inhibit-prefix-letter t)  ; "C-c" から "C-c C-" へ変更
  (setq YaTeX-use-LaTeX2e t)
  (setq YaTeX-use-AMS-LaTeX t)
  (setq YaTeX-kanji-code 4)    ; 文章作成時の漢字コードを utf-8 にする
  (setq YaTeX-latex-message-code 'utf-8)
  ;; (add-hook 'yatex-mode-hook '(lambda ()(setq auto-fill-function nill))) ; 自動改行を抑制
  (setq YaTeX-help-file
        (expand-file-name (concat user-emacs-directory "package/el-get/yatex/help/YATEXHLP.jp"))) ; help ファイル設定
  ;; タイプセット
  (setq tex-command "platex")
  (setq dviprint-command-format "var=`echo %s | sed -e \"s/\.[^.]*$/.dvi/\"` && dvipdfmx $var")
  (setq dvi2-command "open -a Skim")
  (setq bibtex-command "pbibtex")
  ;; reftex-mode
  (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
  (add-hook 'Latex-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
  (add-hook 'yatex-mode-hook                  ; with YaTeX mode
          #'(lambda ()
              (reftex-mode t)
              (define-key reftex-mode-map
                (concat YaTeX-prefix ">") 'YaTeX-comment-region)
              (define-key reftex-mode-map
                (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
  ;; 数式モードの ";" 補間の強化
  (setq YaTeX-math-sign-alist-private
        '(("q"         "quad"          "__")
          ("qq"        "qquad"         "____")
          ("ls"        "varlimsup"     "___\nlim")
          ("li"        "varliminf"     "lim\n---")
          ("il"        "varinjlim"     "lim\n-->")
          ("st"        "text{ s.~t. }" "s.t.")
          ("bigop"     "bigoplus"      "_\n(+)~")
          ("bigot"     "bigotimes"     "_\n(x)\n ~")
          ("pl"        "varprojlim"    "lim\n<--")
          ))
  ;; 数式モードの "," 補間
  (setq YaTeX-math-funcs-list
        '(("s"         "sin"           "sin")
		  ("c"         "cos"           "cos")
          ("t"         "tan"           "tan")
          ("hs"        "sinh"          "sinh")
          ("hc"        "cosh"          "cosh")
          ("ht"        "tanh"          "tanh")
          ("S"         "arcsin"        "arcsin")
          ("C"         "arccos"        "arccos")
          ("T"         "arctan"        "arctan")
          ("se"        "sec"           "sec")
          ("cs"        "csc"           "csc")
          ("cot"       "cot"           "cot")
          ("l"         "ln"            "ln")
          ("L"         "log"           "log")
          ("e"         "exp"           "exp")
          ("M"         "max"           "max")
          ("m"         "min"           "min")
          ("su"        "sup"           "sup")
          ("in"        "inf"           "inf")
          ("di"        "dim"           "dim")
          ("de"        "det"           "det")
          ("i"         "imath"         "i")
          ("j"         "jmath"         "j")
          ("I"         "Im"            "Im")
          ("R"         "Re"            "Re")
          ))
  (setq YaTeX-math-key-list-private
        '(("," . YaTeX-math-funcs-list))))


;;; Org
(use-package org-mode
  :commands org-mode)


;;; Markdown
(use-package markdown-mode
  :mode ("\\.md$" . markdown-mode))
