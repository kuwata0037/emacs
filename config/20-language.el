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


;;; Org
(use-package org-mode
  :commands org-mode)


;;; Markdown
(use-package markdown-mode
  :mode ("\\.md$" . markdown-mode))
