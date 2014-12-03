;; --------------------------------------------------
;; ruby
;; --------------------------------------------------
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; インデント
(setq ruby-indent-level tab-width
      ruby-deep-indent-paren-style nil
      ruby-indent-tabs-mode nil
      )


;; 閉じ括弧をいい感じに
;; @refer : http://www.willnet.in/13
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))


;; robe
(when (and (require 'robe nil t) (executable-find "pry"))
  (add-hook 'ruby-mode-hook 'robe-mode)
  (autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
  (autoload 'ac-robe-setup "ac-robe" "auto-complete robe" nil nil)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  (add-hook 'ruby-mode-hook 'robe-start)
  )


;; ruby-end
;; @describe : 括弧の自動挿入
;; @refer : http://blog.livedoor.jp/ooboofo3/archives/53748087.html
(when (require 'ruby-end nil t)
  (defun my/ruby-end-hook ()
    (abbrev-mode t)
    (ruby-end-mode t))
  (add-hook 'ruby-mode-hook 'my/ruby-end-hook)
  )


;; ruby-block
;; @describe : endに対応する行のハイライト
;; @refer : http://d.hatena.ne.jp/khiker/20071130/emacs_ruby_block
(when (require 'ruby-block nil t)
  (defun my/ruby-block-hook ()
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t))
  (add-hook 'ruby-mode-hook 'my/ruby-block-hook)
  )


;; inf-ruby
;; @describe : インタラクティブRubyを利用する
;; @refer : https://github.com/nonsequitur/inf-ruby
(when (require 'inf-ruby nil t)
  (defun my/inf-ruby-hook ()
    ;; inf-ruby-mirror-mode を on に
    (inf-ruby-minor-mode)
    ;; rbenv への対応
    ;; @refer : http://itiut.hatenablog.com/entry/2013/05/30/021637
    (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:"
                           (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
    (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims")
                          (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))
    )
  (add-hook 'ruby-mode-hook 'my/inf-ruby-hook)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  )
