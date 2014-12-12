;; --------------------------------------------------
;; auto-complete
;; @refer : http://cx4a.org/software/auto-complete/manual.ja.html
;; --------------------------------------------------
(use-package auto-complete-config
  :config
  (setq ac-delay 0.3)
  (setq ac-auto-show-menu 0.2)
  
  ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict/")
  (setq ac-comphist-file (my/set-history "ac-comphist.dat"))

  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

  ;; サンプル設定の有効化
  (ac-config-default)
  )
