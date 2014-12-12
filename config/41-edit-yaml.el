;; --------------------------------------------------
;; yaml
;; --------------------------------------------------
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent)
  )
