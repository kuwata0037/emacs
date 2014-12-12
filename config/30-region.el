;; --------------------------------------------------
;; expand-region
;; --------------------------------------------------
(use-package expand-region
  :config
  (global-set-key (kbd "C-;") 'er/expand-region)
  (global-set-key (kbd "C-'") 'er/contract-region)
  )
