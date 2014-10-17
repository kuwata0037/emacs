;; --------------------------------------------------
;; expand-region
;; --------------------------------------------------
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-;") 'er/expand-region)
  (global-set-key (kbd "C-'") 'er/contract-region)
  )
