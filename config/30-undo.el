;; --------------------------------------------------
;; point-undo
;; --------------------------------------------------
(use-package point-undo
  :config
  (global-set-key (kbd "M-[") 'point-undo)
  (global-set-key (kbd "M-]") 'point-redo)
  )


;; --------------------------------------------------
;; undo-tree
;; --------------------------------------------------
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "C-.") 'undo-tree-redo)
  )


;; --------------------------------------------------
;; undohist
;; --------------------------------------------------
(use-package undohist
  :config
  (setq undohist-directory (my/set-history "undohist/"))
  (undohist-initialize)
  )
