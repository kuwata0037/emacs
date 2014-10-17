;; --------------------------------------------------
;; point-undo
;; --------------------------------------------------
(when (require 'point-undo nil t)
  (global-set-key (kbd "M-[") 'point-undo)
  (global-set-key (kbd "M-]") 'point-redo)
  )


;; --------------------------------------------------
;; undo-tree
;; --------------------------------------------------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "C-.") 'undo-tree-redo)
  )


;; --------------------------------------------------
;; undohist
;; --------------------------------------------------
(when (require 'undohist nil t)
  (setq undohist-directory (my/set-history "undohist/"))
  (undohist-initialize)
  )
