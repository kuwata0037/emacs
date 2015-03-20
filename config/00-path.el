;; --------------------------------------------------
;; exec-path-from-shell
;; @refer : https://github.com/purcell/exec-path-from-shell
;; --------------------------------------------------
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  )
