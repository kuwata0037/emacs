;; --------------------------------------------------
;; exec-path-from-shell
;; @refer : https://github.com/purcell/exec-path-from-shell
;; --------------------------------------------------
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
