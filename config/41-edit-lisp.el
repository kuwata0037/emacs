;; --------------------------------------------------
;; lisp
;; --------------------------------------------------
(use-package cl)

(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (use-package eldoc
    :config
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode))
  )

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)
