;; --------------------------------------------------
;; org
;; --------------------------------------------------
(defun org-mode-hooks ()
  ;; org-modeでの"C-tab"を無効化
  ;; @refer : http://d.hatena.ne.jp/whitypig/20101116/1289858757
  (define-key org-mode-map (kbd "<C-tab>") nil)
  )
(add-hook 'org-mode-hook 'org-mode-hooks)
