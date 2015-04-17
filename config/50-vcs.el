;; --------------------------------------------------
;; VC 設定
;; --------------------------------------------------
;; シンボリックリンク先を開くときの質問しない
(setq vc-follow-symlinks t)


;; --------------------------------------------------
;; magit
;; --------------------------------------------------
(use-package magit
  :if (executable-find "git")
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  ;; ;; 色の指定
  ;; (set-face-foreground 'magit-diff-add "#adff2f")
  ;; (set-face-background 'magit-diff-add "#000000")
  ;; (set-face-foreground 'magit-diff-del "#ff6347")
  ;; (set-face-background 'magit-diff-del "#000000")
  ;; (set-face-foreground 'magit-diff-file-header "#4040ff")
  ;; (set-face-background 'magit-diff-file-header "#000000")
  ;; (set-face-foreground 'magit-diff-hunk-header "#f1f1f1")
  ;; (set-face-background 'magit-diff-hunk-header "#000000")
  ;; (set-face-foreground 'magit-item-highlight "#ffffff")
  ;; (set-face-background 'magit-item-highlight "#000000")
  )
