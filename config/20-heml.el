;; --------------------------------------------------
;; helm
;; --------------------------------------------------
(when (require 'helm-config nil t)
  ;; 有効化
  (helm-mode t)

  ;; describe-binding を helm で表示
  (helm-descbinds-mode t)

  ;; 処理を変更したいコマンドの登録
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

  ;; キーバインド
  (global-set-key (kbd "C-c h") 'helm-mini)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-x") 'helm-M-x)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  )
