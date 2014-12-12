;; --------------------------------------------------
;; ユーザ定義関数
;; --------------------------------------------------
(defun my/killregion-or-deletewindow ()
  "範囲指定時に kill-region, それ以外は delete-window を行う"
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-window)))
(global-set-key (kbd "C-w") 'my/killregion-or-deletewindow)


;; --------------------------------------------------
;; キーバインド
;; --------------------------------------------------
;; 改行と同時にインデント (初期値は newline)
(global-set-key (kbd "C-m") 'newline-and-indent)

;; 折り返しトグル
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)

;; "C-t" でウィンドウ切り替え or 画面分割
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; "C-S-f" に forward-word コマンドを
(global-set-key (kbd "C-S-f") 'forward-word)
;; "C-S-b" に backward-word コマンドを
(global-set-key (kbd "C-S-b") 'backward-word)

;; Command+R で再読み込み
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-r") 'revert-buffer-no-confirm))


;; --------------------------------------------------
;; エイリアス
;; --------------------------------------------------
(defalias 'dtw 'delete-trailing-whitespace)
