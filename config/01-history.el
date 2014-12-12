;; --------------------------------------------------
;; バックアップファイル
;; --------------------------------------------------
;; バックアップファイルを作成しない
;; (setq make-backup-files nil)

;; バックアップファイルの保存先を指定
(add-to-list 'backup-directory-alist
             (cons "." (my/set-history "backup/")))

;; TRAMPでバックアップファイルを作成しない
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))


;; --------------------------------------------------
;; オートセーブファイル
;; --------------------------------------------------
;; オートセーブファイルを作成しない
;; (setq auto-save-files nil)
;; オートセーブファイルのリストファイルを作成しない
;; (setq auto-save-list-file-name nil)

;; オートセーブファイルとリストファイルの保存先を指定
(defvar my/auto-save-directory "auto-save/")
(setq auto-save-file-name-transforms
      `((".*" ,(my/set-history my/auto-save-directory) t)))
(setq auto-save-list-file-prefix
      (my/set-history (concat my/auto-save-directory ".saves-")))

;; オートセーブファイル作成までの感覚 [sec]
(setq auto-save-timeout 15)
;; オートセーブファイル作成までのタイプ数
(setq auto-save-interval 60)


;; --------------------------------------------------
;; recentf
;; @describe : 最近開いたファイルの表示
;; @refer    : http://d.hatena.ne.jp/tomoya/20110217/1297928222
;; --------------------------------------------------
;; 履歴の保存先
(setq recentf-save-file (my/set-history "recentf"))

(use-package recentf
  :config
  ;; 最近使用したファイルの表示数
  (setq recentf-max-menu-items 10)
  ;; 最近使用したファイルの保存数
  (setq recentf-max-saved-items 2000)
  ;; recentfは含めない
  (setq recentf-exclude '("recentf"))
  (setq recentf-auto-cleanup 60)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 120 t 'recentf-save-list))
  (recentf-mode t))


;; --------------------------------------------------
;; save-place
;; @describe : 閉じる直前のカーソルの位置の履歴
;; --------------------------------------------------
(use-package saveplace
  :config
  (setq save-place-file (my/set-history "saveplace"))
  (setq-default save-place t)
  )


;; --------------------------------------------------
;; ミニバッファの履歴
;; --------------------------------------------------
;; 保存先
(setq savehist-file (my/set-history "savehist"))
;; ミニバッファの履歴を保存する
(savehist-mode t)
;; ミニバッファの履歴の保存数
(setq history-length 3000)
;; 再起動後も履歴の引き継ぎ
;; @refer : https://gist.github.com/kuwata0037/4470602f1b99467d108f#file-add-extended-command-history-el
(when (not (find 'extended-command-history savehist-minibuffer-history-variables))
    (add-to-list 'savehist-minibuffer-history-variables 'extended-command-history))


;; --------------------------------------------------
;; Tramp
;; --------------------------------------------------
(setq tramp-persistency-file-name (my/set-history "tramp"))


;; --------------------------------------------------
;; cookie
;; --------------------------------------------------
(setq url-cookie-file (my/set-history "cookies"))
