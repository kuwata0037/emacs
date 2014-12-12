;; --------------------------------------------------
;; 文字コードの設定
;; --------------------------------------------------
;; ロケールを日本に
(set-language-environment "Japanese")


;; utf-8の優先順位の一番上に
(prefer-coding-system 'utf-8)


;; Mac OS Xの場合のファイル名の設定
(use-package ucs-normalize
  :if (eq system-type 'darwin)
  :config
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))


;; Windowsの場合のファイル名の設定
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))
