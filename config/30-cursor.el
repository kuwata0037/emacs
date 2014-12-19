;; --------------------------------------------------
;; smart-newline
;; @describe : いい感じに改行してくれる
;; @refer : http://ainame.hateblo.jp/entry/2013/12/08/162032
;; --------------------------------------------------
(use-package smart-newline
  :config
  (smart-newline-mode)
  (global-set-key (kbd "C-m") 'smart-newline)
  )
