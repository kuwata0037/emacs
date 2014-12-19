;; --------------------------------------------------
;; 基本設定
;; --------------------------------------------------
;; 初期ディレクトリをホームディレクトリへ
;; @refer : http://milanista224.blogspot.jp/
(cd "~/")

;; yes or no を y or n へ
(fset 'yes-or-no-p 'y-or-n-p)

;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)          ; CUAキーバインドを無効にする

;; 行間の指定
;; (setq-default line-spacing 0)

;; スクロールを 1 行ずつに設定
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ; shell-mode

;; 画像ファイルを表示
(auto-image-file-mode t)

;; ファイルを自動で再読み込み
(global-auto-revert-mode t)

;; 大文字と小文字を区別しない
(setq read-file-name-completion-ignore-case t)

;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; ビープ音を消音
;; @refer : http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#bell
(setq visible-bell t)

;; Emacs で削除したファイルをゴミ箱へ
;; @refer : http://qiita.com/yasuyk/items/d5d7190a256e72451543
(setq delete-by-moving-to-trash t
      trash-directory (expand-file-name "~/.Trash"))

;; ファイル名が重複してたらディレクトリ名を追加する
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; gz ファイルも編集可能に
(auto-compression-mode t)
