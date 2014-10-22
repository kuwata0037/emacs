;; --------------------------------------------------
;; 画面表示の設定
;; --------------------------------------------------
;; 初期画面を表示しない
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ
;; (setq initial-scratch-message "")

;; カーソルの点滅感覚
;; (blink-cursor-mode 0)

;; 起動時のディスプレイサイズ
(when window-system
  (set-frame-height (next-frame) 50)
  (set-frame-width (next-frame) 88))

;; ツールバー
(tool-bar-mode 0)

;; メニューバー
(menu-bar-mode 0)

;; スクロールバー
(set-scroll-bar-mode nil)

;; タイトルバー
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号
(global-linum-mode 0)
(setq linum-format "%3d")
;; (set-face-attribute 'linum nil
;;                     :foreground "#800"
;;                     :height 0.9)

;; 列番号
(column-number-mode t)

;; ファイルサイズ
(size-indication-mode t)

;; 時計 (フォーマット変更可)
(setq display-time-day-and-date t)      ; 曜日・月・日を表示
(setq display-time-24hr-format t)       ; 24時表示
(display-time-mode t)

;; バッテリー残量
(when (>= (string-to-number emacs-version) 24.4)
(display-battery-mode t))


;; --------------------------------------------------
;; 色の設定
;; @memo : "list-color-display" で色一覧
;; --------------------------------------------------
;; color-theme
(when (not (require 'ujelly-theme nil t))
  (when (require 'color-theme nil t)
    (color-theme-dark-laptop)))

;; ;; 現在行のハイライト
;; (defface my/hl-line-face
;;   ;; 背景が dark なら背景色を紺に
;;   '((((class color) (background dark))
;;      (:background "LightGoldenrodYellow" t))
;;     (t (:bold t)))
;;   "hl-line's my face")
;; (setq hl-line-face 'my/hl-line-face)
(global-hl-line-mode t)

;; 括弧の対応関係のハイライト
;; (setq show-paren-delay 0)
(show-paren-mode)
(setq show-paren-style 'expression)    ; expression は括弧内も強調表示
(set-face-background 'show-paren-match-face nil)
(set-face-underline 'show-paren-match-face "yellow")

;; 選択領域のハイライト
(set-face-background 'region "#500")

;; 無駄な行末空白を強調表示
;; (setq-default show-trailing-whitespace t)
;; (set-face-background 'trailing-whitespace "#b14770")

;; ;; カーソル位置のシンボルのハイライト
;; ;; @refer : http://d.hatena.ne.jp/yuheiomori0718/20111222/1324562208
;; (when (require 'auto-highlight-symbol nil t)
;;   (global-auto-highlight-symbol-mode t)
;;   ;; (ahs-set-idle-interval 0.8)
;;   )

;; ;; highlight-symbol
;; ;; @refer : http://qiita.com/aKenjiKato/items/f465993ac8e62db69592
;; (when (require 'highlight-symbol nil t)
;;   (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

;;   (global-set-key (kbd "C-]") 'highlight-symbol-at-point)
;;   (global-set-key (kbd "C-c C-]") 'highlight-symbol-remove-all)
;;   )


;; --------------------------------------------------
;; フォントの設定
;; --------------------------------------------------
;; 使用可能フォント一覧表示 : (prin1 (font-family-list))
(when (eq system-type 'darwin)
  ;; ascii のフォント設定
  (set-face-attribute 'default nil
                      :family "Ricty"
                      :height 140)
  ;; 日本語のフォント設定
  (set-fontset-font nil 'japanese-jisx0208
                    (font-spec :family "Hiragino Mincho Pro"))
  (set-fontset-font nil 'japanese-jisx0212
                    (font-spec :family "Hiragino Mincho Pro"))

  ;; 半角 : 全角を 1 : 2 に
  (setq face-font-rescal-alist
    '((".*Menlo.*" . 1.0)
      (".*Ricty.*" . 1.0)
      (".*Hiragino_Micho_Pro.*" . 1.2)
      ("-cdac$" . 1.3)))
  )
