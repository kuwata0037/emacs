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

;; @refer : http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
;; 行頭/行の文字の始まる位置に移動
(defun my/goto-line-beginning-or-indent (&optional $position)
  (interactive)
  (or $position (setq $position (point)))
  (let (($starting-position (progn (back-to-indentation) (point))))
    (if (eq $starting-position $position)
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'my/goto-line-beginning-or-indent)

;; カーソル位置のフェイス情報を表示
(defun my/get-face (&optional $point)
  (interactive)
  (or $point (setq $point (point)))
  (let (($face (or (get-char-property $point 'read-face-name)
                   (get-char-property $point 'face))))
    (if $face
        (message (format "%s" $face))
      (message "no face"))))

;; 現在バッファのファイルのフルパスを取得
(defun my/get-current-path ()
  (interactive)
  (or (buffer-file-name) (expand-file-name default-directory)))

;; バッファのテキストエンコーディングを表示
(defun my/show-text-encoding ()
  (interactive)
  (message (format "%s" buffer-file-coding-system)))

;; 指定したフォントの有無を確認
(defun my/font-existsp ($font-name)
    (if (null (x-list-fonts $font-name))
        nil t))

;; 背景色を取得
(defun my/get-background-color ()
  (interactive)
  (princ (face-attribute 'default :background)))

;; ランダムに色を生成
(defun my/random-color ()
  (interactive)
  (format "#%02x%02x%02x"
          (cl-random 255)
          (cl-random 255)
          (cl-random 255)))

;; 16進数値の色をRGB値に変換
(defun my/convert-hex-color-to-rgb ($hex)
  (interactive "sHEX: ")
  (let ($result)
    (when (string-match
           "^\\s-*\\#\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\s-*$"
           $hex)
      (let (($m1 (match-string 1 $hex))
            ($m2 (match-string 2 $hex))
            ($m3 (match-string 3 $hex)))
        (setq $result (list (read (format "#x%s%s" $m1 $m1))
                            (read (format "#x%s%s" $m2 $m2))
                            (read (format "#x%s%s" $m3 $m3))))))
    (when (string-match
           "^\\s-*\\#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\s-*$"
           $hex)
      (setq $result (list (read (format "#x%s" (match-string 1 $hex)))
                          (read (format "#x%s" (match-string 2 $hex)))
                          (read (format "#x%s" (match-string 3 $hex))))))
    $result))

;; 色の名称を16進数値に変換
(defun my/convert-color-name-to-hex ($color-name)
  (let (($rgb) ($hex "#"))
    (mapc (lambda (x)
            (setq $rgb (cons (round (* x 255)) $rgb)))
          (color-name-to-rgb $color-name))
    (setq $rgb (nreverse $rgb))
    (mapc (lambda (x)
              (setq $hex (concat $hex (format "%02x" x))))
            $rgb)
    $hex))

;; カーソルの色を取得
(defun my/get-cursor-color ()
  (car (cl-loop for ($k . $v) in (frame-parameters)
                if (eq $k 'cursor-color)
                collect $v)))

;; 後述の置換に使用
(defun my/replace-strings-in-region-by-list ($list)
  "Replace strings in a region according to $list"
  (if mark-active
      (let* (($beg (region-beginning))
             ($end (region-end))
             ($word (buffer-substring-no-properties $beg $end)))
        (mapc (lambda ($r)
                (setq $word (replace-regexp-in-string (car $r) (cdr $r) $word)))
              $list)
        (delete-region $beg $end)
        (insert $word))
    (error "Need to make region")))

;; 選択範囲の全角数字を半角数字に置換
(defun my/convert-to-single-byte-number ()
  "Convert multi-byte numbers in region into single-byte number"
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("１" . "1")
     ("２" . "2")
     ("３" . "3")
     ("４" . "4")
     ("５" . "5")
     ("６" . "6")
     ("７" . "7")
     ("８" . "8")
     ("９" . "9")
     ("０" . "0"))))

;; 選択範囲の半角数字を全角数字に置換
(defun my/convert-to-multi-byte-number ()
  "Convert multi-byte numbers in region into single-byte number"
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("1" ."１")
     ("2" ."２")
     ("3" ."３")
     ("4" ."４")
     ("5" ."５")
     ("6" ."６")
     ("7" ."７")
     ("8" ."８")
     ("9" ."９")
     ("0" ."０"))))

;; 選択範囲の全角記号を半角記号に置換
(defun my/convert-yakumono-to-half-width ()
  "Replace multi byte punctuation marks to half width chars"
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("、" . "､")
     ("。" . "｡")
     ("「" . "｢")
     ("」" . "｣")
     ("［" . "[")
     ("］" . "]")
     ("｛" . "{")
     ("｝" . "}")
     ("（" . "(")
     ("）" . ")")
     ("・" . "･"))))

;; Mac: 現在のフォルダ以下の DS_Store を一掃
(defun my/delete-DS_Store-under-current-directory-recursively ()
  (interactive)
  (shell-command "find . -name '*.DS_Store' -type f -delete")
  (if (eq major-mode 'dired-mode)
      (revert-buffer)))


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
