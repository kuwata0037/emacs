;;; --- General ---
;;; Require
(use-package cl)
(use-package color)
(use-package smartrep)
(use-package region-bindings-mode
  :config (region-bindings-mode-enable))
(use-package popwin
  :config (popwin-mode))


;;; Basic
(cd "~")
(auto-image-file-mode)
(auto-compression-mode)
(global-auto-revert-mode)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore) ; @refer : http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#bell
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))


;;; Path
(use-package exec-path-from-shell
  :if     (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))


;;; Coding System
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system   'cp932))
(use-package ucs-normalize
  :if (eq system-type 'darwin)
  :config
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system   'utf-8-hfs))


;;; Font (@memo : (prin1 (font-family-list)))
(when (eq system-type 'darwin)
  ;; ascii font
  (set-face-attribute 'default nil
                      :family "Ricty"
                      :height 140)
  ;; japanese font
  (set-fontset-font nil 'japanese-jisx0208
                    (font-spec :family "Hiragino Mincho Pro"))
  (set-fontset-font nil 'japanese-jisx0212
                    (font-spec :family "Hiragino Mincho Pro"))
  ;; (半角:全角) = (1:2)
  (setq face-font-rescal-alist
        '((".*Menlo.*"              . 1.0)
          (".*Ricty.*"              . 1.0)
          (".*Hiragino_Micho_Pro.*" . 1.2)
          ("-cdac$"                 . 1.3))))


;;; Cursor
(blink-cursor-mode)
(setq cursor-type 'box)
(use-package multiple-cursors
  :bind     ("C-c m" . mc/mark-all-like-this)
  :commands (mc/mark-previous-like-this
             mc/mark-next-like-this
             mc/skip-to-previous-like-this
             mc/skip-to-next-like-this
             mc/mark-more-like-this-extended)
  :init
  (bind-keys :map region-bindings-mode-map
             ("a" . mc/mark-all-like-this)
             ("p" . mc/mark-previous-like-this)
             ("n" . mc/mark-next-like-this)
             ("P" . mc/skip-to-previous-like-this)
             ("N" . mc/skip-to-next-like-this)
             ("m" . mc/mark-more-like-this-extended)))


;;; Scroll
(setq scroll-conservatively 35)
(setq next-screen-context-lines 1)
(setq comint-scroll-show-maximum-output t) ; shell-mode
(use-package smooth-scroll
  :config (smooth-scroll-mode))


;;; Frame
;; bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; margin
(global-linum-mode)
(setq linum-format "%3d: ")
(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.9)
(use-package hlinum
  :config (hlinum-activate))

;; mode line
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(setq display-time-day-and-date t)
(setq display-time-24hr-format  t)
(display-time-mode)
(display-battery-mode)
(add-to-list 'default-mode-line-format '(:eval (my/count-lines-and-chars)))


;;; ColorTheme (@memo : (list-colors-display))
(load-theme 'deeper-blue t)


;;; Highlight
;; current line
(defface my/hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景色がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my/hl-line-face)
(global-hl-line-mode)

;; parentheses
(show-paren-mode)
(setq show-paren-delay 0.125)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match-face nil
                    :foreground nil
                    :background nil
                    :underline "yellow")
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init     (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))

;; region
(set-face-background 'region "#500")

;; trailing whitespace
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; symbol
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode)
  (ahs-set-idle-interval 1.0))
(use-package highlight-symbol
  :commands highlight-symbol-mode
  :init     (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 1.0)
  (setq highlight-symbol-colors   '("DarkOrange" "DodgerBlue1" "DeepPink1"))
  (global-set-key (kbd "C-c C-;") 'highlight-symbol-at-point)
  (global-set-key (kbd "C-c C-'") 'highlight-symbol-remove-all))

;; color code
(use-package rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'web-mode-hook        'rainbow-mode)
  (add-hook 'css-mode-hook        'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; difference
(use-package volatile-highlights
  :config (volatile-highlights-mode))


;;; History (@refer : http://qiita.com/ShingoFukuyama/items/19b02cd1679a6ea0bfd)
(defvar my/history-directory
  (expand-file-name (concat user-emacs-directory ".history/")))
(defun my/set-history (&rest args)
  (expand-file-name (concat my/history-directory (mapconcat 'identity args ""))))

;; backup
;; (setq make-backup-files nil)
(add-to-list 'backup-directory-alist (cons "." (my/set-history "backup/")))

;; autosave
;; (setq auto-save-files nil)
;; (setq auto-save-list-file-name nil)
(defvar my/auto-save-directory "auto-save/")
(setq auto-save-file-name-transforms
      `((".*" ,(my/set-history my/auto-save-directory) t)))
(setq auto-save-list-file-prefix
      (my/set-history (concat my/auto-save-directory ".saves-")))
(setq auto-save-timeout  15)
(setq auto-save-interval 60)

;; recentf
(use-package recentf
  :config
  (setq recentf-save-file (my/set-history "recentf"))
  (setq recentf-max-menu-items    10)
  (setq recentf-max-saved-items 2000)
  (setq recentf-auto-cleanup      60)
  (setq recentf-exclude '("recentf"))
  (setq recentf-auto-save-timer
        (run-with-idle-timer 120 t 'recentf-save-list))
  (recentf-mode))

;; saveplace (@describe : 閉じる直前のカーソルの位置の履歴)
(use-package saveplace
  :config
  (setq save-place-file (my/set-history "saveplace"))
  (setq-default save-place t))

;; savehist (@describe : minibuffer history)
(use-package savehist
  :config
  (setq savehist-file (my/set-history "savehist"))
  (setq history-length 3000)
  (savehist-mode))

;; tramp [TODO]
(setq tramp-persistency-file-name (my/set-history "tramp"))
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; cookie
(setq url-cookie-file (my/set-history "cookies"))

;; trash
(setq delete-by-moving-to-trash t)
(setq trash-directory (expand-file-name "~/.Trash"))

;; undo
(use-package undohist
  :config
  (setq undohist-directory (my/set-history "undohist/"))
  (undohist-initialize))
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "C-.") 'undo-tree-redo))
(use-package point-undo
  :config
  (global-set-key (kbd "M-]") 'point-undo)
  (global-set-key (kbd "M-[") 'point-redo))


;;; Function (@refer : http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149)
;; utility
(defun my/count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(defun my/other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(defun my/killregion-or-deletewindow ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-window)))
(defun my/goto-line-beginning-or-indent (&optional position)
  (interactive)
  (or position (setq position (point)))
  (let ((starting-position (progn (back-to-indentation) (point))))
    (if (eq starting-position position)
        (move-beginning-of-line 1))))
(defun my/random-color ()
  (interactive)
  (format "#%02x%02x%02x"
          (cl-random 255)
          (cl-random 255)
          (cl-random 255)))
(defun my/delete-DS_Store-under-current-directory-recursively ()
  (interactive)
  (shell-command "find . -name '*.DS_Store' -type f -delete")
  (if (eq major-mode 'dired-mode)
      (revert-buffer)))

;; information
(defun my/font-exists-p (font-name)
  (if (null (x-list-fonts font-name))
      nil t))
(defun my/get-current-path ()
  (interactive)
  (or (buffer-file-name) (expand-file-name default-directory)))
(defun my/get-text-encoding ()
  (interactive)
  (message (format "%s" buffer-file-coding-system)))
(defun my/get-face (&optional point)
  (interactive)
  (or point (setq point (point)))
  (let ((face (or (get-char-property point 'read-face-name)
                  (get-char-property point 'face))))
    (if face
        (message (format "%s" face))
      (message "no face"))))
(defun my/get-cursor-color ()
  (car (cl-loop for (k . v) in (frame-parameters)
                if (eq k 'cursor-color)
                collect  v)))
(defun my/get-background-color ()
  (interactive)
  (princ (face-attribute 'default :background)))
(defun my/get-overlay-property-cons-list-at (&optional position)
  (interactive)
  (or position (setq position (point)))
  (let (list ov)
    (setq ov (overlay-properties (car (overlays-in position (1+ position)))))
    (mapc (lambda (elt)
            (let ((key   (car elt))
                  (value (cdr elt)))
              (setq list (cons (cons key value) list))))
          (let (ret)
            (while ov
              (setq ret (cons (cons (car ov) (cadr ov)) ret))
              (setq ov  (cddr ov)))
            ret))
    (message "%s" list)))

;; replace
(defun my/replace-strings-in-region-by-list (list)
  "Replace strings in a region according to list"
  (if mark-active
      (let* ((beg  (region-beginning))
             (end  (region-end))
             (word (buffer-substring-no-properties beg end)))
        (mapc (lambda (r)
                (setq word (replace-regexp-in-string (car r) (cdr r) word)))
              list)
        (delete-region beg end)
        (insert word))
    (error "Need to make region")))
(defun my/join-multi-lines-to-one ()
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("\\(\n\\s-*\\)+" . ""))))
(defun my/escape-in-region ()
  "Escape code in region"
  (interactive)
  (my/replace-strings-in-region-by-list
   '(("\t" . "  ")
     ("\<" . "&lt;")
     ("\>" . "&gt;")
     ("\&" . "&amp;")
     ("\'" . "&#039;")
     ("\"" . "&quot;"))))
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
(defun my/convert-hex-color-to-rgb (hex)
  (interactive "sHEX: ")
  (let (result)
    (when (string-match
           "^\\s-*\\#\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\s-*$"
           hex)
      (let (($m1 (match-string 1 hex))
            ($m2 (match-string 2 hex))
            ($m3 (match-string 3 hex)))
        (setq result (list (read (format "#x%s%s" $m1 $m1))
                           (read (format "#x%s%s" $m2 $m2))
                           (read (format "#x%s%s" $m3 $m3))))))
    (when (string-match
           "^\\s-*\\#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\s-*$"
           hex)
      (setq result (list (read (format "#x%s" (match-string 1 hex)))
                         (read (format "#x%s" (match-string 2 hex)))
                         (read (format "#x%s" (match-string 3 hex))))))
    result))
(defun my/convert-color-name-to-hex (color-name)
  (let ((rgb) (hex "#"))
    (mapc (lambda (x)
            (setq rgb (cons (round (* x 255)) rgb)))
          (color-name-to-rgb color-name))
    (setq rgb (nreverse rgb))
    (mapc (lambda (x) (setq hex (concat hex (format "%02x" x))))
          rgb)
    hex))


;;; Keybind
(bind-keys :map global-map
           ("C-a"   . my/goto-line-beginning-or-indent)
           ("C-t"   . my/other-window-or-split)
           ("C-w"   . my/killregion-or-deletewindow)
           ("C-S-f" . forward-word)
           ("C-S-b" . backward-word)
           ("C-S-v" . cua-scroll-down)
           ("C-c l" . toggle-truncate-lines))


;;; Alias
(defalias 'dtw 'delete-trailing-whitespace)
