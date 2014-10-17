;; --------------------------------------------------
;; tabbar
;; --------------------------------------------------
(when (require 'tabbar nil t)
  (tabbar-mode t)

  ;; マウスホイール操作
  (tabbar-mwheel-mode 0)

  ;; グループ化しない
  (setq tabbar-buffer-groups-function nil)

  ;; 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  ;; タブがはみ出たときの動作
  (setq tabbar-auto-scroll-flag t)

  ;; 外観変更
  (setq tabbar-separator '(1.5))        ; タブの長さ
  (set-face-attribute 'tabbar-default nil
                      :family "Comic Sans MS"
                      ;; :family "Hiragino Mincho Pro"
                      :background "black"
                      :foreground "gray72"
                      :height 1.0)
  (set-face-attribute 'tabbar-selected nil
                      :background "black"
                      :foreground "yellow"
                      :box nil)
  (set-face-attribute 'tabbar-unselected nil
                      :background "black"
                      :foreground "grey72"
                      :box nil)
  (set-face-attribute 'tabbar-button nil
                      :box nil)
  (set-face-attribute 'tabbar-separator nil
                      :height 1.2)

  ;; タブに表示させるバッファの設定
  (defvar my/tabbar-displayed-buffers
    ;; '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-" "*Help*")
    '("*scratch*" "*Colors*" "*Faces*" "*vc-")
    "*Regexps matches buffer names always included tabs.")

  (defun my/tabbar-buffer-list ()
    "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my/tabbar-displayed-buffers'
are always included."
    (let* ((hides (list ?\  ?\*))
           (re (regexp-opt my/tabbar-displayed-buffers))
           (cur-buf (current-buffer))
           (tabs (delq nil
                       (mapcar (lambda (buf)
                                 (let ((name (buffer-name buf)))
                                   (when (or (string-match re name)
                                             (not (memq (aref name 0) hides)))
                                     buf)))
                               (buffer-list)))))
      ;; Always include the current buffer.
      (if (memq cur-buf tabs)
          tabs
        (cons cur-buf tabs))))

  (setq tabbar-buffer-list-function 'my/tabbar-buffer-list)

  ;; タブモードのオン/オフをトグル
  (global-set-key (kbd "M-4") 'tabbar-mode)

  ;; Chromeライクなタブ切り替えのキーバインド
  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

  ;; <Command+number>でタブ指定
  ;; @refer : http://d.hatena.ne.jp/tequilasunset/20101128/p1
  (defun my/tabbar-select-tab-by-number (n)
    "Select Nth tab."
    (interactive "p")
    (let* ((tabset (tabbar-current-tabset t))
           (tab (tabbar-selected-tab tabset))
           previous)
      (when (and tabset (numberp n) (<= 1 n))
        (while (setq previous (tabbar-tab-next tabset tab t))
          (setq tab previous))
        (loop for i from 1 below n
              do (setq tab (tabbar-tab-next tabset tab))
              unless (tabbar-tab-next tabset tab) return nil)
        (tabbar-click-on-tab tab))))

  (loop for i from 1 to 9
        for fn = (intern (format "my-tabbar-select-tab-%d" i))
        do
        (fset fn `(lambda () (interactive) (my/tabbar-select-tab-by-number ,i)))
        (global-set-key (read-kbd-macro (format "s-%d" i)) fn))

  ;; タブ上をマウス中クリックでkill-buffer
  (defun my/tabbar-buffer-help-on-tab (tab)
    "Return the help string shown when mouse is onto TAB."
    (if tabbar--buffer-show-groups
        (let* ((tabset (tabbar-tab-tabset tab))
               (tab (tabbar-selected-tab tabset)))
          (format "mouse-1: switch to buffer %S in group [%s]"
                  (buffer-name (tabbar-tab-value tab)) tabset))
      (format "\
mouse-1: switch to buffer %S\n\
mouse-2: kill this buffer\n\
mouse-3: delete other windows"
              (buffer-name (tabbar-tab-value tab)))))

  (defun my/tabbar-buffer-select-tab (event tab)
    "On mouse EVENT, select TAB."
    (let ((mouse-button (event-basic-type event))
          (buffer (tabbar-tab-value tab)))
      (cond
       ((eq mouse-button 'mouse-2)
        (with-current-buffer buffer
          (kill-buffer)))
       ((eq mouse-button 'mouse-3)
        (delete-other-windows))
       (t
        (switch-to-buffer buffer)))
      ;; Don't show groups.
      (tabbar-buffer-show-groups nil)))

  (setq tabbar-help-on-tab-function 'my/tabbar-buffer-help-on-tab)
  (setq tabbar-select-tab-function 'my/tabbar-buffer-select-tab)
  )


;; --------------------------------------------------
;; ElScreen
;; @refer : http://rubikitch.com/2014/09/05/elscreen/
;; --------------------------------------------------
(when (require 'elscreen nil t)
  ;; (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-set-prefix-key (kbd "C-o"))

  (elscreen-start)

  (setq elscreen-display-tab 10)
  
  ;; タブの先頭に [X] を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;; header-line の先頭に [<->] を表示しない
  (setq elscreen-tab-display-control nil)

  ;; キーバインド
  (global-set-key (kbd "M-t") 'elscreen-clone)
  (global-set-key (kbd "<C-tab>") 'elscreen-next)
  (global-set-key (kbd "<C-S-tab>") 'elscreen-previous)
  
  ;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
  (setq elscreen-buffer-to-nickname-alist
        '(("^dired-mode$" .
           (lambda ()
             (format "Dired(%s)" dired-directory)))
          ("^Info-mode$" .
           (lambda ()
             (format "Info(%s)" (file-name-nondirectory Info-current-file))))
          ("^mew-draft-mode$" .
           (lambda ()
             (format "Mew(%s)" (buffer-name (current-buffer)))))
          ("^mew-" . "Mew")
          ("^irchat-" . "IRChat")
          ("^liece-" . "Liece")
          ("^lookup-" . "Lookup")))
  (setq elscreen-mode-to-nickname-alist
        '(("[Ss]hell" . "shell")
          ("compilation" . "compile")
          ("-telnet" . "telnet")
          ("dict" . "OnlineDict")
          ("*WL:Message*" . "Wanderlust")))
  
  ;; elscreenの順番の入れ替え
  (defun elscreen-swap-previous()
    "Interchange screens selected currently and previous."
    (interactive)
    (cond
     ((elscreen-one-screen-p)
      (elscreen-message "There is only one screen, cannot swap"))
     (t
      (let* ((screen-list (sort (elscreen-get-screen-list) '>))
             (previous-screen
              (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
                  (car screen-list)))
             (current-screen (elscreen-get-current-screen))
             (current-screen-property
              (elscreen-get-screen-property current-screen))
             (previous-screen-property
              (elscreen-get-screen-property previous-screen)))
        (elscreen-set-screen-property current-screen previous-screen-property)
        (elscreen-set-screen-property previous-screen current-screen-property)
        (elscreen-goto-internal (elscreen-get-current-screen)))))
    (elscreen-previous))
  (defun elscreen-swap-next()
    "Interchange screens selected currently and next."
    (interactive)
    (cond
     ((elscreen-one-screen-p)
      (elscreen-message "There is only one screen, cannot swap"))
     (t
      (let* ((screen-list (sort (elscreen-get-screen-list) '<))
             (next-screen
              (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
                  (car screen-list)))
             (current-screen (elscreen-get-current-screen))
             (current-screen-property
              (elscreen-get-screen-property current-screen))
             (next-screen-property
              (elscreen-get-screen-property next-screen)))
        (elscreen-set-screen-property current-screen next-screen-property)
        (elscreen-set-screen-property next-screen current-screen-property)
        (elscreen-goto-internal (elscreen-get-current-screen)))))
    (elscreen-next))
  ;; screenの配置位置ずらし(右)
  (global-set-key (kbd "C-M-<right>") 'elscreen-swap-next)
  ;; screen配置位置ずらし(左)
  (global-set-key (kbd "C-M-<left>") 'elscreen-swap-previous)
  )
