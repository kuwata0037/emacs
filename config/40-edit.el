;; --------------------------------------------------
;; 言語共通設定
;; --------------------------------------------------
;; TABの表示幅
(setq-default tab-width 4)
;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)
;; ファイルが #! から始まる場合, +x を付けて保存する
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;--------------------------------------------------
;; electric-pair-mode
;; @describe : 括弧の補完
;; @refer : http://block-given.hatenablog.com/entry/2014/11/12/005657
;;--------------------------------------------------
;; electric-pair-mode を有効
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)
;; 対応括弧の追加
;; (add-to-list 'electric-pair-pairs '(?| . ?|))
(add-to-list 'electric-pair-pairs '(?{ . ?}))

(add-to-list 'electric-layout-rules '(?{ . after))


;; --------------------------------------------------
;; auto-insert-mode
;; @refer : http://d.hatena.ne.jp/yutoichinohe/20130607/1370627890
;; --------------------------------------------------
(use-package autoinsert
  :config
  (auto-insert-mode t)
  (setq auto-insert-query nil)
  )


;; --------------------------------------------------
;; flychek
;; @refer : http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
;; --------------------------------------------------
(use-package flycheck
  :config
  ;; 対応言語の設定を有効
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; エラー内容をツールチップに表示
  (use-package flycheck-pos-tip
    :config
    (eval-after-load 'flycheck
      '(custom-set-variables
        '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
  )


;; --------------------------------------------------
;; fold-dwim
;; --------------------------------------------------
(use-package fold-dwim
  :config
  ;; フォールディングするモードを設定
  (let ((hs-minor-mode-hooks))
    (dolist (hs-minor-mode-hooks
             '(emacs-lisp-mode-hook
               c-mode-common-hook
               python-mode-hook
               php-mode-hook
               ruby-mode-hook
               js2-mode-hook
               css-mode-hook
               cperl-mode))
      (add-hook hs-minor-mode-hooks 'hs-minor-mode)))

  ;; キーバインド
  (define-key hs-minor-mode-map (kbd "C-,")     'fold-dwim-toggle)
  (define-key hs-minor-mode-map (kbd "C-c C-;") 'fold-dwim-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c C-'") 'fold-dwim-show-all)


  ;; ruby
  ;; @refer : http://www.bookshelf.jp/pukiwiki/pukiwiki.php?cmd=read&page=Elisp%2Fhideshow.el
  (let ((ruby-mode-hs-info
         '(ruby-mode
           "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do"
           "end"
           "#"
           ruby-move-to-block
           nil)))
    (if (not (member ruby-mode-hs-info hs-special-modes-alist))
        (setq hs-special-modes-alist
              (cons ruby-mode-hs-info hs-special-modes-alist))))
  ;; ruby-modeで確実にhs-minor-modeを起動させる設定
  ;; (add-hook 'find-file-hook
  ;;           '(lambda ()
  ;;              (when (eq major-mode 'ruby-mode)
  ;;                (hs-minor-mode))))
  )


;; --------------------------------------------------
;; smart-complie
;; @refer : http://america66.hatenablog.com/entry/2014/03/02/022907
;; --------------------------------------------------
(use-package smart-compile
  :config
  (setq smart-compile-alist
        (append
         '(("\\.rb$" . "ruby %f"))
         smart-compile-alist))
  (setq compilation-window-height 15)

  (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
  (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))
  )


;; --------------------------------------------------
;; quickrun
;; @refer : http://rubikitch.com/tag/emacs-quickrun-%E8%A8%AD%E5%AE%9A/
;; @refer : http://yunojy.github.io/blog/2013/03/17/emacs-de-quickrun-or-quickrun-region/
;; --------------------------------------------------
(use-package quickrun
  :config
  (use-package popwin
    :disabled
    :config
    (popwin-mode)
    (push '("*quickrun*") popwin:special-display-config)
    )
  
  (defun my/quickrun-sc ()
    (interactive)
    (if (use-region-p)
        (quickrun :strat (region-beginning) :end (region-end))
      (quickrun)))

  (global-set-key (kbd "C-\\") 'my/quickrun-sc)
  )


;; --------------------------------------------------
;; rainbow-delimiters
;; @describe : 対応する括弧ごとに色を変える
;; @refer : http://qiita.com/ncaq/items/5a1d102723fec11a8bff
;; --------------------------------------------------
(use-package rainbow-delimiters
  :config
  (defun my/rainbow-delimiters-hook ()
    (rainbow-delimiters-mode)
    ;; 色を強調
    (use-package color
      :config
      (cl-loop
       for index from 1 to rainbow-delimiters-max-face-count
       do
       (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
         (cl-callf color-saturate-name (face-foreground face) 30))))
    )

  ;; (add-to-list 'rainbow-delimiters-ignore-modes 'fundamental-mode) ; helmとの干渉回避
  (add-hook 'prog-mode-hook 'my/rainbow-delimiters-hook)
  )


;; --------------------------------------------------
;; htmlize
;; @describe : ソースコードを html ファイルに
;; @refer    : http://www.kanasansoft.com/weblab/2011/07/htmlize-and-browse.html
;; --------------------------------------------------
(use-package  htmlize
  :config
  (defun htmlize-and-browse ()
    (interactive)
    (defcustom
      htmlize-and-browse-directory-path temporary-file-directory
      "htmlize-and-browse-temporary-file-directory"
      :type 'string
      :group 'htmlize-and-browse)
    (setq htmlize-and-browse-buffer-file-name (concat "htmlize-and-browse-" (format-time-string "%Y%m%d%H%M%S" (current-time)) ".html"))
    (setq htmlize-and-browse-buffer-file-path (concat htmlize-and-browse-directory-path htmlize-and-browse-buffer-file-name))
    (with-current-buffer (htmlize-buffer)
      (write-file htmlize-and-browse-buffer-file-path)
      (set-buffer-modified-p nil)
      (kill-buffer htmlize-and-browse-buffer-file-name)
      (shell-command (concat "open " htmlize-and-browse-buffer-file-path))
      )
    )
  )
