;; --------------------------------------------------
;; 言語共通設定
;; --------------------------------------------------
;; TABの表示幅
(setq-default tab-width 4)
;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)


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
;; flychek
;; @refer : http://qiita.com/senda-akiha/items/cddb02cfdbc0c8c7bc2b
;; --------------------------------------------------
(when (require 'flycheck nil t)
  ;; 対応言語の設定を有効
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; エラー内容をツールチップに表示
  (when (require 'flycheck-pos-tip nil t)
    (eval-after-load 'flycheck
      '(custom-set-variables
        '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
  )


;; --------------------------------------------------
;; fold-dwim
;; --------------------------------------------------
(when (require 'fold-dwim nil t)
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
(when (require 'smart-compile nil t)
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
(when (require 'quickrun nil t)
  (when (require 'popwin nil t)
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
(when (require 'rainbow-delimiters nil t)
  (defun my/rainbow-delimiters-hook ()
    (rainbow-delimiters-mode)
    ;; 色を強調
    (when (require 'color nil t)
      (cl-loop
       for index from 1 to rainbow-delimiters-max-face-count
       do
       (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
         (cl-callf color-saturate-name (face-foreground face) 30))))
    )

  ;; (add-to-list 'rainbow-delimiters-ignore-modes 'fundamental-mode) ; helmとの干渉回避
  (add-hook 'prog-mode-hook 'my/rainbow-delimiters-hook)
  )
