;; --------------------------------------------------
;; zlc
;; @describe : ミニバッファ補完をzshライクに
;; @refer : http://d.hatena.ne.jp/mooz/20101003/p1
;; --------------------------------------------------
(when (require 'zlc nil t)
  (zlc-mode t)
  (let ((map minibuffer-local-map))
    (define-key map (kbd "C-p") 'zlc-select-previous-vertical)
    (define-key map (kbd "C-n") 'zlc-select-next-vertical)
    (define-key map (kbd "C-b") 'zlc-select-previous)
    (define-key map (kbd "C-f") 'zlc-select-next)
    (define-key map (kbd "C-c") 'zlc-reset)
    )

  ;; TAB補完時、即座に一つ目の補完候補を選択する
  (setq zlc-select-completion-immediately nil)
  )
