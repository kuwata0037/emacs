;; --------------------------------------------------
;; インストールしたパッケージリスト
;; @refer : http://shibayu36.hatenablog.com/entry/2013/04/30/175740
;; --------------------------------------------------
(defvar my/el-get-packages
  '(
    ;; inits
    exec-path-from-shell init-loader use-package

    ;; helm
    helm helm-descbinds

    ;; undo
    point-undo undo-tree undohist

    ;; color theme
    ujelly-theme

    ;; vc
    magit

    ;; ruby
    ruby-end ruby-block inf-ruby robe-mode

    ;; yaml
    yaml-mode

    ;; web
    web-mode

    ;; highlight
    auto-highlight-symbol highlight-symbol

    ;; other
    auto-complete expand-region fold-dwim htmlize popwin
    rainbow-delimiters quickrun smart-compile smart-newline
    smooth-scroll tabbar zlc

    )
  "A list of packages to install from el-get at launch.")


(defvar my/elpa-packages
  '(
    ;; ;; other
    ;; helm

    )
  "A list of packages to install from package repository.")


(provide 'install-list)
