;;; --- Install List ---
;;; El-get
(defvar my/el-get-package-list
  '(auto-complete
    auto-highlight-symbol
    eldoc-extension
    exec-path-from-shell
    expand-region
    flycheck
    flycheck-color-mode-line
    flycheck-pos-tip
    fold-dwim
    git-gutter
    git-gutter-fringe
    git-modes
    helm
    helm-descbinds
    highlight-symbol
    hlinum
    htmlize
    inf-ruby
    init-loader
    magit
    markdown-mode
    multiple-cursors
    open-junk-file
    org-mode
    point-undo
    popwin
    quickrun
    rainbow-delimiters
    rainbow-mode
    rbenv
    region-bindings-mode
    robe-mode
    ruby-block
    ruby-end
    ruby-mode
    smart-compile
    smart-newline
    smartrep
    smooth-scroll
    undo-tree
    undohist
    use-package
    volatile-highlights
    web-mode
    yaml-mode
    zlc)
  "A list of packages to install from el-get at launch.")


;;; ELPA
(defvar my/elpa-package-list
  '()
  "A list of packages to install from package repository.")


(provide 'install-list)
