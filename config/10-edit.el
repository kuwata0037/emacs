;;; --- Edit ---
;;; Basic
(setq-default line-spacing 0)
(setq kill-whole-line t)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;; Tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)


;;; Newline
(use-package smart-newline
  :bind   ("C-m" . smart-newline)
  :config (smart-newline-mode))


;;; Region
(cua-mode)
(setq cua-enable-cua-keys nil)
(use-package expand-region
  :bind (("C-;" . er/expand-region)
         ("C-'" . er/contract-region)))


;;; Search & Replace [TODO]


;;; Folding
(use-package fold-dwim
  :init
  (dolist (hs-minor-mode-hooks
           '(emacs-lisp-mode-hook
             c-mode-common-hook
             python-mode-hook
             php-mode-hook
             ruby-mode-hook
             js2-mode-hook
             css-mode-hook
             cperl-mode))
    (add-hook hs-minor-mode-hooks 'hs-minor-mode))
  :config
  ;; ruby
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
  (bind-keys :map hs-minor-mode-map
             ("C-,"     . fold-dwim-toggle)
             ("C-c C-," . fold-dwim-hide-all)
             ("C-c C-." . fold-dwim-show-all)))


;;; Minibuffer
(fset 'yes-or-no-p 'y-or-n-p)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case    t)
(use-package helm-config
  :bind (("C-c h"   . helm-mini)
         ("C-x C-r" . helm-recentf)
         ("C-x C-b" . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring))
  :config
  (helm-mode)
  (use-package helm-descbinds :config (helm-descbinds-mode))
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  (define-key helm-map            (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char))
(use-package zlc
  :config
  (zlc-mode)
  (setq zlc-select-completion-immediately nil)
  (bind-keys :map minibuffer-local-map
             ("C-c" . zlc-reset)
             ("C-b" . zlc-select-previous)
             ("C-f" . zlc-select-next)
             ("C-p" . zlc-select-previous-vertical)
             ("C-n" . zlc-select-next-vertical)))
(use-package open-junk-file
  :bind   ("C-x j" . open-junk-file)
  :config (setq open-junk-file-format "/var/tmp/junk/%Y-%m%d-%H%M%S."))


;;; Complement
;; auto complete
(use-package auto-complete-config
  :config
  (ac-config-default)
  (setq ac-comphist-file (my/set-history "ac-comphist.dat"))
  (setq ac-use-menu-map t)
  (setq ac-delay 0.0)
  (setq ac-auto-show-menu 0.3))

;; auto insert
(use-package autoinsert
  :config
  (auto-insert-mode)
  (setq auto-insert-query nil))

;; parentheses
(electric-pair-mode)
(electric-indent-mode)
(electric-layout-mode)
(add-to-list 'electric-pair-pairs   '(?{ . ?}))
(add-to-list 'electric-layout-rules '(?{ . after))


;;; Syntax Check
(use-package flycheck
  :config
  (global-flycheck-mode)
  (defun my/flycheck-c++-language-standrd ()
    (setq flycheck-gcc-language-standard   "c++1y")
    (setq flycheck-clang-language-standard "c++1y"))
  (add-hook 'c++-mode-hook 'my/flycheck-c++-language-standrd)
  (use-package flycheck-pos-tip
    :config
    (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))
  (use-package flycheck-color-mode-line
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))


;;; Execution
;; quickrun
(use-package quickrun
  :bind ("C-\\" . my/quickrun-sc)
  :config
  (defun my/quickrun-sc ()
    (interactive)
    (if (use-region-p)
        (quickrun :strat (region-beginning) :end (region-end))
      (quickrun)))
  (quickrun-add-command "c++/g++"
                        '((:exec         . ("%c -std=c++1y -x c++ %o -o %e %s"
                                            "%e %a"))
                          (:compile-only .  "%c -Wall -Werror -std=c++1y %o -o %e %s"))
                        :override t)
  (quickrun-add-command "c++/clang++"
                        '((:exec         . ("%c -std=c++1y -x c++ %o -o %e %s"
                                            "%e %a"))
                          (:compile-only .  "%c -Wall -Werror -std=c++1y %o -o %e %s"))
                        :override t)
  (push '("*quickrun*") popwin:special-display-config))

;; smart compile [TODO]
(use-package smart-compile
  :disabled
  :config
  (setq compilation-window-height 15)
  (define-key ruby-mode-map (kbd "C-c c")   'smart-compile)
  (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m")))


;;; Version Control System
(setq vc-follow-symlinks t)

;; git
(use-package magit
  :if   (executable-find "git")
  :bind ("C-c C-m" . magit-status))
(defun my/git-gutter-initialize ()
  (global-git-gutter-mode)
  (bind-keys :map global-map
             ("C-x v p" . git-gutter:popup-hunk)
             ("C-x v s" . git-gutter:stage-hunk)
             ("C-x v r" . git-gutter:revert-hunk))
  (smartrep-define-key
      global-map "C-c" '(("p" . git-gutter:previous-hunk)
                         ("n" . git-gutter:next-hunk))))
(use-package git-gutter
  :if (not window-system)
  :config
  (my/git-gutter-initialize)
  (git-gutter:linum-setup))
(use-package git-gutter-fringe
  :if (window-system)
  :config
  (my/git-gutter-initialize)
  (setq git-gutter-fr:side 'right-fringe))


;;; Printout
(use-package htmlize
  :commands my/htmlize-and-browse
  :config
  (defun my/htmlize-and-browse ()
    (interactive)
    (defcustom
      htmlize-and-browse-directory-path temporary-file-directory
      "htmlize-and-browse-temporary-file-directory"
      :type  'string
      :group 'htmlize-and-browse)
    (setq htmlize-and-browse-buffer-file-name
          (concat "htmlize-and-browse-"
                  (format-time-string "%Y%m%d%H%M%S" (current-time)) ".html"))
    (setq htmlize-and-browse-buffer-file-path
          (concat htmlize-and-browse-directory-path htmlize-and-browse-buffer-file-name))
    (with-current-buffer (htmlize-buffer)
      (write-file htmlize-and-browse-buffer-file-path)
      (set-buffer-modified-p nil)
      (kill-buffer htmlize-and-browse-buffer-file-name)
      (shell-command (concat "open " htmlize-and-browse-buffer-file-path)))))
