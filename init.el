;; --------------------------------------------------
;; ディレクトリ構成を定義
;; @refer : http://qiita.com/ShingoFukuyama/items/19b02cd1679a6ea0bfdb
;; --------------------------------------------------
;; Emacs23以前では user-emacs-directoryが未定義のため設定を追加
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

(defvar my/config-path "config/")
(defvar my/emacs-config-directory
  (expand-file-name (concat user-emacs-directory my/config-path)))

(defvar my/packages-path "packages/")
(defvar my/emacs-packages-directory
  (expand-file-name (concat user-emacs-directory my/packages-path)))

(defvar my/histroy-path ".history/")
(defvar my/history-directry
  (expand-file-name (concat user-emacs-directory my/histroy-path)))
(defun my/set-history (&rest args)
  (expand-file-name (concat my/history-directry (mapconcat 'identity args ""))))


;; --------------------------------------------------
;; load-pathの追加
;; --------------------------------------------------
;; load-pathを追加する関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリ以下をload-pathに追加
(add-to-load-path my/config-path my/packages-path)


;; --------------------------------------------------
;; ELPA
;; --------------------------------------------------
(when (require 'package nil t)
  ;; ディレクトリの指定
  (setq package-user-dir (concat my/emacs-packages-directory "elpa-packages/"))
  
  ;; パッケージリポジトリの追加
  (add-to-list 'package-archives
               '("marmalage" . "http://marmalage-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

  ;; インストールしたパッケージを読み込む
  (package-initialize)
  
  ;; 手元にないパッケージをインストール
  (when (require 'install-list nil t)
    (dolist (package my/elpa-packages)
      (when (or (not (package-installed-p package)))
        (package-refresh-contents)
        (package-install package))))
)


;; --------------------------------------------------
;; el-get
;; @refer : http://blog.ogatomo.com/blog/2014/01/08/migration-to-el-get/
;; --------------------------------------------------
;; インストール先を指定
(defvar el-get-dir (concat my/emacs-packages-directory "elget-packages/"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; ユーザ定義のレシピ置き場を指定
(defvar my/el-get-recipes-path "recipes/")
(add-to-list 'el-get-recipe-path (concat my/emacs-packages-directory my/el-get-recipes-path))

;; 手元にないパッケージをインストール
(when (require 'install-list nil t)
  (el-get 'sync my/el-get-packages))


;; --------------------------------------------------
;; use-package
;; @refer : http://qiita.com/kai2nenobu/items/5dfae3767514584f5220
;; --------------------------------------------------
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))


;; --------------------------------------------------
;; 分割設定 (init-loader)
;; --------------------------------------------------
(when (require 'init-loader nil t)
  (init-loader-load my/emacs-config-directory)
)
