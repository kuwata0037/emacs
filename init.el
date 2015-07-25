;;; --- Initialize ---
;;; Directory
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))
(defvar my/config-directory
  (expand-file-name (concat user-emacs-directory "config/")))
(defvar my/package-directory
  (expand-file-name (concat user-emacs-directory "package/")))


;;; Load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name path)))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path my/config-directory my/package-directory)


;;; Package
;; ELPA
(when (require 'package nil t)
  (setq package-user-dir (concat my/package-directory "elpa/"))
  (add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalage" . "http://marmalage-repo.org/packages/"))
  (when (require 'install-list nil t)
    (dolist (package my/elpa-package-list)
      (when (or (not (package-installed-p package)))
        (package-refresh-contents)
        (package-install package))))
  (package-initialize))

;; El-get
(defvar el-get-dir (concat my/package-directory "el-get/"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (concat my/package-directory "recipes/"))
(when (require 'install-list nil t)
  (el-get 'sync my/el-get-package-list))


;;; Use-package
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))


;;; Init-loader
(use-package init-loader
  :config (init-loader-load my/config-directory))
