;; --------------------------------------------------
;; web -- html, css, script
;; --------------------------------------------------
;; web-mode
;; @refer : http://yanmoo.blogspot.jp/2013/06/html5web-mode.html
(use-package web-mode
  :mode (("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.html?$"     . web-mode))
  :config
  ;; タブ設定
  ;; @refer : http://suzuki.tdiary.net/20140404.html
  (setq tab-width 2)
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-css-indent-offset tab-width)
  (setq web-mode-code-indent-offset tab-width)
  (setq web-mode-style-padding tab-width)
  (setq web-mode-script-padding tab-width)
  (setq web-mode-block-padding tab-width)
  ;; 色の設定
  (custom-set-faces
   '(web-mode-doctype-face
     ((t (:foreground "#82AE46"))))     ; doctype
   '(web-mode-html-tag-face
     ((t (:foreground "#E6B422" :weight bold)))) ; 要素名
   '(web-mode-html-attr-name-face
     ((t (:foreground "#C97586"))))     ; 属性名など
   '(web-mode-html-attr-value-face
     ((t (:foreground "#82AE46"))))     ; 属性値
   '(web-mode-comment-face
     ((t (:foreground "#D9333F"))))     ; コメント
   '(web-mode-server-comment-face
     ((t (:foreground "#D9333F"))))     ; コメント
   '(web-mode-css-rule-face
     ((t (:foreground "#A0D8EF"))))     ; cssのタグ
   '(web-mode-css-pseudo-class-face
     ((t (:foreground "#FF7F00"))))     ; css 疑似クラス
   '(web-mode-css-at-rule-face
     ((t (:foreground "#FF7F00"))))     ; cssのタグ
   )
)
