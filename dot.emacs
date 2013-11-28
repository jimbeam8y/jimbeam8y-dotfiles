
;;;;;;;; -*- emacs-lisp -*-

;;; Code:
;;;; ガベッジコレクションを実行するまでの割り当てメモリの閾値を増やす
(setq gc-cons-threshold (* 100 gc-cons-threshold))
;;ログの記録量を増やす
(setq message-log-max 10000)
;;履歴存数を増やす
(setq history-length 1000)
;;重複する履歴は保存しない
(setq history-delete-duplicates t)

;;; load-path
(add-to-list 'load-path "~/.emacs.d/")

;; global settings
(show-paren-mode t)
(global-linum-mode t)
(which-function-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-hl-line-mode t)
(global-hi-lock-mode 1)
(setq vc-follow-symlinks t)

;; マーク領域を色付け
(setq transient-mark-mode t)
;; マウスで選択するとコピーする Emacs 24 ではデフォルトが nil
(setq mouse-drag-copy-region t)

(global-set-key (kbd "C-m") 'newline-and-indent)

;;; backup file
(setq make-backup-files t)
(setq backup-directory-alist
  (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
    backup-directory-alist))

;;;;
;;;; mozc
;;;; via http://d.hatena.ne.jp/syohex/20120126/1327597912
(require 'mozc)
(setq mozc-leim-title "[I&#9825;Mozc]") ; modeline変更

(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(global-set-key (kbd "C-o") 'toggle-input-method)

;;;(setq mozc-candidate-style 'echo-area)
(setq mozc-candidate-style 'overlay)

(prefer-coding-system 'utf-8-unix)      ; 日本語入力のための設定

;;;;
;;;; タイトルバーに日時を表示する
(setq display-time-string-forms
      '((format "%s/%s(%s)%s:%s" month day dayname 24-hours minutes)))
(display-time)
(cond (window-system
       (setq frame-title-format
             '((multiple-frames "")
               display-time-string))
       (remove-hook 'global-mode-string 'display-time-string)))

;;;; Coolな設定
;;;; via http://sakito.jp/emacs/emacs23.html#id17
;; 垂直スクロール用のスクロールバーを付けない
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
;; 背景の透過
;; (add-to-list 'default-frame-alist '(alpha . (85 20)))
(add-to-list 'default-frame-alist '(alpha . (92 70)))

;;; フォントの設定
(when (find-font (font-spec :family "UmePlus Gothic mod"))
  ;; http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html
  (set-face-attribute 'default
                      nil
                      :family "UmePlus Gothic mod"
                      :height 120)
  (add-to-list 'default-frame-alist '(font . "UmePlus Gothic mod"))
  (set-fontset-font nil
                    'unicode
                    (font-spec :family "UmePlus Gothic mod")
                    nil
                    'append)
  ;; (set-frame-font "Ricty-16:weight=normal:slant=normal")
  ;; (set-frame-font "Aicty-14:weight=normal:slant=normal")
  )

;; フォントロックの設定
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  ;;(setq font-lock-maximum-decoration t)
  (setq font-lock-support-mode 'jit-lock-mode))

;; タブ文字、全角空白、文末の空白の色付け
;; @see http://www.emacswiki.org/emacs/WhiteSpace
;; @see http://xahlee.org/emacs/whitespace-mode.html
(setq whitespace-style '(spaces tabs space-mark tab-mark))
(setq whitespace-display-mappings
      '(
       ;; (space-mark 32 [183] [46]) ; normal space, ·
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
;        (space-mark ?\x3000 [?\□]) ;; 全角スペース
        (space-mark ?\x3000 [9655] [46]) ;; 全角スペース
        ;; (newline-mark 10 [182 10]) ; newlne, ¶
;        (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
        (tab-mark 9 [8614 9] [92 9]) ; tab, ↦
        ))

(require 'whitespace)
(global-whitespace-mode 0) ;; 常に whitespace-mode だと動作が遅くなる場合がある

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)
;; EOB を表示
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;;;;;;;;
;;;;;;;;
;;;;;;;;

(require 'wdired)
(setq default-file-name-coding-system 'utf-8-unix) ;diredで日本語file名出力

;;;;;; package manager
(require 'package)
(add-to-list 'package-archives '("mepla" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
(package-initialize)


;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil t)
;;   (url-retrieve
;;    "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
;;    (lambda (s)
;;      (end-of-buffer)
;;      (eval-print-last-sexp))))

;; ;; now either el-get is `require'd already, or have been `load'ed by the
;; ;; el-get installer.
;; (setq
;;  el-get-sources
;;  '(el-get                               ; el-get is self-hosting
;;    escreen                              ; screen for emacs, C-\ C-h
;;    php-mode-improved                    ; if you're into php...
;;    switch-window                        ; takes over C-x o
;;    auto-complete                        ; complete as you type with overlays
;;    zencoding-mode                       ; http://www.emacswiki.org/emacs/ZenCoding

;;    (:name buffer-move                   ; have to add your own keys
;;           :after (lambda ()
;;                    (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;;                    (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;;                    (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;;                    (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

;;    (:name smex                          ; a better (ido like) M-x
;;           :after (lambda ()
;;                    (setq smex-save-file "~/.emacs.d/.smex-items")
;;                    (global-set-key (kbd "M-x") 'smex)
;;                    (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

;;    (:name magit                                 ; git meet emacs, and a binding
;;           :after (lambda ()
;;                    (global-set-key (kbd "C-x C-z") 'magit-status)))

;;    (:name goto-last-change              ; move pointer back to last change
;;           :after (lambda ()
;;                    ;; when using AZERTY keyboard, consider C-x C-_
;;                    (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;; (unless (string-match "apple-darwin" system-configuration)
;;   (loop for p in '(color-theme          ; nice looking emacs
;;                    color-theme-tango    ; check out color-theme-solarized
;;                    )
;;         do (add-to-list 'el-get-sources p)))

;; ;;
;; ;; Some recipes require extra tools to be installed
;; ;;
;; ;; Note: el-get-install requires git, so we know we have at least that.
;; ;;
;; (when (el-get-executable-find "cvs")
;;   (add-to-list 'el-get-sources 'emacs-goodies-el)) ; the debian addons for emacs

;; (when (el-get-executable-find "svn")
;;   (loop for p in '(psvn                 ; M-x svn-status
;;                    yasnippet            ; powerful snippet mode
;;                    )
;;         do (add-to-list 'el-get-sources p)))

;; ;; install new packages and init already installed packages
;; (el-get 'sync)

;;;;;; packages
;; ;;; color-theme
;; ;;; via http://d.hatena.ne.jp/fatrow/20101025/emacs_color_theme
;; (require 'color-theme)
;; (require 'zenburn)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;; ;     (color-theme-ld-dark)
;;      (color-theme-zenburn)
;;      ))

;;; Aspell
(setq ispell-program-name "aspell")
(setq ispell-grep-command "grep")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]")))
;(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "aspell")

;;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;;; twitter
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-display-remaining t)
(setq twittering-initial-timeline-spec-string
      '("jimbeam8y/friends"))

(defun my-twit ()
  (interactive)
  (elscreen-create)
  (twit)
  (cond
   ((twittering-account-authorized-p)
    (switch-to-buffer "jimbeam8y/friends")
    (other-window 1)
    )
   (t
    (delete-other-windows))))

(add-hook 'twittering-new-tweets-hook (lambda ()
   (let ((n twittering-new-tweets-count))
     (start-process "twittering-notify" nil "notify-send"
                    "-i" "/usr/share/pixmaps/gnome-emacs.png"
                    "New tweets"
                    (format "You have %d new tweet%s"
                            n (if (> n 1) "s" ""))))))
;;; recentf
(require 'recentf)
(recentf-mode 1)

;;; completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'icomplete)
(icomplete-mode 99)
(eval-after-load "icomplete" '(progn (require 'icomplete+)))

;;; window select
(require 'popup)
(require 'popup-select-window)
(global-set-key "\C-xo" 'popup-select-window)
;(setq popup-select-window-popup-windiws 2)
(setq popup-select-window-window-highlight-face
      '(:foreground "white" :background "orange"))

;; ;;; like a 'screen'
;; ;;; via http://www.morishima.net/~naoto/elscreen-ja/
;; ;;; code from GitHub.
;; (add-to-list 'load-path "~/.emacs.d/elscreen") ;github repo.
;; (require 'elscreen)
;; (elscreen-start)
;; (load "elscreen-gf" "ElScreen-GF" t) ;; TODO GNU GLOBAL で使ってみたい。
;; (require 'elscreen-color-theme)
;; (require 'elscreen-dired)
;; (require 'elscreen-w3m)
;; (require 'elscreen-server)
;; (require 'elscreen-wl)

;; (defmacro elscreen-create-automatically (ad-do-it)
;;   `(if (not (elscreen-one-screen-p))
;;        ,ad-do-it
;;      (elscreen-create)
;;      (elscreen-notify-screen-modification 'force-immediately)
;;      (elscreen-message "New screen is automatically created")))

;; (defadvice elscreen-next (around elscreen-create-automatically activate)
;;   (elscreen-create-automatically ad-do-it))

;; (defadvice elscreen-previous (around elscreen-create-automatically activate)
;;   (elscreen-create-automatically ad-do-it))

;; (defadvice elscreen-toggle (around elscreen-create-automatically activate)
;;   (elscreen-create-automatically ad-do-it))
;; ;;; key bind for elscreen
;; (global-unset-key "\C-q")
;; (defvar ctl-q-map (make-keymap))
;; (define-key global-map (kbd "C-q") ctl-q-map)
;; (define-key ctl-q-map (kbd "c") 'elscreen-create)
;; (define-key ctl-q-map (kbd "k") 'elscreen-kill)
;; (define-key ctl-q-map (kbd "d") 'elscreen-dired)
;; (define-key ctl-q-map (kbd "n") 'elscreen-next)
;; (define-key ctl-q-map (kbd "C-n") 'elscreen-next)
;; (define-key ctl-q-map (kbd "p") 'elscreen-previous)
;; (define-key ctl-q-map (kbd "s") 'my-google-search)
;; (define-key ctl-q-map (kbd "C-p") 'elscreen-previous)
;; (define-key ctl-q-map (kbd "0") 'elscreen-jump-0)
;; (define-key ctl-q-map (kbd "1") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "2") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "3") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "4") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "5") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "6") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "7") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "8") 'elscreen-jump)
;; (define-key ctl-q-map (kbd "9") 'elscreen-jump-9)
;; (define-key ctl-q-map (kbd "?") 'elscreen-help)
;; (define-key ctl-q-map (kbd "t") 'my-twit)
;; (define-key ctl-q-map (kbd "w") 'elscreen-w3m)
;; ;;;;;;;; ：
;; ;;(define-key ctl-q-map (kbd "C-a") 'your-favorite-funca)
;; ;;(define-key ctl-q-map (kbd "C-b") 'your-favorite-funcb)
;; (define-key ctl-q-map (kbd "C-q") 'quoted-insert)
;; ;;(define-key ctl-q-map (kbd "C-z") 'your-favorite-funcz)

;;; w3m
(add-to-list 'load-path "~/.emacs.d/emacs-w3m") ;; Development Version
(require 'w3m-load)
(setq w3m-use-cookies t)                ;Enable Cookies
;;Follow links in W3M
(setq browse-url-browser-function 'w3m-browse-url)
(setq browse-url-new-window-flag t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point)
(autoload 'browse-url-interactive-arg "browse-url")
;;Set default download directory
;; (let ((d "~/downloads/"))
;;   (setq w3m-default-save-directory(or(and(file-directory-p d) d)
;;                                   w3m-default-directory)
;;      )
;;   )

;;W3M doesn't name buffers very intelligently. Let's fix that:
(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*"
                     (or w3m-current-titlew3m-current-url))
             t)))

(defun my-google-search ()
  (interactive)
  (elscreen-create)
  (elscreen-w3m-initialize)
  )

;; Chrome text area edit
(require 'edit-server)
(edit-server-start)
(setq edit-server-new-frame t)

;; Wanderlust
;calistがないとか言うようになった
;(autoload 'wl "wl" "Wanderlust" t)
;(require 'mime-w3m)

;;; Magit
(add-to-list 'load-path "~/.emacs.d/magit") ;github repo.
(require 'magit)


;;;;; mode hook
;; Text
(add-hook 'text-mode-hook
          (lambda ()
            (whitespace-mode)
            (setq tab-width 4)
            ))

;; Emacs-Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (whitespace-mode)
            ))
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))
(add-hook 'c-mode-common-hook 'flycheck-mode)

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (message "hook")
            (setq tab-width 2)
            (setq indent-tabs-mode t)
            (setq c-basic-offset 2)))

;; nXML
(add-hook 'nxml-mode-hook
          (lambda ()
            (message "hook")
            (setq tab-width 4)
            ))

;; Python
(add-hook 'python-mode-hook 'flycheck-mode)

;; Ruby
(add-hook 'ruby-mode-hook 'flycheck-mode)

;; CSS
(add-hook 'css-mode-hook 'flycheck-mode)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)
(add-hook 'web-mode-hook 'flycheck-mode)


;;;;
;;;; tips for Ubuntu
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
)

(setq migemo-command "cmigemo")
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
;(setq search-whitespace-regexp nil)

;;;;
;;;; org-mode
(add-to-list 'load-path "~/.emacs.d/org-mode") ;github repo.
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp") ;github repo.
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-src-fontify-natively t)

;;;;
;;;; GNU GLOBAL
(add-to-list 'load-path "~/.emacs.d/gtags") ;github repo.
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))

;;;;
;;;; JDEE
(add-to-list 'load-path "~/.emacs.d/jdee/lisp") ;github repo.
(load "jde-autoload")

(defun my-jde-mode-hook ()
  (require 'jde)

  (setq jde-build-function 'jde-ant-build) ; ビルドにantを利用する
  (setq jde-ant-read-target t)             ; targetを問い合わせる
  (setq jde-ant-enable-find t)             ; antに-findオプションを指定する(要らないかも)

  ;; complilationバッファを自動的にスクロールさせる
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'first-error)

  (define-key jde-mode-map (kbd "C-c C-v .") 'jde-complete-minibuf)
  )

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;;;;
;;;; JSON
;;;; via Emacs で JSON を Flymake する http://yak-shaver.blogspot.jp/2013/06/emacs-json-flymake.html
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(require 'flymake-json)
(add-hook 'json-mode-hook 'flymake-json-load)

;; ;;;;
;; ;;;; PowerLine
;; (require 'powerline)
;; (defun arrow-right-xpm (color1 color2)
;;   "Return an XPM right arrow string representing."
;;   (format "/* XPM */
;; static char * arrow_right[] = {
;; \"12 18 2 1\",
;; \". c %s\",
;; \"  c %s\",
;; \".           \",
;; \"..          \",
;; \"...         \",
;; \"....        \",
;; \".....       \",
;; \"......      \",
;; \".......     \",
;; \"........    \",
;; \".........   \",
;; \".........   \",
;; \"........    \",
;; \".......     \",
;; \"......      \",
;; \".....       \",
;; \"....        \",
;; \"...         \",
;; \"..          \",
;; \".           \"};"  color1 color2))

;; (defun arrow-left-xpm (color1 color2)
;;   "Return an XPM right arrow string representing."
;;   (format "/* XPM */
;; static char * arrow_right[] = {
;; \"12 18 2 1\",
;; \". c %s\",
;; \"  c %s\",
;; \"           .\",
;; \"          ..\",
;; \"         ...\",
;; \"        ....\",
;; \"       .....\",
;; \"      ......\",
;; \"     .......\",
;; \"    ........\",
;; \"   .........\",
;; \"   .........\",
;; \"    ........\",
;; \"     .......\",
;; \"      ......\",
;; \"       .....\",
;; \"        ....\",
;; \"         ...\",
;; \"          ..\",
;; \"           .\"};"  color2 color1))


;; (defconst color1 "NavyBlue")
;; (defconst color2 "RoyalBlue")
;; (defconst color3 "#4682b4")
;; (defconst color4 "#CDC0B0")
;; (defconst color5 "#0000cd")
;; (defconst color6 "#8b008b")

;; (defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
;; (defvar arrow-right-2 (create-image (arrow-right-xpm color2 color5) 'xpm t :ascent 'center))
;; (defvar arrow-right-3 (create-image (arrow-right-xpm color5 color6) 'xpm t :ascent 'center))
;; (defvar arrow-right-4 (create-image (arrow-right-xpm color6 "None") 'xpm t :ascent 'center))
;; (defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
;; (defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

;; (setq-default mode-line-format
;;  (list
;;   '(:eval (concat (propertize " %Z " 'face 'mode-line-color-1)
;; 		  (propertize "  " 'display arrow-right-1)))
;;   '(:eval (concat (propertize " %* %b " 'face 'mode-line-color-2)
;; 		  (propertize " " 'display arrow-right-2)))
;;   '(:eval (concat (propertize " %m " 'face 'mode-line-color-3)
;; 		  (propertize " " 'display arrow-right-3)))
;;   '(:eval (concat (propertize " " vc-mode " " 'face 'mode-line-color-4)
;; 		  (propertize " " 'display arrow-right-4)))

;;   mode-line-process minor-mode-alist mode-line-misc-info

;;   ;; Justify right by filling with spaces to right fringe - 16
;;   ;; (16 should be computed rahter than hardcoded)
;;   '(:eval (propertize " " 'display '((space :align-to (- right-fringe 16)))))

;;   '(:eval (concat (propertize " " 'display arrow-left-2)
;; 		  (propertize " %p " 'face 'mode-line-color-2)))
;;   '(:eval (concat (propertize " " 'display arrow-left-1)
;; 		  (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
;;   )
;;  )

;; (make-face 'mode-line-color-1)
;; (set-face-attribute 'mode-line-color-1 nil
;;                     :foreground "#fff"
;;                     :background color1)

;; (make-face 'mode-line-color-2)
;; (set-face-attribute 'mode-line-color-2 nil
;;                     :foreground "#fff"
;;                     :background color2)

;; (make-face 'mode-line-color-3)
;; (set-face-attribute 'mode-line-color-3 nil
;;                     :foreground "#fff"
;;                     :background color5)

;; (make-face 'mode-line-color-4)
;; (set-face-attribute 'mode-line-color-4 nil
;;                     :foreground "#fff"
;;                     :background color6)

;; (set-face-attribute 'mode-line nil
;;                     :foreground "black")

;;;;
;;;; tips: Emacs24 で動作が重くなったのを改善
;;;; via http://www.kaichan.mydns.jp/~kai/orgweb/init.html
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction (quote left-to-right))

;; faces
(set-face-attribute 'mozc-cand-overlay-even-face 'nil
                    :background "midnight blue" :foreground "white")
(set-face-attribute 'mozc-cand-overlay-odd-face 'nil
                    :background "midnight blue" :foreground "white")

;;;;; e2em
;;;;;
(add-to-list 'load-path "~/.emacs.d/emacs-window-layout") ;github repo.
(add-to-list 'load-path "~/.emacs.d/emacs-window-manager") ;github repo.
(require 'e2wm-my-config)

(global-set-key (kbd "M-+") 'e2wm:start-management)

;; with edbi
(require 'edbi)
(autoload 'edbi:open-db-viewer "edbi")

(load "e2wm-edbi-pre")

;; calfw
(add-to-list 'load-path "~/.emacs.d/emacs-calfw") ;github repo.
(require 'calfw)
(require 'calfw-ical)
(defun open-my-ical ()
  (interactive)
  (cfw:open-ical-calendar "http://www.google.com/calendar/ical/kawano%40yoshidumi.co.jp/public/basic.ics")
 (cfw:open-ical-calendar "https://www.google.com/calendar/ical/hjmkawano%40gmail.com/public/basic.ics")
  (cfw:open-ical-calendar "https://www.google.com/calendar/ical/jimbeam8y%40gmail.com/public/basic.ics")
  )
;; 月
(setq calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"])
;;; via http://sheephead.homelinux.org/2011/01/19/6571/
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "#8cd0d3")))
 '(cfw:face-regions ((t :foreground "#366060")))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold))))

;; 曜日
(setq calendar-day-name-array
      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

;; 週の先頭の曜日
(setq calendar-week-start-day 1) ; 日曜日は0, 月曜日は1

;; ;;;;
;; ;;;; smartrep.el
;; ;;;; via http://sheephead.homelinux.org/2011/12/19/6930/
;; (require 'smartrep)

;; (eval-after-load "org"
;;         '(progn
;;            (smartrep-define-key
;;             org-mode-map "C-c" '(("C-n" . (lambda ()
;;                                             (outline-next-visible-heading 1)))
;;                                  ("C-p" . (lambda ()
;;                                             (outline-previous-visible-heading 1)))))))

;; ;; flymake
;; (smartrep-define-key
;;     global-map "M-g" '(("M-n" . 'flymake-goto-next-error)
;;                        ("M-p" . 'flymake-goto-prev-error)))

(require 'flycheck-color-mode-line)


(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
;;;; helm
;;;; this section must be here.
(add-to-list 'load-path "~/.emacs.d/helm") ;github repo.
(require 'helm-config)
;; (require 'helm-migemo)
;; (setq helm-use-migemo t)


(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c o") 'helm-occur)
(define-key ctl-x-map "\C-f" 'helm-for-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-mode 1)


;;;;
;;;; The Solarized colour theme, ported to Emacs.
;;;; https://github.com/bbatsov/solarized-emacs
;; make the fringe stand out from the background

(setq solarized-distinct-fringe-background t)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

(add-to-list 'load-path "~/.emacs.d/solarized-emacs") ;github repo.
(require 'solarized)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-dark t)

;(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
;(load-theme 'solarized-dark t)

;;;;
;;;; powerline
;;;; https://github.com/milkypostman/powerline
(add-to-list 'load-path "~/.emacs.d/powerline") ;github repo.
(require 'powerline)
(powerline-default-theme)

;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(global-hl-line-mode t)
 '(initial-frame-alist (quote ((top . 10) (left . 410) (width . 130) (height . 58) (fullscreen . maximized))))
 '(show-paren-mode t)
 '(vc-follow-symlinks t))

(provide 'dot)
;;; dot.emacs ends here
