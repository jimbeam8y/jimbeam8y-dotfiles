;;;;;;;; -*- emacs-lisp -*-
;;ガベッジコレクションを実行するまでの割り当てメモリの閾値を増やす
(setq gc-cons-threshold (* 50 gc-cons-threshold))
;;ログの記録量を増やす
(setq message-log-max 10000)
;;履歴存数を増やす
(setq history-length 1000)
;;重複する履歴は保存しない
(setq history-delete-duplicates t)

;;; load-path
(add-to-list 'load-path "~/.emacs.d/")
;; via http://forums.pcbsd.org/showthread.php?t=17911

;;; mozc
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
; (setq mozc-candidate-style 'overlay)
(setq mozc-candidate-style 'echo-area)
(global-set-key (kbd "C-o") 'toggle-input-method)

;; タイトルバーに日時を表示する
(setq display-time-string-forms
      '((format "%s/%s(%s)%s:%s" month day dayname 24-hours minutes)))
(display-time)
(cond (window-system
       (setq frame-title-format
	     '((multiple-frames "")
	       display-time-string))
       (remove-hook 'global-mode-string 'display-time-string)))

;;;;;;; Coolな設定
;;;;;;; via http://sakito.jp/emacs/emacs23.html#id17
;;;;;;;
;; 垂直スクロール用のスクロールバーを付けない
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
;; 背景の透過
;; (add-to-list 'default-frame-alist '(alpha . (85 20)))
(add-to-list 'default-frame-alist '(alpha . (92 70)))

;;; フォントの設定
(when (find-font (font-spec :family "UmePlus Gothic"))
  ;; http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html
  (set-face-attribute 'default
                      nil
                      :family "UmePlus Gothic"
                      :height 120)
  (add-to-list 'default-frame-alist '(font . "UmePlus Gothic-12"))
  (set-fontset-font nil
                    'unicode
                    (font-spec :family "UmePlus Gothic")
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
(global-whitespace-mode 1) ;; 常に whitespace-mode だと動作が遅くなる場合がある

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)
;; EOB を表示
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
;; 変更点に色付け
(global-highlight-changes-mode t)
;; 初期は非表示として highlight-changes-visible-mode で表示する
(setq highlight-changes-visibility-initial-state nil)
(global-set-key (kbd "M-]") 'highlight-changes-next-change)
(global-set-key (kbd "M-[") 'highlight-changes-previous-change)

;;;;;;;;
;;;;;;;;
;;;;;;;;

;; global settings
(show-paren-mode t)
(global-linum-mode t)
(which-function-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-hl-line-mode t)
(global-hi-lock-mode 1)
(setq vc-follow-symlinks t)
(setq hi-lock-file-patterns-policy t)
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

;; coding-system
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-emacs-unix 'utf-8-emacs-unix)
            ))

(prefer-coding-system 'utf-8-unix)      ; 日本語入力のための設定

(require 'wdired)
(setq default-file-name-coding-system 'utf-8-unix) ;diredで日本語file名出力

(add-hook 'calendar-load-hook
          (lambda ()
            (require 'japanese-holidays)
            (setq calendar-holidays
                  (append japanese-holidays local-holidays other-holidays))))

;;; 動かない… Meadow専用か？
;(require 'gnuserv)
;(gnuserv-start)

;;;;;; package manager
(require 'package)
(add-to-list 'package-archives '("mepla" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
(package-initialize)

(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
;(auto-install-compatibility-setup)


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(setq
 el-get-sources
 '(el-get                               ; el-get is self-hosting
   escreen                              ; screen for emacs, C-\ C-h
   php-mode-improved                    ; if you're into php...
   switch-window                        ; takes over C-x o
   auto-complete                        ; complete as you type with overlays
   zencoding-mode                       ; http://www.emacswiki.org/emacs/ZenCoding

   (:name buffer-move                   ; have to add your own keys
          :after (lambda ()
                   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex                          ; a better (ido like) M-x
          :after (lambda ()
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit                                 ; git meet emacs, and a binding
          :after (lambda ()
                   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change              ; move pointer back to last change
          :after (lambda ()
                   ;; when using AZERTY keyboard, consider C-x C-_
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

(unless (string-match "apple-darwin" system-configuration)
  (loop for p in '(color-theme          ; nice looking emacs
                   color-theme-tango    ; check out color-theme-solarized
                   )
        do (add-to-list 'el-get-sources p)))

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
(when (el-get-executable-find "cvs")
  (add-to-list 'el-get-sources 'emacs-goodies-el)) ; the debian addons for emacs

(when (el-get-executable-find "svn")
  (loop for p in '(psvn                 ; M-x svn-status
                   yasnippet            ; powerful snippet mode
                   )
        do (add-to-list 'el-get-sources p)))

;; install new packages and init already installed packages
(el-get 'sync)

;;;;;; packages
;;; color-theme
;;; via http://d.hatena.ne.jp/fatrow/20101025/emacs_color_theme
(require 'color-theme)
(require 'zenburn)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
;     (color-theme-ld-dark)
     (color-theme-zenburn)
     ))

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

;;; like a 'screen'
;;; via http://www.morishima.net/~naoto/elscreen-ja/
;;; code from GitHub.
(add-to-list 'load-path "~/.emacs.d/elscreen") ;github repo.
(require 'elscreen)
(elscreen-start)
(load "elscreen-gf" "ElScreen-GF" t) ;; TODO GNU GLOBAL で使ってみたい。
(require 'elscreen-color-theme)
(require 'elscreen-dired)
(require 'elscreen-w3m)
(require 'elscreen-server)
(require 'elscreen-wl)

(defmacro elscreen-create-automatically (ad-do-it)
  `(if (not (elscreen-one-screen-p))
       ,ad-do-it
     (elscreen-create)
     (elscreen-notify-screen-modification 'force-immediately)
     (elscreen-message "New screen is automatically created")))

(defadvice elscreen-next (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-previous (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))

(defadvice elscreen-toggle (around elscreen-create-automatically activate)
  (elscreen-create-automatically ad-do-it))
;;; key bind for elscreen
(global-unset-key "\C-q")
(defvar ctl-q-map (make-keymap))
(define-key global-map (kbd "C-q") ctl-q-map)
(define-key ctl-q-map (kbd "c") 'elscreen-create)
(define-key ctl-q-map (kbd "k") 'elscreen-kill)
(define-key ctl-q-map (kbd "d") 'elscreen-dired)
(define-key ctl-q-map (kbd "n") 'elscreen-next)
(define-key ctl-q-map (kbd "C-n") 'elscreen-next)
(define-key ctl-q-map (kbd "p") 'elscreen-previous)
(define-key ctl-q-map (kbd "s") 'my-google-search)
(define-key ctl-q-map (kbd "C-p") 'elscreen-previous)
(define-key ctl-q-map (kbd "0") 'elscreen-jump-0)
(define-key ctl-q-map (kbd "1") 'elscreen-jump)
(define-key ctl-q-map (kbd "2") 'elscreen-jump)
(define-key ctl-q-map (kbd "3") 'elscreen-jump)
(define-key ctl-q-map (kbd "4") 'elscreen-jump)
(define-key ctl-q-map (kbd "5") 'elscreen-jump)
(define-key ctl-q-map (kbd "6") 'elscreen-jump)
(define-key ctl-q-map (kbd "7") 'elscreen-jump)
(define-key ctl-q-map (kbd "8") 'elscreen-jump)
(define-key ctl-q-map (kbd "9") 'elscreen-jump-9)
(define-key ctl-q-map (kbd "?") 'elscreen-help)
(define-key ctl-q-map (kbd "t") 'my-twit)
(define-key ctl-q-map (kbd "w") 'elscreen-w3m)
;;;;;;;; ：
;;(define-key ctl-q-map (kbd "C-a") 'your-favorite-funca)
;;(define-key ctl-q-map (kbd "C-b") 'your-favorite-funcb)
(define-key ctl-q-map (kbd "C-q") 'quoted-insert)
;;(define-key ctl-q-map (kbd "C-z") 'your-favorite-funcz)

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
;; 				     w3m-default-directory)
;; 	)
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
(setq edit-server-new-frame nil)

;; Wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(require 'mime-w3m)

(require 'edbi)
(autoload 'edbi:open-db-viewer "edbi")

;;; Magit
(add-to-list 'load-path "~/.emacs.d/magit") ;github repo.
(require 'magit)

;;;;; e2em
;;;;;
;;;;;最小の e2wm 設定例
;; with Magit
(auto-install-from-url "https://github.com/kiwanami/emacs-window-manager/raw/master/e2wm-vcs.el")
(require 'e2wm-config)

(global-set-key (kbd "M-+") 'e2wm:start-management)

(load "e2wm-edbi-pre")

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
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-regions ((t :foreground "#366060")))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-periods ((t :foreground "#8cd0d3")))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f"))))

;; 曜日
(setq calendar-day-name-array
      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

;; 週の先頭の曜日
(setq calendar-week-start-day 1) ; 日曜日は0, 月曜日は1

;;;; 動かない
;(require 'backlog)

;;;;; mode hook
;; Emacs-Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ))

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

;; helm
;;;; this section must be here.
(require 'helm-config)
(require 'helm-migemo)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-mini)
;(global-set-key "\C-x\C-f" 'helm-for-files)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-hl-line-mode t)
 '(initial-frame-alist (quote ((top . 10) (left . 410) (width . 130) (height . 58) (fullscreen . maximized))))
 '(show-paren-mode t)
 '(vc-follow-symlinks t))
