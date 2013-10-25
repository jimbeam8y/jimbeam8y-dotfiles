;;;;;;;; -*- emacs-lisp -*-

;; 日本語入力はibus経由のmozcを使う
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

;; C-SPC は Set Mark に使う
;(ibus-define-common-key ?C-s nil)
;; C-/ は Undo に使う
;(ibus-define-common-key ?C-/ nil)
;; IBusの状態によってカーソル色を変化させる ("on" "off" "disabled")
(setq ibus-cursor-color '("firebrick" "dark orange" "royal blue"))
;; すべてのバッファで入力状態を共有 (default ではバッファ毎にインプットメソッドの状態を保持)
(setq ibus-mode-local nil)
;; カーソル位置で予測候補ウィンドウを表示 (default はプリエディット領域の先頭位置に表示)
(setq ibus-prediction-window-position t)

;; isearch 時はオフに
(ibus-disable-isearch)

;; mini buffer ではオフに
(add-hook 'minibuffer-setup-hook 'ibus-disable)

;; Key bindings
(global-set-key (kbd "C-o") 'ibus-toggle)
 
(global-set-key (kbd "C-<f7>")
                (lambda ()
                  (interactive)
                  (shell-command-to-string
                   "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")))

;(setq quail-japanese-use-double-n t)

;; load-path
(add-to-list 'load-path "~/.emacs.d/")
;(add-to-list 'load-path "~/.emacs.d/auto-install/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")

;; global settings
(global-linum-mode t)
(which-function-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-hl-line-mode t)
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy t)
(setq transient-mark-mode t)

;;; key bind
(global-unset-key "\C-q")
(defvar ctl-q-map (make-keymap))
(define-key global-map (kbd "C-q") ctl-q-map)
(define-key ctl-q-map (kbd "c") 'elscreen-create)
(define-key ctl-q-map (kbd "k") 'elscreen-kill)
(define-key ctl-q-map (kbd "d") 'elscreen-dired)
(define-key ctl-q-map (kbd "n") 'elscreen-next)
(define-key ctl-q-map (kbd "C-n") 'elscreen-next)
(define-key ctl-q-map (kbd "p") 'elscreen-previous)
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

;(define-key ctl-q-map (kbd "C-a") 'your-favorite-funca)
;(define-key ctl-q-map (kbd "C-b") 'your-favorite-funcb)
(define-key ctl-q-map (kbd "C-q") 'quoted-insert)
;(define-key ctl-q-map (kbd "C-z") 'your-favorite-funcz)


(global-set-key (kbd "C-m") 'newline-and-indent)

;; マウスポインタの色を設定します。
(add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
;; モードラインの文字の色を設定します。
(set-face-foreground 'modeline "black")
;; モードラインの背景色を設定します。
(set-face-background 'modeline "LemonChiffon")
;; 選択中のリージョンの色を設定します。
;(set-face-background 'region "LightSteelBlue1")
;; モードライン（アクティブでないバッファ）の文字色を設定します。
(set-face-foreground 'mode-line-inactive "gray30")
;; モードライン（アクティブでないバッファ）の背景色を設定します。
(set-face-background 'mode-line-inactive "gray85")

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
(setq default-file-name-coding-system 'utf-8-unix) ;diredで日本語file名出力

;;;;;; package manager
(require 'package)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("mepla" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; (require 'auto-install)
;; (auto-install-update-emacswiki-package-name t)
;; (auto-install-compatibility-setup)

;;;;;; packages
;;; Aspell
(setq ispell-program-name "aspell")
(setq ispell-grep-command "grep")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]")))
;(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "aspell")

(mapc                                   ;; 以下flyspell-modeの設定
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   c-mode-common-hook ;; ここに書いたモードではコメント領域のところだけ
   emacs-lisp-mode-hook ;; flyspell-mode が有効になる
   ))

;;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;;; twitter
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-icon-mode nil)
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
    (other-window 1))
   (t
    (delete-other-windows))))

(add-hook 'twittering-new-tweets-hook (lambda ()
   (let ((n twittering-new-tweets-count))
     (start-process "twittering-notify" nil "notify-send"
                    "-i" "/usr/share/pixmaps/gnome-emacs.png"
                    "New tweets"
                    (format "You have %d new tweet%s"
                            n (if (> n 1) "s" ""))))))
;;; w3m
(require 'w3m-load)
(setq w3m-use-cookies t)                ;Enable Cookies

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
(elscreen-start)
(setq elscreen-prefix-key "\C-q")

;; Chrome text area edit
(require 'edit-server)
(edit-server-start)
(setq edit-server-new-frame nil)

;(require 'backlog)


;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
;; (load-file "~/.emacs.d/cedet-1.1/common/cedet.el")

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;(global-srecode-minor-mode 1)

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
 '(initial-frame-alist (quote ((top . 10) (left . 430) (width . 110) (height . 58))))
 '(show-paren-mode t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "lime green"))))
 '(hl-line ((t (:underline "dodger blue")))))
