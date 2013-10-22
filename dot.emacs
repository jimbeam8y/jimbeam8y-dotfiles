;;;;;;;; -*- emacs-lisp -*-

;; ���ܸ����Ϥ�ibus��ͳ��mozc��Ȥ�
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

;; C-SPC �� Set Mark �˻Ȥ�
;(ibus-define-common-key ?C-s nil)
;; C-/ �� Undo �˻Ȥ�
;(ibus-define-common-key ?C-/ nil)
;; IBus�ξ��֤ˤ�äƥ������뿧���Ѳ������� ("on" "off" "disabled")
(setq ibus-cursor-color '("firebrick" "dark orange" "royal blue"))
;; ���٤ƤΥХåե������Ͼ��֤�ͭ (default �ǤϥХåե���˥���ץåȥ᥽�åɤξ��֤��ݻ�)
(setq ibus-mode-local nil)
;; ����������֤�ͽ¬���䥦����ɥ���ɽ�� (default �ϥץꥨ�ǥ��å��ΰ����Ƭ���֤�ɽ��)
(setq ibus-prediction-window-position t)

;; isearch ���ϥ��դ�
(ibus-disable-isearch)

;; mini buffer �Ǥϥ��դ�
(add-hook 'minibuffer-setup-hook 'ibus-disable)

;; Keybindings
(global-set-key (kbd "C-o") 'ibus-toggle)
;(global-set-key (kbd "M-o") (lambda () (interactive) (ibus-enable "m17n:sa:vz-prefix")))
;(global-set-key (kbd "C-o") (lambda () (interactive) (ibus-enable "mozc-jp")))
 
(global-set-key (kbd "C-<f7>")
                (lambda ()
                  (interactive)
                  (shell-command-to-string
                   "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")))

;(setq quail-japanese-use-double-n t)


;; load-path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")


;; global settings
(global-linum-mode t)
(which-function-mode 1)
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

(prefer-coding-system 'utf-8-unix)	; ���ܸ����ϤΤ��������
(setq default-file-name-coding-system 'utf-8-unix) ;dired�����ܸ�file̾����

;;;;;; package manager
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("mepla" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;;;;;; packages

;;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;;; twitter
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-icon-mode nil)
(setq twittering-display-remaining t)
(defun my-twit ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
;  (split-window-horizontally)
  (balance-windows)
  (twit)
  (cond
   ((twittering-account-authorized-p)
    (switch-to-buffer ":home")
    (other-window 1)
    (twittering-visit-timeline ":replies")
    (other-window 1))
;    (twittering-visit-timeline "jimbeam8y/friends")
;    (other-window 1))
   (t
    (delete-other-windows))))

;;; w3m
(require 'w3m-load)
(setq w3m-use-cookies t)		;Enable Cookies

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

;; Chrome text area edit
(require 'edit-server)
(edit-server-start)

(require 'backlog)


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
;;; ����ǥ�ȿ�
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

;;; via http://d.hatena.ne.jp/kitokitoki/20121103/p3
;; �ե�����Τ���ǥ��쥯�ȥ������ Nautilus �򳫤�
(defun exec-filemanager ()
  (interactive)
  (call-process "nautilus" nil nil nil "--no-desktop" "-n"
                (or (file-name-directory buffer-file-name)
                    default-directory)))

(defalias 'nau 'exec-filemanager)
;; dired �ǳ����Ƥ���ǥ��쥯�ȥ�� nautilus �ǳ����ؿ��⤢��ޤ�����
;; http://qiita.com/items/2620874c802db60c99f9
(defun dired-open-nautilus ()
  (interactive)
  (call-process "nautilus" nil 0 nil (dired-current-directory)))
(define-key dired-mode-map "e" 'dired-open-nautilus)
(defalias 'naud 'dired-open-nautilus)

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
 '(initial-frame-alist (quote ((top . 50) (left . 480) (width . 96) (height . 55))))
 '(show-paren-mode t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "lime green")))))
