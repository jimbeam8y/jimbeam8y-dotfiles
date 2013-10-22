;;;;;;;;
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")


;(define-key global-map (kbd "C-o") 'toggle-input-method)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((top . 50) (left . 480) (width . 96) (height . 55))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "lime green")))))

(global-linum-mode t)
(which-function-mode 1)

(global-set-key (kbd "C-m") 'newline-and-indent)

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

;;;;;; packages
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("mepla" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(require 'w3m-load)
 ;;Enable Cookies
(setq w3m-use-cookies t)

(require 'recentf)
(recentf-mode 1)

;; completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'icomplete)
(icomplete-mode 99)
(eval-after-load "icomplete" '(progn (require 'icomplete+)))


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


(require 'helm-config)
(require 'helm-migemo)