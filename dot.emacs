;;; package --- Summary -*- emacs-lisp -*-
;;; Commentary:

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

;(load-theme 'deeper-blue t)
(load-theme 'wombat t)

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

;;; IME
(set-language-environment "Japanese")
(global-set-key (kbd "C-o") 'toggle-input-method)
(prefer-coding-system 'utf-8-unix)      ; 日本語入力のための設定

;;;;
;;;; mozc
;;;; via http://d.hatena.ne.jp/syohex/20120126/1327597912
;(setq mozc-leim-title "[I&#9825;Mozc]") ; modeline変更
(require 'mozc)
(setq default-input-method "japanese-mozc")
;;;(setq mozc-candidate-style 'echo-area)
(setq mozc-candidate-style 'overlay)

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
(add-to-list 'default-frame-alist '(alpha . (96 80)))

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


;;;;;; packages

;;;; Powerline
(require 'powerline)

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

;; JavaScript
(add-hook 'js-mode-hook 'flycheck-mode)
(add-hook 'javascipy-mode-hook
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
  (setq tab-width 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)
  (set-face-attribute 'web-mode-css-rule-face nil :foreground "Pink3")
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t)
  )
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
;; (add-to-list 'load-path "~/.emacs.d/jdee/lisp") ;github repo.
;; (load "jde-autoload")

;; (defun my-jde-mode-hook ()
;;   (require 'jde)

;;   (setq jde-build-function 'jde-ant-build) ; ビルドにantを利用する
;;   (setq jde-ant-read-target t)             ; targetを問い合わせる
;;   (setq jde-ant-enable-find t)             ; antに-findオプションを指定する(要らないかも)

;;   ;; complilationバッファを自動的にスクロールさせる
;;   (setq compilation-ask-about-save nil)
;;   (setq compilation-scroll-output 'first-error)

;;   (define-key jde-mode-map (kbd "C-c C-v .") 'jde-complete-minibuf)
;;   )

;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)

;;;;
;;;; JSON
;;;; via Emacs で JSON を Flymake する http://yak-shaver.blogspot.jp/2013/06/emacs-json-flymake.html
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(require 'flymake-json)
(add-hook 'json-mode-hook 'flymake-json-load)

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
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(which-func ((t (:background "navy" :foreground "white")))))

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


;;;;
;;;; 拾い物
;;;; via http://d.hatena.ne.jp/kitokitoki/20091129/p1
;;;; →こっちのほうがいいかもしれないのであとで導入する。
;;;;  Emacs で SQL を整形する: http://dev.ariel-networks.com/Members/matsuyama/sql-beautifying-in-emacs/
;;;; →モードそのものはここでちゃんと使えるようにする。
;;;;  sql-mode: Emacs から SQL 文を実行する: http://www.sixnine.net/roadside/sqlmode.html

(defun sqlf (start end)
  "リージョンのSQLを整形する"
  (interactive "r")
  (let ((case-fold-search t))
    (let* ((s (buffer-substring-no-properties start end))
           (s (replace-regexp-in-string "\\(select \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(update \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(insert into \\)\\(fuga\\)\\(fuga\\)" "\n\\2\n  " s))
           (s (replace-regexp-in-string "\\(delete from \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(create table \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(alter table \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(drop constraint \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(from \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(exists \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(where \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(values \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(order by \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(group by \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(having \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(left join \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(left outer join )\\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(right join \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(right outer join \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(inner join \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(cross join \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(union join \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(and \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(or \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(any \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on update restrict \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on update cascade \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on update set null \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on update no action \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on delete restrict \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on delete cascade \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on delete set null \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(on delete no action \\)" "\n\\1\n  " s))
           (s (replace-regexp-in-string "\\(,\\)" "\\1\n  " s)))
    (save-excursion
      (insert s)))))



;; ;; flymake
;; (smartrep-define-key
;;     global-map "M-g" '(("M-n" . 'flymake-goto-next-error)
;;                        ("M-p" . 'flymake-goto-prev-error)))

(require 'flycheck-color-mode-line)


(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;;;; Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;; helm
;;;; this section must be here.
(add-to-list 'load-path "~/.emacs.d/helm") ;github repo.
(require 'helm-config)
;; (require 'helm-migemo)
;; (setq helm-use-migemo t)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c f") 'helm-occur)
; (define-key ctl-x-map "\C-f" 'helm-for-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-mode 1)

(require 'helm-ls-git)
(global-set-key (kbd "C-<f6>") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-hl-line-mode t)
 '(initial-frame-alist (quote ((top . 10) (left . 410) (width . 130) (height . 58) (fullscreen . maximized))))
 '(show-paren-mode t)
 '(vc-follow-symlinks t))

(provide 'dot)
;;; dot.emacs ends here
