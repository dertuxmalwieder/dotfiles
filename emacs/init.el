;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Making Emacs relatively usable.

;;; Code:

;; Wrap lines please:
(global-visual-line-mode t)

(when (eq system-type 'darwin)
  ;; Disable "ls --dired" (macOS cannot do that).
  (setq dired-use-ls-dired nil)
  
  ;; Let's disable the right "Alt" key so I can still
  ;; use my German keyboard for entering German letters
  ;; on a Mac.
  (setq ns-right-alternate-modifier nil))

;; Remember where we are:
;; (desktop-save-mode t)

;; Overwrite by typing:
(delete-selection-mode 1)

;; Stop chatting:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show the current line:
(global-hl-line-mode t)

;; Don't warn about parenthesis mismatch in non-
;; programming modes:
(setq blink-matching-paren nil)
(add-hook 'prog-mode-hook '(lambda () (setq-local blink-matching-paren t)))

;; Nicer font (UI and prog-mode):
(if (fboundp 'set-default-font)
    (set-default-font "Hack 10" nil t)
  (set-face-attribute 'default nil :family "Hack"))

;; (join-lines) should respect comments:
;; Source: https://tony-zorman.com/posts/join-lines-comments.html
(advice-add 'delete-indentation :around
            (lambda (old-fun &optional arg beg end)
              (let ((fill-prefix comment-start))
                (funcall old-fun arg beg end))))

;; Utility functions:
(load "~/.emacs.d/utilities")


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PACKAGE PREPARATION:
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BUILT-IN PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load private user data first:
(load "~/.emacs.d/private-settings") ;; !!! you might want to delete this.

;; org-mode:
(load "~/.emacs.d/init-org")

;; Perl programming:
;; Use the C Perl mode (may be better than the default one).
(defalias 'perl-mode 'cperl-mode)

;; IRC:
(load "~/.emacs.d/init-erc")

;; Usenet:
(load "~/.emacs.d/init-gnus")

;; LSP (requires Emacs 29, eglot is NOT in older versions!):
(use-package eglot
  :ensure nil
  :unless (version< emacs-version "29.1")
  :hook ((js-mode-hook . eglot-ensure)
         (js-ts-mode-hook . eglot-ensure)
         (typescript-mode-hook . eglot-ensure)
         (python-mode-hook . eglot-ensure)
         (python-ts-mode-hook . eglot-ensure)
         (rust-mode-hook . eglot-ensure)
         (rust-ts-mode-hook . eglot-ensure)
         (go-mode-hook . eglot-ensure)
         (go-ts-mode-hook . eglot-ensure)
         (cperl-mode-hook . eglot-ensure)
         (perl-ts-mode-hook . eglot-ensure)
         (c-mode-hook . eglot-ensure)
         (c-ts-mode-hook . eglot-ensure)
         (markdown-mode-hook . eglot-ensure))
  :custom
  (eglot-autoshutdown t))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3rd PARTY PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Asynchronous package management (etc.):
(use-package async
  :init
  (setq async-bytecomp-allowed-packages '(all))
  :config
  (async-bytecomp-package-mode 1))

;; Some platforms (cough) don't update Emacs's path.
;; Make them.
(when (or (eq system-type 'darwin) (daemonp))
  (use-package exec-path-from-shell
    ;; Hack: I *always* use non-standard shells ... use
    ;; one that exec-path-from-shell actually knows.
    :init
    (setq exec-path-from-shell-shell-name "zsh")
    (setq exec-path-from-shell-check-startup-files nil)
    :config
    (exec-path-from-shell-initialize))
  
  (use-package alert
    ;; Actually working notifications on macOS without dbus
    :config
    (define-advice notifications-notify
        (:override (&rest params) using-alert)
      (alert (plist-get params :body)
             :style 'osx-notifier
             :title (plist-get params :title)))))

;; E-mail:
(load "~/.emacs.d/mu4e-config.el")

;; Multiple cursors:
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

;; Emojis:
(use-package emojify
  :config
  (global-emojify-mode))

;; elfeed for RSS:
(load "~/.emacs.d/init-elfeed.el")

;; Better web browsing:
(if (fboundp 'xwidget-webkit-browse-url)
    ;; If this Emacs was compiled with --with-xwidgets, use one for browsing.
    (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  ;; Else, use w3m.
  (when (executable-find "w3m")
    (use-package w3m
      :init
      (setq browse-url-browser-function 'w3m-browse-url))))

;; Undo/redo:
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Better diff view:
(use-package diffview
  :config
  (add-hook 'diff-mode-hook 'diffview-current))

;; Better CSV editing:
(use-package csv-mode
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode)
  (add-hook 'csv-mode-hook '(lambda () (interactive) (toggle-truncate-lines nil))))

;; Switch and split windows visually:
(use-package switch-window
  :init
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
  (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
  (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
  (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

  (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
  (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
  (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

  (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

  (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer))

;; Move lines/regions:
(use-package move-text
  :config
  (move-text-default-bindings))

;;;;;;; Messaging ;;;;;;;
;; Mastodon:
(use-package mastodon
  :config
  (setq mastodon-toot--enable-custom-instance-emoji t))

;; Matrix:
(use-package ement)

;; Telegram:
(use-package telega
  ;; Note: REQUIRES TDLIB!
  :config
  (setq telega-server-libs-prefix "/opt/homebrew/"))

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Web development:
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

(use-package web-beautify
  :config
  (add-hook 'js2-mode-hook
            (lambda () (add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
  (add-hook 'html-mode-hook
            (lambda () (add-hook 'before-save-hook 'web-beautify-html-buffer t t)))
  (add-hook 'css-mode-hook
            (lambda () (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

;; A less shitty modeline:
(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (when (daemonp)
    (setq doom-modeline-icon t))
  (setq doom-modeline-minor-modes t))

;; ... with less minor mode cruft:
(use-package minions
  :config
  (minions-mode 1))

;; Markdown support:
(when (executable-find "multimarkdown")
  (use-package markdown-mode
    :init
    (setq markdown-command "multimarkdown")
    (setq markdown-asymmetric-header t)
    (setq markdown-header-scaling t)
    (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

  ;; Writing mode, depends on Markdown-Mode:
  (use-package olivetti
    :config
    (add-hook 'markdown-mode-hook 'olivetti-mode)))

;; Spell checking:
(use-package guess-language
  :config
  ;; ... with aspell, not ispell:
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list")
  
  (when (executable-find "aspell")
    ;; Automatically enable smart spell checking:
    (add-hook 'text-mode-hook
              '(lambda ()
                 (guess-language-mode 1)
                 (flyspell-mode 1))))
  (setq guess-language-languages '(de en))
  (setq guess-language-min-paragraph-length 50))

;; Enable right-clicks for Flyspell:
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Enable some icons throughout Emacs:
(use-package all-the-icons)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-gnus
  :config
  (all-the-icons-gnus-setup))

;; Better regexp search&replace:
(use-package visual-regexp)
(use-package visual-regexp-steroids
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  (define-key global-map (kbd "C-c m") 'vr/mc-mark))

;; Expand selections:
(use-package expand-region
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Lisp programming:
;; Use SLIME as a CL subsystem.
(use-package slime
  :config
  (require 'slime)
  (slime-setup '(slime-fancy))
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))
  (if (executable-find "ros")
      (setq inferior-lisp-program "ros -Q run")
    (setq inferior-lisp-program (executable-find "sbcl"))))

;; Go programming:
;; Install and set up the Go mode.
(use-package go-mode
  :init
  (setenv "GOPATH" (concat (getenv "HOME") "/go")))

;; COBOL programming:
(use-package cobol-mode
  :init
  (setq auto-mode-alist
        (append
         '(("\\.cob\\'" . cobol-mode)
           ("\\.cbl\\'" . cobol-mode)
           ("\\.cpy\\'" . cobol-mode))
         auto-mode-alist)))

;; Rust programming:
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  (remove-hook 'rustic-mode-hook 'flycheck-mode))

;; Corfu auto-completion for code:
(use-package corfu
  :init
  (setq corfu-auto t)
  (setq corfu-separator ?\s)
  (add-hook 'prog-mode-hook 'corfu-mode))

;; Gopher:
(use-package elpher)

;; Vertico for most interactive stuff:
(use-package vertico
  :config
  (setq vertico-cycle t)
  (vertico-mode))

;; Icons for the minibuffer:
(use-package all-the-icons-completion)

;; Minibuffer improvements:
(use-package marginalia
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))                               
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode))

;; Orderless search:
(use-package orderless)

;; Completion via consult:
(use-package consult
  :config
  (define-key global-map (kbd "C-s") 'consult-line)
  (setq completion-styles '(orderless flex))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((eglot (styles . (orderless flex)))))
  (add-hook 'vertico-mode-hook '(lambda ()
                                  (setq completion-in-region-function
                                        (if vertico-mode
                                            #'consult-completion-in-region
                                          #'completion--in-region))))
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple))

;; Smart parentheses:
(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))

;; Clickable links everywhere:
(use-package orglink
  :config
  (global-orglink-mode))

;; EAT instead of Emacs's terminal:
(use-package eat
  ;; Integrate with eshell:
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package vc-fossil
  :init
  (add-to-list 'vc-handled-backends 'Fossil t))

;; PostgreSQL stuff:
(use-package pg)
(use-package pgmacs
  :ensure t
  :vc (:url "https://github.com/emarsden/pgmacs")
  :after pg)

;; Tree-sitter configuration:
(unless (version< emacs-version "29.1")
  (load "~/.emacs.d/init-treesitter"))

;; Use ligatures if possible (requires the Fira Code Symbol font)
;; for programming:
(use-package fira-code-mode
  :config
  (fira-code-mode-set-font)
  (add-hook 'prog-mode-hook 'fira-code-mode))

;; Nicer theme:
(use-package nofrils-acme-theme
  :config
  (load-theme 'nofrils-acme t))

(provide 'init)
;;; init.el ends here
