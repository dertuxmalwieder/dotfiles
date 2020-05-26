;;; init.el --- Emacs configuration

;;; Commentary:

;; Making Emacs relatively usable on a MacBook.

;;; Code:

(tool-bar-mode -1)
(toggle-scroll-bar -1)

(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(turn-on-font-lock)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

(setq make-backup-files nil)

;; Automatically reload files edited elsewhere:
(setq global-auto-revert-mode t)

;; Wrap lines please:
(global-visual-line-mode t)

(setq gc-cons-threshold 100000000)

;; Nicer font:
(set-face-attribute 'default nil :family "Hack")

(when (eq system-type 'darwin)
  ;; Let's disable the right "Alt" key so I can still
  ;; use my German keyboard for entering German letters
  ;; on a Mac.
  (setq ns-right-alternate-modifier nil))

;; Remember where we are:
(desktop-save-mode t)

;; Don't keep open buffers though:
;; -- This will break Circe - disable for now.
;;(add-hook 'kill-emacs-hook (lambda () (desktop-clear)))

;; Stop chatting:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show the current line:
(global-hl-line-mode t)

;; Make window resizes undoable:
(when (fboundp 'winner-mode)
  (winner-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PACKAGE PREPARATION:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use straight.el instead of Emacs's default package.el for
;; managing installed packages. This might or might not be a
;; good idea.
(defvar bootstrap-version)

;; Enable ":ensure t"-like behavior:
(setq straight-use-package-by-default t)

;; Load (or download) straight.el:
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package:
(straight-use-package 'use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BUILT-IN PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perl programming:
;; Use the C Perl mode (may be better than the default one).
(defalias 'perl-mode 'cperl-mode)

;; Gnus preparation: Make it faster and nicer looking.
;; (Let's keep the account configuration in .gnus.el though.)
(use-package gnus
  :straight nil
  :config
  (setq gnus-always-read-dribble-file t)
  (setq gnus-read-active-file t)
  (gnus-add-configuration '(article (vertical 1.0 (summary .35 point) (article 1.0)))))

(use-package gnus-async
  :straight nil
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

;; org-mode improvements:
(use-package org
  :straight nil
  :config
  ;; Better HTML export.
  (setq org-html-coding-system 'utf-8-unix)
  
  ;; Better inline code blocks.
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3rd PARTY PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep my packages up-to-date:
;; Commented out. straight.el will do that for a while.
;;(use-package auto-package-update
;;  :ensure t
;;  :config
;;  (setq auto-package-update-interval 4) ;; ... days
;;  (setq auto-package-update-delete-old-versions t)
;;  (setq auto-package-update-hide-results t)
;;  (auto-package-update-maybe))

;; Some platforms (cough) don't update Emacs's path.
;; Make them.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; Multiple cursors:
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

;; Asynchronous Emacs:
(use-package async
  :ensure t)

;; Gopher:
(use-package gopher
  :ensure t)

;; Support org-mode import from a website:
(use-package org-web-tools
  :ensure t)

;; Support my blog as well:
(use-package org2blog
  :ensure t
  :config
  (setq org2blog/wp-show-post-in-browser t)
  ;; Keep the log-in data out of the public eye:
  (load-file "~/.emacs.d/org2blog-config.el"))

;; Preview HTML:
(use-package org-preview-html
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-preview-html-mode))

;; Web development:
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

;; RSS feed reader with some extras:
(use-package elfeed
  :ensure t)

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

;; Hook elfeed into Newsblur:
(use-package elfeed-protocol
  :ensure t
  :after elfeed
  :config
  (elfeed-protocol-enable)
  ;; Keep the log-in data out of the public eye:
  (load-file "~/.emacs.d/elfeed-config.el"))

;; A less shitty modeline:
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

;; Markdown support:
(use-package markdown-mode
  :if (executable-find "multimarkdown")
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

;; Use ripgrep instead of grep (if applicable):
(use-package rg
  :if (executable-find "rg")
  :ensure t
  :config
  (rg-enable-default-bindings))
  
;; Project-related functionalities:
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Syntax checking:
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Enable some icons throughout Emacs:
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-gnus
  :after all-the-icons
  :config
  (all-the-icons-gnus-setup))

;; Better regexp search&replace:
(use-package visual-regexp
  :ensure t)

(use-package visual-regexp-steroids
  :ensure t
  :after visual-regexp
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  (define-key global-map (kbd "C-c m") 'vr/mc-mark))

;; Lisp programming:
;; Use SLY as a CL subsystem.
(use-package sly
  :ensure t
  :config
  (when (eq system-type 'darwin)
    ;; Requires SBCL from MacPorts.
    (setq inferior-lisp-program "/opt/local/bin/sbcl")))

;; Go programming:
;; Install and set up the Go mode.
(use-package go-mode
  :ensure t
  :config
  (progn
    (unless (member "/opt/local/go/bin" (split-string (getenv "PATH") ":"))
      (setenv "PATH" (concat "/opt/local/go/bin:" (getenv "PATH"))))
    (setenv "GOPATH" (concat (getenv "HOME") "/go"))
    (setq gofmt-command (concat (getenv "GOPATH") "/bin/goimports"))))

;; Language Server Protocol:
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-snippet nil)
  :hook ((go-mode . lsp-deferred)
	 (perl-mode . lsp-deferred)))

(defun lsp-go-install-save-hooks ()
  "Install LSP hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'perl-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company auto-completion for code:
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :after company
  :config
  (setq company-backends
        ;; Let's keep company-mode unobtrusive in most cases.
        (quote
         (company-lsp company-bbdb company-xcode company-cmake company-capf company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords)))))

;; Counsel auto-completion for commands:
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

;; Ivy for most interactive stuff:
(use-package ivy
  :ensure t
  :defer 0.1
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate (quote full))
  :config (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; With icons:
(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode t))

;; Swiper for searching:
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

;; Smart parentheses:
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; Version Control enhancements:
(use-package darcsum
  :ensure t)

(use-package vc-fossil
  :ensure t
  :config
  (add-to-list 'vc-handled-backends 'Fossil t))

;; IRC:
(use-package circe
  :ensure t
  :commands (circe circe-set-display-handler)
  :config
  (enable-circe-color-nicks)
  (setq circe-reduce-lurker-spam t
        lui-time-stamp-position 'right-margin
        lui-time-stamp-format "%H:%M"
        circe-format-say "{nick:-16s} {body}"
        circe-format-self-say "<{nick:-16s}> {body}")
  ;; Again, keep the log-in data private:
  (load-file "~/.emacs.d/circe-config.el"))


;; Nicer theme:
(use-package nofrils-acme-theme
  :ensure t
  :init
  (load-theme 'nofrils-acme t))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "5e5c6fd080d6c0bce2d09b8ec6e3693c1a63c654")
 '(ivy-count-format "(%d/%d) " t)
 '(ivy-use-virtual-buffers t t)
 '(ivy-virtual-abbreviate (quote full) t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
