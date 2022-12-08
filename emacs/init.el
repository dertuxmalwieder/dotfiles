;;; init.el --- Emacs configuration

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
  (setq ns-right-alternate-modifier nil)

  ;; Enable right-clicks for Flyspell:
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))))

;; Remember where we are:
(desktop-save-mode t)

;; Stop chatting:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show the current line:
(global-hl-line-mode t)

;; Make window resizes undoable:
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Don't warn about parenthesis mismatch in non-
;; programming modes:
(setq blink-matching-paren nil)
(add-hook 'prog-mode-hook (lambda () (setq-local blink-matching-paren t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PACKAGE PREPARATION:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use straight.el instead of Emacs's default package.el for
;; managing installed packages. This might or might not be a
;; good idea.
(defvar bootstrap-version)

;; Enable ":ensure t"-like behavior:
(setq straight-use-package-by-default t)

;; Use the "develop" branch:
(setq straight-repository-branch "develop")

;; Load (or download) straight.el:
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

;; IRC:
(use-package erc
  :straight nil
  :config
  (add-to-list 'desktop-modes-not-to-save 'erc-mode)
  ;; Keep the log-in data out of the public eye:
  (load-file "~/.emacs.d/erc-config.el"))

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

;; LSP (requires Emacs 29, eglot is NOT in older versions!):
(use-package eglot
  :unless (version< emacs-version "29.1")
  :straight nil
  :hook ((js-mode-hook . eglot-ensure)
         (typescript-mode-hook . eglot-ensure)
         (python-mode-hook . eglot-ensure)
         (go-mode-hook . eglot-ensure)
         (cperl-mode-hook . eglot-ensure)
         (c-mode-hook . eglot-ensure))
  :custom
  (eglot-autoshutdown t))
  
;; org-mode improvements:
;; Hint: We use the upstream version instead of Emacs's own one
;;       so addins from Git won't fail us.
(use-package org
  ;; :straight nil
  :config
  ;; Better HTML export.
  (setq org-html-coding-system 'utf-8-unix)
  
  ;; Better inline code blocks.
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; Enable Markdown export.
  (require 'ox-md))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3rd PARTY PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some platforms (cough) don't update Emacs's path.
;; Make them.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
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

;; Emojis:
(use-package emojify
  :ensure t
  :commands emojify-mode
  :config
  (add-hook 'erc-mode-hook 'emojify-mode))

;; Undo/redo:
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Switch and split windows visually:
(use-package switch-window
  :ensure t
  :config
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

;; Support org-mode import from a website:
(use-package org-web-tools
  :ensure t)

;; Support my blog as well:
(use-package org2blog
  :ensure t
  :config
  (setq org2blog/wp-show-post-in-browser t)
  (setq org2blog/wp-image-upload t)
  ;; Keep the log-in data out of the public eye:
  (load-file "~/.emacs.d/org2blog-config.el"))

;; Mastodon:
(use-package mastodon
  :ensure t
  :config
  (setq mastodon-toot--enable-custom-instance-emoji t)
  ;; Keep the log-in data out of the public eye:
  (load-file "~/.emacs.d/mastodon-config.el"))

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

;; A less shitty modeline:
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  (when (daemonp)
    (setq doom-modeline-icon t))
  (setq doom-modeline-minor-modes t))

;; ... with less minor mode cruft:
(use-package minions
  :ensure t
  :config
  (minions-mode 1))

;; Markdown support:
(use-package markdown-mode
  :if (executable-find "multimarkdown")
  :ensure t
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Writing mode:
(use-package olivetti
  :ensure t
  :after markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'olivetti-mode))

;; Use ripgrep instead of grep (if applicable):
(use-package rg
  :if (executable-find "rg")
  :ensure t
  :config
  (rg-enable-default-bindings))

;; Paste online:
(use-package dpaste
  :ensure t
  :config
  (setq dpaste-poster "tux0r")
  ;; Paste with C-c p:
  (global-set-key (kbd "C-c p") 'dpaste-region-or-buffer))

;; Spell checking:
(use-package guess-language
  :ensure t
  :if (executable-find "ispell")
  :config
  (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
  (setq guess-language-languages '(de en))
  (setq guess-language-min-paragraph-length 50))

;; Project-related functionalities:
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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

;; Expand selections:
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Lisp programming:
;; Use SLIME as a CL subsystem.
(use-package slime
  :ensure t
  :config
  (require 'slime-autoloads)
  (slime-setup '(slime-fancy))
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))
  (if (executable-find "ros")
      (setq inferior-lisp-program "ros -Q run")
    (when (eq system-type 'darwin)
      (setq inferior-lisp-program "/opt/pkg/bin/sbcl"))))

;; JS programming:
;; Use a less bad JavaScript mode.
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; Go programming:
;; Install and set up the Go mode.
(use-package go-mode
  :ensure t
  :config
  (progn
    (setenv "GOPATH" (concat (getenv "HOME") "/go"))
    (setq gofmt-command (concat (getenv "GOPATH") "/bin/goimports"))))

;; COBOL programming:
(use-package cobol-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (append
         '(("\\.cob\\'" . cobol-mode)
           ("\\.cbl\\'" . cobol-mode)
           ("\\.cpy\\'" . cobol-mode))
         auto-mode-alist)))

;; Rust programming:
(use-package rustic
  :ensure t
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  (remove-hook 'rustic-mode-hook 'flycheck-mode))
  
;; Corfu auto-completion for code:
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-separator ?\s)
  :hook ((prog-mode . corfu-mode)))

;; Gopher:
(use-package elpher
  :ensure t)

;; Vertico for most interactive stuff:
(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Icons for the minibuffer:
(use-package all-the-icons-completion
  :after vertico
  :ensure t)

;; Minibuffer improvements:
(use-package marginalia
  :after all-the-icons-completion
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode))

;; Orderless search:
(use-package orderless
  :after vertico
  :ensure t)

;; Completion via consult:
(use-package consult
  :after orderless
  :ensure t
  :config
  (define-key global-map (kbd "C-s") 'consult-line)
  (setq completion-styles '(orderless flex))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((eglot (styles . (orderless flex)))))
  (add-hook 'vertico-mode-hook (lambda ()
                                 (setq completion-in-region-function
                                       (if vertico-mode
                                           #'consult-completion-in-region
                                         #'completion--in-region))))
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple))

;; Smart parentheses:
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'perl-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'smartparens-mode))

;; Clickable links everywhere:
(use-package orglink
  :ensure t
  :config
  (global-orglink-mode))

;; vterm instead of Emacs's terminal:
(use-package vterm
  ;; Update the module automatically:
  :unless (eq system-type 'windows-nt)
  :straight (:post-build ((unless (eq system-type 'windows-nt)
                            (let ((vterm-always-compile-module t))
                              (require 'vterm)))))
  :config
  ;; Disable the highlighting of the current line
  ;; for the virtual terminal:
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

;; Version Control enhancements:
;;(use-package darcsum
;;  :ensure t)

(use-package vc-fossil
  :straight (:host github :branch "trunk")
  :config
  (add-to-list 'vc-handled-backends 'Fossil t))

;; Use ligatures if possible:
(let ((ligatures `((?-  ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                   (?/  ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                   (?*  ,(regexp-opt '("*>" "***" "*/")))
                   (?<  ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||" "<|>" "<:" "<>" "<-<"
                                       "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                       "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                   (?:  ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=")))
                   (?=  ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                   (?!  ,(regexp-opt '("!==" "!!" "!=")))
                   (?>  ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                   (?&  ,(regexp-opt '("&&&" "&&")))
                   (?|  ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                   (?.  ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                   (?+  ,(regexp-opt '("+++" "+>" "++")))
                   (?\[ ,(regexp-opt '("[||]" "[<" "[|")))
                   (?\{ ,(regexp-opt '("{|")))
                   (?\? ,(regexp-opt '("??" "?." "?=" "?:")))
                   (?#  ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                   (?\; ,(regexp-opt '(";;")))
                   (?_  ,(regexp-opt '("_|_" "__")))
                   (?\\ ,(regexp-opt '("\\" "\\/")))
                   (?~  ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                   (?$  ,(regexp-opt '("$>")))
                   (?^  ,(regexp-opt '("^=")))
                   (?\] ,(regexp-opt '("]#"))))))
  (dolist (char-regexp ligatures)
    (apply (lambda (char regexp) (set-char-table-range
                                  composition-function-table
                                  char `([,regexp 0 font-shape-gstring])))
           char-regexp))
  ;; Nicer font that actually uses the ligatures:
  (set-face-attribute 'default nil :family "Fira Code"))


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
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
