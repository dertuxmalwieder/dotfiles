(tool-bar-mode -1)
(toggle-scroll-bar -1)

(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(turn-on-font-lock)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

(setq make-backup-files nil)

(setq gc-cons-threshold 100000000)

(when (eq system-type 'darwin)
  ;; Let's disable the right "Alt" key so I can still
  ;; use my German keyboard for entering German letters
  ;; on a Mac.
  (setq ns-right-alternate-modifier nil))


;;;;;;;;;;;;;;;;;;;;;;;
;;;; PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Initialize MELPA packages:
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Keep my packages up-to-date:
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Some platforms (cough) don't update Emacs's path.
;; Make them.
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

;; Multiple cursors:
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

;; Go programming:
(use-package go-mode
  :ensure t
  :config
  (progn
    (unless (member "/opt/local/go/bin" (split-string (getenv "PATH") ":"))
      (setenv "PATH" (concat "/opt/local/go/bin:" (getenv "PATH"))))
    (setenv "GOPATH" (concat (getenv "HOME") "/go"))
    (setq gofmt-command (concat (getenv "GOPATH") "/bin/goimports"))
    (add-hook 'before-save-hook 'gofmt-before-save)))

;; Language Server Protocol:
(use-package lsp-mode :ensure t)

(use-package lsp
  :commands 'lsp
  :ensure nil
  :config
  (add-hook 'go-mode-hook #'lsp))

;; Counsel auto-completion:
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode))

;; Ivy for most interactive stuff:
(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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
  (require 'smartparens-config))

;; Version Control enhancements:
(use-package darcsum
  :ensure t)

(use-package vc-fossil
  :ensure t
  :config
  (add-to-list 'vc-handled-backends 'Fossil t))

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
 '(custom-safe-themes
    (nofrils-acme-theme auto-package-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
