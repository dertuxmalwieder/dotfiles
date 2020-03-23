(tool-bar-mode -1)
(toggle-scroll-bar -1)

(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(turn-on-font-lock)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(setq make-backup-files nil)

(setq gc-cons-threshold 100000000)


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

(use-package go-mode
    :ensure t
    :config
    (progn
       (unless (member "/opt/local/go/bin" (split-string (getenv "PATH") ":"))
         (setenv "PATH" (concat "/opt/local/go/bin:" (getenv "PATH"))))
       (setenv "GOPATH" (concat (getenv "HOME") "/go"))
       (setq gofmt-command (concat (getenv "GOPATH") "/bin/goimports"))
       (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package lsp-mode :ensure t)

(use-package lsp
    :commands 'lsp
    :ensure nil
    :config
    (add-hook 'go-mode-hook #'lsp))










(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
