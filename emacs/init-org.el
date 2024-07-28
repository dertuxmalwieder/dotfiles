;;; init-org.el --- Emacs org-mode configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Improvements for org-mode.

;;; Code:

;; Hint: We use the upstream version instead of Emacs's own one
;;       so addins from Git won't fail us.
(use-package org
  :ensure nil
  :config
  ;; Better HTML export.
  (setq org-html-coding-system 'utf-8-unix)
  
  ;; Better inline code blocks.
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; More width for export.
  (setq org-ascii-text-width 400)

  ;; Enable Markdown export.
  (require 'ox-md))

;; Support org-mode import from a website:
(use-package org-web-tools)

;; Support my blog as well:
(use-package org2blog
  :init
  (setq org2blog/wp-show-post-in-browser t)
  (setq org2blog/wp-image-upload t))

;; Preview HTML:
;; (use-package org-preview-html :init (add-hook 'org-mode-hook #'org-preview-html-mode))

;; init-org.el ends here
