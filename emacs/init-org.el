;;; init-org.el --- Emacs org-mode configuration

;;; Commentary:

;; Improvements for org-mode.

;;; Code:

;; Hint: We use the upstream version instead of Emacs's own one
;;       so addins from Git won't fail us.
(elpaca org
  ;; Better HTML export.
  (setq org-html-coding-system 'utf-8-unix)
  
  ;; Better inline code blocks.
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; Enable Markdown export.
  (require 'ox-md))

;; Support org-mode import from a website:
(elpaca org-web-tools)

;; Support my blog as well:
(elpaca org2blog
  (setq org2blog/wp-show-post-in-browser t)
  (setq org2blog/wp-image-upload t)
  ;; Keep the log-in data out of the public eye:
  (load "~/.emacs.d/org2blog-config"))

;; Preview HTML:
(elpaca org-preview-html (add-hook 'org-mode-hook #'org-preview-html-mode))

;; init-org.el ends here
