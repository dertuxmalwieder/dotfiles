;;; init-treesitter.el --- Emacs tree-sitter configuration

;;; Commentary:

;; Loading and configuring tree-sitters for coding.

;;; Code:

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; init-treesitter.el ends here
