;;; early-init.el --- Emacs configuration

;;; Commentary:

;; Making Emacs relatively usable.

;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Default to utf-8:
(set-language-environment "UTF-8")

;; Add completion when tab'ing:
(setq tab-always-indent 'complete)

;; Fix native-comp.
;; This basically sets the LIBRARY_PATH to wherever libgccjit
;; is installed. Note that this MIGHT require adjustments on
;; some systems. This path has been written on a M2 Mac.
(when (eq system-type 'darwin)
  (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/13:/opt/homebrew/opt/libgccjit/lib/gcc/13:/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13"))

;; Hide "invalid" commands in M-x:
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Disable package.el initialization:
(setq package-enable-at-startup nil)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; Don't resize the frame just because the
;; font changed:
(setq frame-inhibit-implied-resize t)

;; Smoothness:
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Automatically reload files edited elsewhere:
;; (Commented out for battery reduction.)
;; (setq global-auto-revert-mode t)

(setq gc-cons-threshold 100000000)

;;; early-init.el ends here
