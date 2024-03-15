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

;; Use elpaca instead of Emacs's default package.el for
;; managing installed packages. This might or might not be a
;; good idea.
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq elpaca-queue-limit 12) ;; to avoid "too many open files" errors

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
         (c-ts-mode-hook . eglot-ensure))
  :custom
  (eglot-autoshutdown t))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3rd PARTY PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some platforms (cough) don't update Emacs's path.
;; Make them.
(when (or (eq system-type 'darwin) (daemonp))
  (elpaca exec-path-from-shell
    ;; Hack: I *always* use non-standard shells ... use
    ;; one that exec-path-from-shell actually knows.
    (setq exec-path-from-shell-shell-name "zsh")
    (setq exec-path-from-shell-check-startup-files nil))
  
  (elpaca alert
    ;; Actually working notifications on macOS without dbus
    (define-advice notifications-notify
        (:override (&rest params) using-alert)
      (alert (plist-get params :body)
             :style 'osx-notifier
             :title (plist-get params :title))))

  (elpaca-process-queues)
  (exec-path-from-shell-initialize))

;; E-mail:
(load "~/.emacs.d/mu4e-config.el")

;; Multiple cursors:
(elpaca multiple-cursors
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

;; Emojis:
(elpaca emojify
  (global-emojify-mode))

;; elfeed for RSS:
(load "~/.emacs.d/init-elfeed.el")

;; Better web browsing:
(when (executable-find "w3m")
  (elpaca w3m
    (setq browse-url-browser-function 'w3m-browse-url)))

;; Undo/redo:
(elpaca undo-fu
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Better diff view:
(elpaca diffview
  (add-hook 'diff-mode-hook 'diffview-current))

;; Switch and split windows visually:
(elpaca switch-window
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
(elpaca move-text
  (move-text-default-bindings))

;;;;;;; Messaging ;;;;;;;
;; Mastodon:
(elpaca mastodon
  (setq mastodon-toot--enable-custom-instance-emoji t))

;; Matrix:
(elpaca ement)

;; Telegram:
(elpaca telega
  ;; Note: REQUIRES TDLIB!
  (setq telega-server-libs-prefix "/opt/homebrew/"))

;;;;;;;;;;;;;;;;;;;;;;;;;

;; Web development:
(elpaca web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode)))

;; A less shitty modeline:
(elpaca doom-modeline
  (doom-modeline-mode 1)
  (when (daemonp)
    (setq doom-modeline-icon t))
  (setq doom-modeline-minor-modes t))

;; ... with less minor mode cruft:
(elpaca minions
  (minions-mode 1))

;; Markdown support:
(when (executable-find "multimarkdown")
  (elpaca markdown-mode
    (setq markdown-command "multimarkdown")
    (setq markdown-asymmetric-header t)
    (setq markdown-header-scaling t)
    (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

  ;; Writing mode, depends on Markdown-Mode:
  (elpaca olivetti
    (add-hook 'markdown-mode-hook 'olivetti-mode)))

;; Paste online:
(elpaca dpaste
  (setq dpaste-poster "tux0r")
  ;; Paste with C-c p:
  (global-set-key (kbd "C-c p") 'dpaste-region-or-buffer))

;; Spell checking:
(elpaca guess-language
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

;; Project-related functionalities:
(elpaca projectile
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Enable some icons throughout Emacs:
(elpaca all-the-icons)

(elpaca all-the-icons-dired
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(elpaca all-the-icons-gnus
  (all-the-icons-gnus-setup))

;; Better regexp search&replace:
(elpaca visual-regexp)
(elpaca visual-regexp-steroids
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  (define-key global-map (kbd "C-c m") 'vr/mc-mark))

;; Expand selections:
(elpaca expand-region
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Lisp programming:
;; Use SLIME as a CL subsystem.
(elpaca slime
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (require 'slime)
              (slime-setup '(slime-fancy))
              (eval-after-load "auto-complete"
                '(add-to-list 'ac-modes 'slime-repl-mode))
              (eval-after-load "auto-complete"
                '(add-to-list 'ac-modes 'slime-repl-mode))
              (if (executable-find "ros")
                  (setq inferior-lisp-program "ros -Q run")
                (setq inferior-lisp-program (executable-find "sbcl"))))))

;; Go programming:
;; Install and set up the Go mode.
(elpaca go-mode
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (setq gofmt-command (concat (getenv "GOPATH") "/bin/goimports")))

;; COBOL programming:
(elpaca cobol-mode
  (setq auto-mode-alist
        (append
         '(("\\.cob\\'" . cobol-mode)
           ("\\.cbl\\'" . cobol-mode)
           ("\\.cpy\\'" . cobol-mode))
         auto-mode-alist)))

;; Rust programming:
(elpaca rustic
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  (remove-hook 'rustic-mode-hook 'flycheck-mode))

;; Corfu auto-completion for code:
(elpaca corfu
  (setq corfu-auto t)
  (setq corfu-separator ?\s)
  (add-hook 'prog-mode-hook 'corfu-mode))

;; Gopher:
(elpaca elpher)

;; Vertico for most interactive stuff:
(elpaca vertico
  (setq vertico-cycle t)
  (vertico-mode))

;; Icons for the minibuffer:
(elpaca all-the-icons-completion)

;; Minibuffer improvements:
(elpaca marginalia
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))                               
              (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
              (marginalia-mode))))

;; Orderless search:
(elpaca orderless)

;; Completion via consult:
(elpaca consult
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
(elpaca smartparens
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))

;; Clickable links everywhere:
(elpaca orglink
  (global-orglink-mode))

;; EAT instead of Emacs's terminal:
(elpaca eat
  ;; Integrate with eshell:
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(elpaca vc-fossil
  (add-to-list 'vc-handled-backends 'Fossil t))

;; Tree-sitter configuration:
(unless (version< emacs-version "29.1")
  (load "~/.emacs.d/init-treesitter"))

;; GhostText support:
(elpaca atomic-chrome
  (atomic-chrome-start-server))

;; Use ligatures if possible (requires the Fira Code Symbol font)
;; for programming:
(elpaca fira-code-mode
  (fira-code-mode-set-font)
  (add-hook 'prog-mode-hook 'fira-code-mode))

;; Nicer theme:
(elpaca nofrils-acme-theme
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
