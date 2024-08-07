;; This REQUIRES mu to have been installed via Homebrew.
;; If it is installed elsewhere, the load-path will need to be adjusted.
(use-package mu4e
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"
  :commands (mu4e mu4e-update-index)
  :config
  (require 'smtpmail)
  (require 'mu4e-speedbar)
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote mu4e-main-buffer-name)
                 display-buffer-same-window))
  
  (setq mu4e-maildir "~/.maildir"
        mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
        mu4e-update-interval 300
        mu4e-index-cleanup t
        mu4e-attachment-dir "~/Downloads"
        mu4e-org-support t
        mu4e-use-fancy-chars t
        mu4e-confirm-quit nil
        mu4e-change-filenames-when-moving t
        mu4e-html2text-command (concat (executable-find "textutil") " -stdin -format html -convert txt -stdout")
        sendmail-program (executable-find "msmtp")
        send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  (defun +mu4e-view-settings ()
    "Settings for mu4e-view-mode."
    (visual-line-mode)
    (when (functionp olivetti-mode)
      (olivetti-mode))
    (variable-pitch-mode))
  (add-hook 'mu4e-view-mode-hook #'+mu4e-view-settings))

;; Integrate BBDB:
;; Currently disabled.
  ;;; (setq bbdb-mail-user-agent 'mu4e-user-agent)
  ;;; (setq mu4e-view-rendered-hook 'bbdb-mua-auto-update)
  ;;; (setq mu4e-compose-complete-addresses nil)
  ;;; (setq bbdb-mua-pop-up t)
  ;;; (setq bbdb-mua-pop-up-window-size 5)
  ;;; (setq mu4e-view-show-addresses t)

;; Enable notifications:
;; (use-package mu4e-alert
;;   :after mu4e
;;   :config
;;   (mu4e-alert-set-default-style 'notifier)  ;; requires terminal-notifier
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
