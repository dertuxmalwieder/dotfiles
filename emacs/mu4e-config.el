(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e")
(use-package mu4e
  :init
  (require 'mu4e)
  (require 'smtpmail)
  (require 'mu4e-speedbar)
  :config
  (setq mu4e-mu-binary (executable-find "mu")
        mu4e-maildir "~/.maildir"
        mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
        mu4e-update-interval 300
        mu4e-index-cleanup t
        mu4e-attachment-dir "~/Downloads"
        mu4e-org-support t
        mu4e-change-filenames-when-moving t
        mu4e-html2text-command (concat (executable-find "textutil") " -stdin -format html -convert txt -stdout")
        sendmail-program (executable-find "msmtp")
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; Integrate BBDB:
  ;; Currently disabled.
  ;;; (setq bbdb-mail-user-agent 'mu4e-user-agent)
  ;;; (setq mu4e-view-rendered-hook 'bbdb-mua-auto-update)
  ;;; (setq mu4e-compose-complete-addresses nil)
  ;;; (setq bbdb-mua-pop-up t)
  ;;; (setq bbdb-mua-pop-up-window-size 5)
  ;;; (setq mu4e-view-show-addresses t)

  ;; Enable notifications:
  (elpaca mu4e-alert
    (mu4e-alert-set-default-style 'notifier)  ;; requires terminal-notifier
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)))
