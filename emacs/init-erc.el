;;; init-erc.el --- Emacs ERC configuration

;;; Commentary:

;; Loading and configuring ERC for IRC.

;;; Code:

(use-package erc
  :config
  ;; (add-to-list 'desktop-modes-not-to-save 'erc-mode)

  ;; Config:
  (setq erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))
  (setq erc-fill-function 'erc-fill-static)
  (setq erc-fill-static-center 20)
  (setq erc-fill-column 120)
  (setq erc-interpret-mirc-color t)
  (setq erc-track-shorten-function nil)
  (setq erc-hide-timestamps t)
  (setq erc-prompt-for-nickserv-password nil)

  ;; (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'nickserv)
  (add-to-list 'erc-modules 'spelling)

  (erc-timestamp-mode nil)
  (erc-services-mode 1)
  (erc-spelling-mode 1)
  (erc-notifications-mode 1)
  (erc-update-modules))

;; init-erc.el ends here
