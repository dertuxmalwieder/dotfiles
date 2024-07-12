;;; init-erc.el --- Emacs ERC configuration

;;; Commentary:

;; Loading and configuring ERC for IRC.

;;; Code:

(use-package erc
  :custom
  ;; (add-to-list 'desktop-modes-not-to-save 'erc-mode)

  ;; Config:
  (erc-track-exclude-types '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "315" "324" "329" "332" "333" "353" "366" "477"))
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)
  (erc-fill-column 120)
  (erc-interpret-mirc-color t)
  (erc-track-shorten-function nil)
  (erc-hide-timestamps t)
  (erc-prompt-for-nickserv-password nil)

  :config
  ;; (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'nickserv)
  (add-to-list 'erc-modules 'spelling)

  (erc-timestamp-mode nil)
  (erc-services-mode 1)
  (erc-spelling-mode 1)
  (erc-notifications-mode 1)
  (erc-update-modules))

;; init-erc.el ends here
