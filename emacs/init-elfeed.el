;;; init-elfeed.el --- Elfeed configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuring elfeed with Newsblur.

;;; Code:

(use-package elfeed
  :config
  (setq elfeed-use-curl t))

;; Show a dashboard:
(use-package elfeed-summary
  :after elfeed)

;; Enable Newsblur:
(use-package elfeed-protocol
  :after elfeed
  :config
  (setq elfeed-protocol-newsblur-maxpages 20)
  (setq elfeed-protocol-enabled-protocols '(newsblur))
  (setq elfeed-protocol-newsblur-sub-category-separator "/")
  (elfeed-protocol-enable)
  (setq elfeed-curl-extra-arguments '("--cookie-jar" "/tmp/newsblur-cookie"
                                      "--cookie" "/tmp/newsblur-cookie")))

;; Rearrange the window:
(use-package elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup))

;; Better YouTube feeds:
(use-package elfeed-tube
  :after elfeed)

;; init-elfeed.el ends here
