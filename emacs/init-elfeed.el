;;; init-elfeed.el --- Elfeed configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuring elfeed with Newsblur.

;;; Code:

(elpaca elfeed
  (setq elfeed-use-curl t))

;; Show a dashboard:
(elpaca elfeed-summary)

;; Enable Newsblur:
(elpaca elfeed-protocol
  (setq elfeed-protocol-newsblur-maxpages 20)
  (setq elfeed-protocol-enabled-protocols '(newsblur))
  (elfeed-protocol-enable)
  (setq elfeed-curl-extra-arguments '("--cookie-jar" "/tmp/newsblur-cookie"
                                      "--cookie" "/tmp/newsblur-cookie")))

;; Rearrange the window:
(elpaca elfeed-goodies
  (elfeed-goodies/setup))

;; Better YouTube feeds:
(elpaca elfeed-tube)
;; init-elfeed.el ends here
