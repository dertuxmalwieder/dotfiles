;;; init-gnus.el --- Emacs Gnus configuration

;;; Commentary:

;; Configuring Gnus for Usenet.

;;; Code:

;; Make it faster and nicer looking.
(use-package gnus
  :config
  (setq gnus-always-read-dribble-file t)
  (setq gnus-read-active-file t)
  (gnus-add-configuration '(article (vertical 1.0 (summary .35 point) (article 1.0)))))

(use-package gnus-async
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

;; Use eternal-september.
(setq gnus-select-method '(nntp "news.eternal-september.org"
                                (nntp-open-connection-function
                                 nntp-open-tls-stream)
                                (nntp-port-number 563)
                                (nntp-address
                                 "news.eternal-september.org")))

;; init-gnus.el ends here
