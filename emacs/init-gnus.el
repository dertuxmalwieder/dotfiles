;;; init-gnus.el --- Emacs Gnus configuration

;;; Commentary:

;; Configuring Gnus for Usenet.

;;; Code:

;; Make it faster and nicer looking.
(use-package gnus
  :ensure nil
  :init
  (setq gnus-always-read-dribble-file t)
  (setq gnus-read-active-file t)
  (gnus-add-configuration '(article (vertical 1.0 (summary .35 point) (article 1.0)))))

(use-package gnus-async
  :ensure nil
  :after gnus
  :config
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15))

;; Use eternal-september.
(setq gnus-select-method '(nntp "news.eternal-september.org"))

;; init-gnus.el ends here
