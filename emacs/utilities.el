;; Various functions, written for my own amusement.
(require 'url)

(defun whiskybase-open-url ()
  "Open a Whiskybase URL from a number under the cursor."
  (interactive)
  (let ((has-whiskybase-item nil))
    (if (numberp (number-at-point))
        ;; We'll need to browse to the sentence (that is the Whiskybase number, as a string)
        (progn ()
               (browse-url (concat "https://www.whiskybase.com/whiskies/whisky/" (number-to-string (number-at-point))))
               (setq has-whiskybase-item t))

      ;; No such number.
      ;; Do we have a selection? If so, use it as a Whiskybase query.
      (if mark-active
          (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
            (if (> (length selection) 0)
                (progn ()
                       (browse-url (concat "https://www.whiskybase.com/search?q=" (url-hexify-string selection)))
                       (setq has-whiskybase-item t))))))


    (when (not has-whiskybase-item)
      ;; We haven't found a valid Whiskybase term. Ask for an ID:
      (let ((wb-number (read-number "Enter the WhiskyBase ID: ")))
        (if (numberp wb-number)
            (browse-url (concat "https://www.whiskybase.com/whiskies/whisky/" (number-to-string wb-number)))
          (message (concat "Invalid number: " wb-number ". Aborting.")))))))
