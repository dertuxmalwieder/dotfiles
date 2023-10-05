;; Various functions, written for my own amusement.

(defun whiskybase-open-url ()
  "Open a Whiskybase URL from a number under the cursor."
  (interactive)
  (if (numberp (number-at-point))
      ;; We'll need to browse to the sentence (that is the Whiskybase number, as a string)
      (browse-url (concat "https://www.whiskybase.com/whiskies/whisky/" (sentence-at-point)))

    ;; No such number.
    (let ((wb-number (read-number "Enter the WhiskyBase ID: ")))
      (if (numberp wb-number)
          (browse-url (concat "https://www.whiskybase.com/whiskies/whisky/" (number-to-string wb-number)))
        (message (concat "Invalid number: " wb-number ". Aborting."))))))
