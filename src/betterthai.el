(defvar betterthai-ime-mode nil
  "Non-nil if BetterThai IME mode is enabled.")

(defun betterthai-toggle-ime ()
  "Toggle BetterThai IME mode."
  (interactive)
  (setq betterthai-ime-mode (not betterthai-ime-mode))
  (message "BetterThai IME %s" (if betterthai-ime-mode "ON" "OFF")))

(defvar betterthai-ime-map
  (let ((map (make-sparse-keymap)))
    ;; Bind printable ASCII characters (space to tilde)
    (dolist (i (number-sequence ?\s ?~))
      (define-key map (char-to-string i)
        (lambda ()
          (interactive)
          (if betterthai-ime-mode
              (message "Character pressed")
            (self-insert-command 1)))))
    map)
  "Keymap for BetterThai IME minor mode.")

(define-minor-mode betterthai-ime-minor-mode
  "Minor mode for BetterThai IME."
  :lighter " 🅑"
  :global t
  :keymap betterthai-ime-map)

(defun betterthai ()
  "Initialize BetterThai IME system."
  (interactive)
  ;; Bind F1 to toggle the IME mode
  (global-set-key (kbd "<f1>") #'betterthai-toggle-ime)

  ;; Enable the minor mode
  (betterthai-ime-minor-mode 1)

  (message "BetterThai IME loaded. Press F1 to toggle."))
