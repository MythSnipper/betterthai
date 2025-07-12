(defvar betterthai-ime-mode nil
  "Non-nil if BetterThai IME mode is enabled.")

(defvar betterthai-ime-char-buffer ""
  "Buffer that accumulates characters typed in IME mode.")

(defvar betterthai-ime-process nil
  "Persistent background process for BetterThai IME.")

(defvar betterthai-ime-output-file "~/betterthai-output.txt"
  "Path to the output file to write IME results.")

(defvar betterthai-ime-output-buffer ""
  "Temporary buffer accumulating output from the IME process.")

(defvar betterthai-ime-flush-timer nil
  "Timer to periodically flush IME output buffer to file.")

(defun betterthai-toggle-ime ()
  "Toggle BetterThai IME mode."
  (interactive)
  (setq betterthai-ime-mode (not betterthai-ime-mode))
  (setq betterthai-ime-char-buffer "")
  (message "BetterThai IME %s" (if betterthai-ime-mode "ON" "OFF")))

(defun betterthai-ime-process-filter (_proc output)
  "Handle output from the BetterThai IME process asynchronously."
  ;; Append output to the in-memory buffer
  (setq betterthai-ime-output-buffer (concat betterthai-ime-output-buffer output)))

(defun betterthai-ime-flush-output-buffer ()
  "Write the accumulated IME output buffer to file and clear it."
  (when (> (length betterthai-ime-output-buffer) 0)
    (with-temp-buffer
      (insert betterthai-ime-output-buffer)
      (write-region (point-min) (point-max) betterthai-ime-output-file t))
    (setq betterthai-ime-output-buffer "")))

(defun betterthai-ime-start-flush-timer ()
  "Start a timer to periodically flush IME output buffer."
  (unless betterthai-ime-flush-timer
    (setq betterthai-ime-flush-timer
          (run-with-timer 1 1 #'betterthai-ime-flush-output-buffer))))

(defun betterthai-ime-stop-flush-timer ()
  "Stop the IME output flush timer."
  (when betterthai-ime-flush-timer
    (cancel-timer betterthai-ime-flush-timer)
    (setq betterthai-ime-flush-timer nil)))

(defun betterthai-start-ime-process ()
  "Start the persistent BetterThai IME process."
  (unless (and betterthai-ime-process
               (process-live-p betterthai-ime-process))
    (setq betterthai-ime-output-buffer "")
    (setq betterthai-ime-process
          (make-process
           :name "betterthai-ime"
           :buffer "*betterthai-ime*"
           :command '("~/.emacs.d/betterthai-ime/betterthai-ime")
           :noquery t
           :filter #'betterthai-ime-process-filter
           :sentinel (lambda (_proc event)
                       (message "BetterThai IME exited: %s" event)
                       (betterthai-ime-stop-flush-timer))))
    (betterthai-ime-start-flush-timer)))

(defun betterthai-send-buffer ()
  "Send the character buffer to the IME program."
  (when (process-live-p betterthai-ime-process)
    (process-send-string betterthai-ime-process
                         (concat betterthai-ime-char-buffer "\n"))))

(defvar betterthai-ime-map
  (let ((map (make-sparse-keymap)))
    ;; Printable ASCII chars (except space)
    (dolist (i (number-sequence ?\s ?~))
      (let ((char (char-to-string i)))
        (unless (string= char " ")
          (define-key map char
            (lambda ()
              (interactive)
              (if betterthai-ime-mode
                  (progn
                    (setq betterthai-ime-char-buffer
                          (concat betterthai-ime-char-buffer (string last-command-event)))
                    (message "Buffer: %s" betterthai-ime-char-buffer))
                (self-insert-command 1))))))
    ;; Space triggers IME
    (define-key map (kbd "SPC")
      (lambda ()
        (interactive)
        (if betterthai-ime-mode
            (progn
              (betterthai-start-ime-process)
              (betterthai-send-buffer)
              (setq betterthai-ime-char-buffer "")
              (message "IME triggered, buffer cleared."))
          (self-insert-command 1))))
    ;; Backspace removes a character
    (define-key map (kbd "DEL")
      (lambda ()
        (interactive)
        (if betterthai-ime-mode
            (progn
              (when (> (length betterthai-ime-char-buffer) 0)
                (setq betterthai-ime-char-buffer
                      (substring betterthai-ime-char-buffer 0 -1)))
              (message "Buffer: %s" betterthai-ime-char-buffer))
          (backward-delete-char-untabify 1))))
    map)
  "Keymap for BetterThai IME minor mode.")

(define-minor-mode betterthai-ime-minor-mode
  "Minor mode for BetterThai IME."
  :lighter " 🅑"
  :global t
  :keymap betterthai-ime-map))

(defun betterthai ()
  "Initialize BetterThai IME system."
  (interactive)
  ;; F1 toggles IME mode
  (global-set-key (kbd "<f1>") #'betterthai-toggle-ime)
  ;; Start minor mode and process
  (betterthai-ime-minor-mode 1)
  (betterthai-start-ime-process)
  (message "BetterThai IME loaded. Press F1 to toggle."))


(defun test ()
  "Start and test the IME process in isolation."
  (interactive)
  (let ((proc
         (make-process
          :name "test-ime"
          :buffer "*test-ime*"
          :command '("~/.emacs.d/betterthai-ime/betterthai-ime") ;; ← Adjust path
          :noquery t
          :filter (lambda (_proc output)
                    (message "IME says: %s" output)))))
    (process-send-string proc "hello\n")))
