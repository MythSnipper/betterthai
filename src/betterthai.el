(defvar betterthai-ime-mode nil
  "Non-nil if BetterThai IME mode is enabled.")

(defvar betterthai-ime-char-buffer ""
  "Buffer that accumulates characters typed in IME mode.")

(defvar betterthai-ime-process nil
  "Persistent background process for BetterThai IME.")

(defvar betterthai-ime-output-buffer-name "*BetterThai Output*"
  "Name of the buffer to display BetterThai IME output.")

(defun betterthai-toggle-ime ()
  "Toggle BetterThai IME mode."
  (interactive)
  (setq betterthai-ime-mode (not betterthai-ime-mode))
  (setq betterthai-ime-char-buffer "")
  (message "BetterThai IME mode is now: %s" (if betterthai-ime-mode "ENABLED" "DISABLED")))

(defun betterthai-ime-process-filter (_proc output)
  "Handle output from the BetterThai IME process asynchronously and show in side panel."
  (let ((buffer (get-buffer-create betterthai-ime-output-buffer-name)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert output))
    (unless (get-buffer-window buffer)
      (display-buffer buffer '((display-buffer-in-side-window)
                               (side . right)
                               (window-width . 0.3))))))

(defun betterthai-start-ime-process ()
  "Start the persistent BetterThai IME process if not already running."
  (unless (and betterthai-ime-process (process-live-p betterthai-ime-process))
    (let ((ime-dir (expand-file-name "~/.emacs.d/betterthai-ime/"))
          (exe (expand-file-name "betterthai-ime" "~/.emacs.d/betterthai-ime/")))
      (unless (file-executable-p exe)
        (error "BetterThai IME executable not found or not executable: %s" exe))
      (message "Starting BetterThai IME at: %s" exe)
      (setq betterthai-ime-process
            (make-process
             :name "betterthai-ime"
             :buffer "*betterthai-ime*"
             :stderr "*betterthai-error*"
             :command (list exe)
             :noquery t
             :filter #'betterthai-ime-process-filter
             :sentinel (lambda (_proc event)
                         (message "BetterThai IME exited: %s" event))
             :coding 'utf-8
             :file-handler t
             :working-directory ime-dir)))))

(defun betterthai-send-buffer ()
  "Send the character buffer to the IME program."
  (unless (and betterthai-ime-process (process-live-p betterthai-ime-process))
    (message "IME process not running; restarting...")
    (betterthai-start-ime-process))
  (when (process-live-p betterthai-ime-process)
    (process-send-string betterthai-ime-process
                         (concat betterthai-ime-char-buffer "\n"))))

(defun betterthai-insert-char ()
  "Insert character into IME buffer and trigger backend."
  (interactive)
  (let ((char (string last-command-event)))
    (if betterthai-ime-mode
        (progn
          (setq betterthai-ime-char-buffer
                (concat betterthai-ime-char-buffer char))
          ;; Clear the output buffer
          (let ((buffer (get-buffer betterthai-ime-output-buffer-name)))
            (when buffer
              (with-current-buffer buffer
                (erase-buffer))))
          ;; Trigger backend
          (betterthai-start-ime-process)
          (betterthai-send-buffer)
          (message "Buffer: %s" betterthai-ime-char-buffer))
      (self-insert-command 1))))



(defun betterthai-backspace-key ()
  "Handle backspace key in IME mode.
Update IME output after modifying the buffer."
  (interactive)
  (if betterthai-ime-mode
      (progn
        (when (> (length betterthai-ime-char-buffer) 0)
          (setq betterthai-ime-char-buffer
                (substring betterthai-ime-char-buffer 0 -1)))
        (message "Buffer: %s" betterthai-ime-char-buffer)
        ;; Clear the output buffer
        (let ((buffer (get-buffer betterthai-ime-output-buffer-name)))
          (when buffer
            (with-current-buffer buffer
              (erase-buffer))))
        ;; Restart the process if needed and send the new buffer
        (betterthai-start-ime-process)
        (betterthai-send-buffer))
    (backward-delete-char-untabify 1)))

(defun betterthai-handle-number-key ()
  "Send the number key directly to the IME process, bypassing the buffer."
  (interactive)
  (let ((char (string last-command-event)))
    (when betterthai-ime-mode
      (setq betterthai-ime-char-buffer "") ;; Clear character buffer
      ;; Clear the output buffer
      (let ((buffer (get-buffer betterthai-ime-output-buffer-name)))
        (when buffer
          (with-current-buffer buffer
            (erase-buffer))))
      ;; Restart the IME process if needed and send the number key
      (betterthai-start-ime-process)
      (process-send-string betterthai-ime-process (concat char "\n")))))


(defvar betterthai-ime-map
  (let ((map (make-sparse-keymap)))
    ;; Bind number keys to number handler
    (dolist (i (number-sequence ?0 ?9))
      (define-key map (char-to-string i) #'betterthai-handle-number-key))

    ;; Bind all printable characters including space (now part of insert-char)
    (dolist (i (number-sequence ?\s ?~)) ; space to ~
      (let ((char (char-to-string i)))
        (unless (string-match-p "[0-9]" char) ; skip numbers
          (define-key map char #'betterthai-insert-char))))

    ;; Backspace
    (define-key map (kbd "DEL") #'betterthai-backspace-key)
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
  (global-set-key (kbd "<f1>") #'betterthai-toggle-ime)
  (betterthai-ime-minor-mode 1)
  (message "BetterThai IME loaded. Press F1 to toggle."))

(defun test ()
  "Start and test the IME process in isolation."
  (interactive)
  (let ((proc
         (make-process
          :name "test-ime"
          :buffer "*test-ime*"
          :command '("~/.emacs.d/betterthai-ime/betterthai-ime")
          :noquery t
          :filter (lambda (_proc output)
                    (let ((buffer (get-buffer-create "*test-ime*")))
                      (with-current-buffer buffer
                        (goto-char (point-max))
                        (insert output))
                      (display-buffer buffer))))))
    (process-send-string proc "hello\n")))
