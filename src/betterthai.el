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
  (message "BetterThai IME %s" (if betterthai-ime-mode "ON" "OFF")))

(defun betterthai-ime-process-filter (_proc output)
  "Handle output from the BetterThai IME process asynchronously and show in side panel."
  (let ((buffer (get-buffer-create betterthai-ime-output-buffer-name)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert output)
      (when (get-buffer-window buffer)
        (with-selected-window (get-buffer-window buffer)
          (goto-char (point-max)))))
    (unless (get-buffer-window betterthai-ime-output-buffer-name)
      (display-buffer-in-side-window
       (get-buffer betterthai-ime-output-buffer-name)
       '((side . right) (slot . 0) (window-width . 0.33))))))

(defun betterthai-start-ime-process ()
  "Start the persistent BetterThai IME process."
  (unless (and betterthai-ime-process
               (process-live-p betterthai-ime-process))
    (let ((ime-dir (expand-file-name "~/.emacs.d/betterthai-ime/"))
          (exe (expand-file-name "betterthai-ime" "~/.emacs.d/betterthai-ime/")))
      (message "Starting BetterThai IME at: %s" exe)
      (setq betterthai-ime-process
            (make-process
             :name "betterthai-ime"
             :buffer "*betterthai-ime*"
             :stderr "*better-error*"
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
  (when (process-live-p betterthai-ime-process)
    (process-send-string betterthai-ime-process
                         (concat betterthai-ime-char-buffer "\n"))))

(defun betterthai-insert-char ()
  "Insert character into IME buffer or self-insert."
  (interactive)
  (let ((char (string last-command-event)))
    (if betterthai-ime-mode
        (progn
          (setq betterthai-ime-char-buffer
                (concat betterthai-ime-char-buffer char))
          (message "Buffer: %s" betterthai-ime-char-buffer))
      (self-insert-command 1))))

(defun betterthai-space-key ()
  "Handle space key press in IME mode."
  (interactive)
  (if betterthai-ime-mode
      (progn
        ;; Clear the output buffer before sending new input
        (when (get-buffer betterthai-ime-output-buffer-name)
          (with-current-buffer betterthai-ime-output-buffer-name
            (erase-buffer)))
        (betterthai-start-ime-process)
        (betterthai-send-buffer)
        (setq betterthai-ime-char-buffer "")
        (message "IME triggered, buffer cleared."))
    (self-insert-command 1)))


(defun betterthai-backspace-key ()
  "Handle backspace key in IME mode."
  (interactive)
  (if betterthai-ime-mode
      (progn
        (when (> (length betterthai-ime-char-buffer) 0)
          (setq betterthai-ime-char-buffer
                (substring betterthai-ime-char-buffer 0 -1)))
        (message "Buffer: %s" betterthai-ime-char-buffer))
    (backward-delete-char-untabify 1)))

(defvar betterthai-ime-map
  (let ((map (make-sparse-keymap)))
    ;; Printable ASCII chars (except space)
    (dolist (i (number-sequence ?\s ?~))
      (let ((char (char-to-string i)))
        (unless (string= char " ")
          (define-key map char #'betterthai-insert-char))))
    ;; Special keys
    (define-key map (kbd "SPC") #'betterthai-space-key)
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
