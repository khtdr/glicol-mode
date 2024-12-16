;;; glicol-cli.el --- CLI integration for Glicol mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides CLI integration features for glicol-mode

;;; Code:

(require 'term)

(defcustom glicol-cli-command "glicol-cli"
  "Command to run the Glicol CLI TUI.
This should be the path to the glicol-cli executable.
The default assumes it's available in your PATH as 'glicol-cli'."
  :type 'string
  :group 'glicol)

(defcustom glicol-bpm 120
  "Current BPM (beats per minute) for Glicol playback."
  :type 'integer
  :group 'glicol)

(defvar glicol-cli-process nil
  "Process handle for the running Glicol CLI instance.")

(defvar glicol-cli-buffer-name "*Glicol CLI*"
  "Name of the buffer where the Glicol CLI runs.")

(defun glicol-set-bpm (bpm)
  "Set Glicol BPM to BPM and restart the server if it's running."
  (interactive "nBPM: ")
  (setq glicol-bpm bpm)
  (when (and glicol-cli-process
             (process-live-p glicol-cli-process))
    (glicol-restart-cli))
  (message "Glicol BPM set to %d" bpm))

(defun glicol-setup-cli-window ()
  "Configure the CLI window display."
  (let ((cli-window (get-buffer-window glicol-cli-buffer-name)))
    (when cli-window
      (select-window cli-window)
      (set-window-dedicated-p cli-window t)
      (fit-window-to-buffer cli-window 3))))

(defun glicol-start-cli ()
  "Start the Glicol CLI in headless mode."
  (interactive)
  (if glicol-cli-process
      (message "Glicol CLI is already running!")
    (unless buffer-file-name
      (error "Buffer is not visiting a file"))
    (let* ((glicol-file buffer-file-name))
      (setq glicol-cli-process
            (get-buffer-process
             (term-ansi-make-term glicol-cli-buffer-name
                                  glicol-cli-command
                                  nil
                                  "--headless"
                                  "--bpm"
                                  (number-to-string glicol-bpm)
                                  glicol-file)))
      ;; Configure the terminal window
      (display-buffer glicol-cli-buffer-name
                      '((display-buffer-at-bottom)
                        (window-height . 3)
                        (dedicated . t)
                        (inhibit-switch-frame . t)))
      (glicol-setup-cli-window)
      ;; Return focus to the original window
      ;; (select-window (get-buffer-window (current-buffer)))
      (when (featurep 'doom)
        (glicol-doom-modeline-status-update 'running))
      (message "Started Glicol CLI in headless mode"))))

(defun glicol-server-status ()
  "Check if the Glicol server is running."
  (interactive)
  (if (and glicol-cli-process
           (process-live-p glicol-cli-process))
      (message "Glicol server is running")
    (message "Glicol server is not running")))

(defun glicol-restart-cli ()
  "Restart the Glicol CLI server."
  (interactive)
  (glicol-stop-cli)
  ;; Give it a moment to fully stop
  (sleep-for 0.2)
  (glicol-start-cli))

(defun glicol-stop-cli ()
  "Stop the running Glicol CLI instance and close its buffer."
  (interactive)
  (when-let ((buffer (get-buffer glicol-cli-buffer-name)))
    (when (buffer-live-p buffer)
      (let ((proc (get-buffer-process buffer)))
        (when (process-live-p proc)
          ;; Kill the process first
          (set-process-query-on-exit-flag proc nil)
          (delete-process proc)))
      (kill-buffer buffer)
      (setq glicol-cli-process nil)
      (when (featurep 'doom)
        (glicol-doom-modeline-status-update 'stopped))
      (message "Glicol CLI stopped")))
  (unless glicol-cli-process
    (message "No running Glicol CLI instance found")))

(provide 'glicol-cli)
;;; glicol-cli.el ends here
