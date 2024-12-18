;;; glicol-doom.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Provides Doom integration for glicol-mode
;;
;;; Code:

(require 'nerd-icons)
(require 'doom-modeline)

(defvar glicol-bpm)

(defvar glicol-doom-modeline-icon-running
  (nerd-icons-mdicon "nf-md-stop"))

(defvar glicol-doom-modeline-music-note
  (nerd-icons-mdicon "nf-md-music_note"))

(defvar glicol-doom-modeline-icon-stopped
  (nerd-icons-mdicon "nf-md-play"))

(defvar glicol-doom-modeline-status 'stopped
  "Current status of Glicol server: `running or `stopped.")

(defvar glicol-doom-modeline-note-visible nil
  "Whether the music note is currently visible in the modeline.")

(defvar glicol-doom-modeline-timer nil
  "Timer for blinking the music note.")

(defun glicol-doom-modeline-toggle-note ()
  "Toggle the visibility of the music note."
  (setq glicol-doom-modeline-note-visible (not glicol-doom-modeline-note-visible))
  (force-mode-line-update t))

(defun glicol-doom-modeline-start-animation ()
  "Start the music note blinking animation based on current BPM."
  (when (not glicol-doom-modeline-timer)
    (let ((interval (/ 60.0 glicol-bpm)))  ; Convert BPM to seconds
      (setq glicol-doom-modeline-timer
            (run-with-timer 0 interval #'glicol-doom-modeline-toggle-note)))))

(defun glicol-doom-modeline-stop-animation ()
  "Stop the music note blinking animation."
  (when glicol-doom-modeline-timer
    (cancel-timer glicol-doom-modeline-timer)
    (setq glicol-doom-modeline-timer nil
          glicol-doom-modeline-note-visible t))
  (force-mode-line-update t))

(defun glicol-doom-modeline-status-update (status)
  "Update the Glicol modeline STATUS indicator."
  (setq glicol-doom-modeline-status status)
  (if (eq status 'running)
      (glicol-doom-modeline-start-animation)
    (glicol-doom-modeline-stop-animation))
  (force-mode-line-update t))

(defun glicol-doom-modeline-click-handler (event)
  "Handle click EVENT on the Glicol modeline icon."
  (interactive "e")
  (if (eq glicol-doom-modeline-status 'running)
      (glicol-stop-cli)
    (glicol-start-cli)))

(doom-modeline-def-segment glicol
  "Glicol server status indicator."
  (when (derived-mode-p 'glicol-mode)
    (concat
     " "
     (pcase glicol-doom-modeline-status
       ('running
        (concat
         (propertize glicol-doom-modeline-icon-running
                     'help-echo "Glicol is playing - click to stop"
                     'mouse-face 'mode-line-highlight
                     'local-map (let ((map (make-sparse-keymap)))
                                  (define-key map [mode-line mouse-1]
                                              #'glicol-doom-modeline-click-handler)
                                  map))))
       ('stopped
        (propertize glicol-doom-modeline-icon-stopped
                    'help-echo "Glicol is stopped - click to play"
                    'mouse-face 'mode-line-highlight
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line mouse-1]
                                             #'glicol-doom-modeline-click-handler)
                                 map))))
     " " (number-to-string glicol-bpm) " BPM "
     (pcase glicol-doom-modeline-status
       ('running
        (when glicol-doom-modeline-note-visible
          (propertize glicol-doom-modeline-music-note)))))))


(defun glicol-doom-setup ()
  "Setup Doom integration for Glicol major mode."

  ;; Add modeline segment
  (doom-modeline-def-modeline 'glicol-doom-modeline
    '(bar workspace-name window-number modals matches buffer-info remote-host glicol)
    '(misc-info minor-modes input-method buffer-position process major-mode))
  
  ;; Set the modeline
  (when (derived-mode-p 'glicol-mode)
    (doom-modeline-set-modeline 'glicol-doom-modeline 'default))
  
  ;; Setup keybindings
  (map! :leader
        (:prefix ("G" . "Glicol")
         :desc "Play Glicol" "g" #'glicol-start-cli
         :desc "Stop Glicol" "x" #'glicol-stop-cli
         :desc "Restart Glicol" "t" #'glicol-restart-cli
         :desc "Check Glicol status" "s" #'glicol-server-status
         :desc "Set BPM" "b" #'glicol-set-bpm)))


;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'glicol-doom-setup)
(add-hook 'dired-mode-hook #'glicol-doom-setup)
(add-hook 'after-change-major-mode-hook #'glicol-doom-setup)

(provide 'glicol-doom)
;;; glicol-doom.el ends here
