;;; glicol-doom.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jose Mazzarelli
;;
;; Author: Jose Mazzarelli <mazzarelli@gmail.com>
;; Maintainer: Jose Mazzarelli <mazzarelli@gmail.com>
;; Created: December 14, 2024
;; Modified: December 14, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/khtdr/glicol-doom
;; Package-Requires: ((emacs "24.3") (nerd-icons "0.0.1") (doom-modeline "3.0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'nerd-icons)
(require 'doom-modeline)

(defvar glicol-doom-modeline-icon-running
  (nerd-icons-mdicon "nf-md-stop"))

(defvar glicol-doom-modeline-music-note
  (nerd-icons-mdicon "nf-md-music_note"))

(defvar glicol-doom-modeline-icon-stopped
  (nerd-icons-mdicon "nf-md-play"))

(defvar glicol-doom-modeline-status 'stopped
  "Current status of Glicol server: 'running or 'stopped.")

(defun glicol-doom-modeline-status-update (status)
  "Update the Glicol modeline STATUS indicator."
  (setq glicol-doom-modeline-status status)
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
                                  map))
         " "
         (propertize glicol-doom-modeline-music-note)))
       ('stopped
        (propertize glicol-doom-modeline-icon-stopped
                    'help-echo "Glicol is stopped - click to play"
                    'mouse-face 'mode-line-highlight
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line mouse-1]
                                             #'glicol-doom-modeline-click-handler)
                                 map)))))))

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
         :desc "Play Glicol" "p" #'glicol-start-cli
         :desc "Stop Glicol" "s" #'glicol-stop-cli
         :desc "Restart Glicol" "r" #'glicol-restart-cli
         :desc "Check Glicol status" "c" #'glicol-server-status
         :desc "Set BPM" "b" #'glicol-set-bpm)))


;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'glicol-doom-setup)
(add-hook 'dired-mode-hook #'glicol-doom-setup)
(add-hook 'after-change-major-mode-hook #'glicol-doom-setup)

(provide 'glicol-doom)
;;; glicol-doom.el ends here
