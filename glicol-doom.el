;;; glicol-doom.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jose Mazzarelli
;;
;; Author: Jose Mazzarelli <mazzarelli@gmail.com>
;; Maintainer: Jose Mazzarelli <mazzarelli@gmail.com>
;; Created: December 14, 2024
;; Modified: December 14, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/joey/glicol-doom
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

(defvar glicol-modeline-icon-running
  (nerd-icons-mdicon "nf-md-stop"))

(defvar glicol-modeline-icon-stopped
  (nerd-icons-mdicon "nf-md-play"))

(defvar glicol-modeline-status 'stopped
  "Current status of Glicol server: 'running or 'stopped.")

(defun glicol-modeline-status-update (status)
  "Update the Glicol modeline status indicator."
  (setq glicol-modeline-status status)
  (force-mode-line-update t))

(doom-modeline-def-segment glicol
  "Glicol server status indicator."
  (when (derived-mode-p 'glicol-mode)
    (concat
     " "
     (pcase glicol-modeline-status
       ('running
        (propertize glicol-modeline-icon-running
                    'help-echo "Glicol server running - click to stop"))
       ('stopped
        (propertize glicol-modeline-icon-stopped
                    'help-echo "Glicol server stopped - click to start"))))))

(defun glicol-doom-setup-keys ()
  "Setup Glicol keybindings for Glicol major mode"
  ;; Add modeline segment
  (doom-modeline-def-modeline 'glicol-modeline
    '(bar workspace-name window-number modals matches buffer-info remote-host glicol)
    '(misc-info minor-modes input-method buffer-position process major-mode))
  
  ;; Set the modeline
  (when (derived-mode-p 'glicol-mode)
    (doom-modeline-set-modeline 'glicol-modeline 'default))
  
  ;; Setup keybindings
  (map! :leader
        (:prefix ("G" . "Glicol")
         :desc "Start Glicol CLI" "s" #'glicol-start-cli
         :desc "Stop Glicol CLI" "q" #'glicol-stop-cli
         :desc "Restart Glicol CLI" "r" #'glicol-restart-cli
         :desc "Check server status" "c" #'glicol-server-status)))


;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'glicol-doom-setup-keys)
(add-hook 'dired-mode-hook #'glicol-doom-setup-keys)
(add-hook 'after-change-major-mode-hook #'glicol-doom-setup-keys)

(provide 'glicol-doom)
;;; glicol-doom.el ends here
