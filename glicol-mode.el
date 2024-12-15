;;; glicol-mode.el --- Major mode for Glicol audio programming language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jose Mazzarelli
;;
;; Author: Jose Mazzarelli <mazzarelli@gmail.com>
;; Maintainer: Jose Mazzarelli <mazzarelli@gmail.com>
;; Created: December 14, 2024
;; Modified: December 14, 2024
;;
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, multimedia
;; URL: https://github.com/khtdr/glicol-mode

;;; Commentary:
;; Major mode for editing Glicol audio programming language files.
;; Provides syntax highlighting and basic editing features.

;;; Code:

(require 'subr-x)
(require 'term)

(defgroup glicol nil
  "Major mode for editing Glicol files."
  :group 'languages)

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
      (when (featurep 'doom)
        (glicol-modeline-status-update 'running))
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
        (glicol-modeline-status-update 'stopped))
      (message "Glicol CLI stopped")))
  (unless glicol-cli-process
    (message "No running Glicol CLI instance found")))

(defvar glicol-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments start with //
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Symbol constituents
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?~ "_" table)
    table)
  "Syntax table for `glicol-mode'.")

(defvar glicol-font-lock-keywords
  `(
    ;; Comments
    (,"//.*$" . font-lock-comment-face)
    
    ;; Node references (starting with ~)
    (,"~[[:alnum:]_]+" . font-lock-variable-name-face)
    
    ;; Oscillators and generators
    (,(regexp-opt '("sin" "saw" "squ" "tri" "noiz" "imp") 'words)
     . font-lock-function-name-face)
    
    ;; Signal processors
    (,(regexp-opt '("mul" "add" "lpf" "hpf" "envperc" "delayms" "delayn" 
                   "allpass" "apf" "apfgain" "apfmsgain" "onepole" "plate"
                   "balance" "pan" "comb") 'words)
     . font-lock-builtin-face)
    
    ;; Sequencing and control
    (,(regexp-opt '("seq" "speed" "choose" "arrange" "meta") 'words)
     . font-lock-keyword-face)
    
    ;; Instruments and synths
    (,(regexp-opt '("sp" "bd" "sn" "hh" "sawsynth" "squsynth" "trisynth") 'words)
     . font-lock-type-face)
    
    ;; Mixing and routing
    (,(regexp-opt '("mix" "mono_sum" "buf" "pha" "state") 'words)
     . font-lock-preprocessor-face)
    
    ;; Numbers
    (,"\\<-?[0-9]*\\.?[0-9]+\\>" . font-lock-constant-face)
    
    ;; Node connection operator
    (">>" . font-lock-keyword-face))
  "Syntax highlighting for Glicol mode.")


;;;###autoload
(define-derived-mode glicol-mode prog-mode "Glicol"
  "Major mode for editing Glicol files."
  :syntax-table glicol-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(glicol-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.glicol\\'" . glicol-mode))

;; Key bindings for the Glicol mode map
(define-key glicol-mode-map (kbd "C-c C-s") #'glicol-start-cli)
(define-key glicol-mode-map (kbd "C-c C-q") #'glicol-stop-cli)
(define-key glicol-mode-map (kbd "C-c C-c") #'glicol-server-status)
(define-key glicol-mode-map (kbd "C-c C-r") #'glicol-restart-cli)
(define-key glicol-mode-map (kbd "C-c C-b") #'glicol-set-bpm)

(when (featurep 'doom)
  (require 'glicol-doom))

(provide 'glicol-mode)
;;; glicol-mode.el ends here
