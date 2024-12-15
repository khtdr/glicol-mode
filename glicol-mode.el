;;; glicol-mode.el --- Major mode for Glicol audio programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Your Name
;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, multimedia
;; URL: https://github.com/yourusername/glicol-mode

;;; Commentary:
;; Major mode for editing Glicol audio programming language files.
;; Provides syntax highlighting and basic editing features.

;;; Code:

(require 'subr-x)

(defgroup glicol nil
  "Major mode for editing Glicol files."
  :group 'languages)

(defcustom glicol-cli-command "glicol-cli"
  "Command to run the Glicol CLI TUI.
This should be the path to the glicol-cli executable.
The default assumes it's available in your PATH as 'glicol-cli'."
  :type 'string
  :group 'glicol)

(defvar glicol-cli-process nil
  "Process handle for the running Glicol CLI instance.")

(defvar glicol-cli-buffer-name "*Glicol CLI*"
  "Name of the buffer where the Glicol CLI runs.")

(defun glicol-start-cli ()
  "Start the Glicol CLI in a dedicated buffer."
  (interactive)
  (if glicol-cli-process
      (message "Glicol CLI is already running!")
    (let ((buffer (get-buffer-create glicol-cli-buffer-name)))
      (with-current-buffer buffer
        (special-mode))  ; Make it read-only
      (unless buffer-file-name
        (error "Buffer is not visiting a file"))
      (setq glicol-cli-process
            (start-process "glicol-cli" buffer glicol-cli-command buffer-file-name))
      (display-buffer buffer
                     '((display-buffer-in-direction)
                       (direction . below)
                       (window-height . 0.5))))))

(defun glicol-stop-cli ()
  "Stop the running Glicol CLI instance and close its buffer."
  (interactive)
  (when glicol-cli-process
    (delete-process glicol-cli-process)
    (setq glicol-cli-process nil)
    (when-let ((buffer (get-buffer glicol-cli-buffer-name)))
      (kill-buffer buffer))
    (message "Glicol CLI stopped"))
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
    ;; Basic nodes
    (,(regexp-opt '("sin" "saw" "squ" "tri" "noiz" "imp" "seq" "sp" "mul" "add" "mix" "lpf" "plate" "meta" "speed" "envperc" "choose" "arrange") 'words)
     . font-lock-builtin-face)
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

(provide 'glicol-mode)

;; Key bindings for the Glicol mode map
(define-key glicol-mode-map (kbd "C-c C-s") #'glicol-start-cli)
(define-key glicol-mode-map (kbd "C-c C-q") #'glicol-stop-cli)

;;; glicol-mode.el ends here
