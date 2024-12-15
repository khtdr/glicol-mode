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

;;; glicol-mode.el ends here
