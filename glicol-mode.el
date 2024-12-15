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
(require 'glicol-docs)
(require 'glicol-docs-ui)
(require 'glicol-cli)
(require 'glicol-company)

;;; Customization

(defgroup glicol nil
  "Major mode for editing Glicol files."
  :group 'languages)

;;; Syntax Table


(defconst glicol-mode-syntax-table
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

;;; Font Lock

(defconst glicol-font-lock-keywords
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


;;; Mode Definition

;;;###autoload
(define-derived-mode glicol-mode prog-mode "Glicol"
  "Major mode for editing Glicol files."
  :syntax-table glicol-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(glicol-font-lock-keywords))
  (add-to-list 'company-backends 'company-glicol))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.glicol\\'" . glicol-mode))

;;; Key Bindings
;; Documentation commands
(define-key glicol-mode-map (kbd "C-c C-h") #'glicol-describe-node)
(define-key glicol-mode-map (kbd "C-c C-d") #'glicol-describe-node-at-point)

;; CLI commands
(define-key glicol-mode-map (kbd "C-c C-s") #'glicol-start-cli)
(define-key glicol-mode-map (kbd "C-c C-q") #'glicol-stop-cli)
(define-key glicol-mode-map (kbd "C-c C-c") #'glicol-server-status)
(define-key glicol-mode-map (kbd "C-c C-r") #'glicol-restart-cli)
(define-key glicol-mode-map (kbd "C-c C-b") #'glicol-set-bpm)

(when (featurep 'doom)
  (require 'glicol-doom))

(provide 'glicol-mode)
;;; glicol-mode.el ends here
