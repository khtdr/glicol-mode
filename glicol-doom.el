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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun glicol-doom-setup-keys ()
  "Setup Glicol keybindings for Glicol major mode"
  (map! :leader
        (:prefix ("G" . "Glicol")

                 )))



;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'glicol-doom-setup-keys)
(add-hook 'dired-mode-hook #'glicol-doom-setup-keys)
(add-hook 'after-change-major-mode-hook #'glicol-doom-setup-keys)

(provide 'glicol-doom)
;;; glicol-doom.el ends here
