;;; glicol-docs-ui.el --- Documentation UI for Glicol mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides documentation display functionality for glicol-mode

;;; Code:

(require 'glicol-docs)

(defvar glicol-doc-buffer-name "*Glicol Doc*"
  "Name of the buffer for displaying Glicol documentation.")

(defun glicol-describe-node-at-point ()
  "Display documentation for the Glicol node at point."
  (interactive)
  (let* ((node (thing-at-point 'symbol))
         (node-sym (when node (intern node)))
         (doc (when node-sym (alist-get node-sym glicol-node-docs))))
    (if doc
        (with-help-window glicol-doc-buffer-name
          (princ (format "Node: %s\n\n" node))
          (princ (format "Description:\n%s\n\n" (alist-get 'description doc)))
          (princ "Parameters:\n")
          (dolist (param (alist-get 'parameters doc))
            (princ (format "  %s: %s\n" (car param) (cdr param))))
          (princ (format "\nInput:\n%s\n" (alist-get 'input doc)))
          (princ (format "\nOutput:\n%s\n" (alist-get 'output doc)))
          (princ (format "\nExample:\n%s\n" (alist-get 'example doc))))
      (message "No documentation found for node at point"))))

(defun glicol-describe-node (node)
  "Display documentation for a Glicol NODE."
  (interactive
   (list (completing-read "Node: " 
                         (mapcar #'symbol-name (mapcar #'car glicol-node-docs))
                         nil t)))
  (let ((node-sym (intern node)))
    (if-let ((doc (alist-get node-sym glicol-node-docs)))
        (with-help-window glicol-doc-buffer-name
          (princ (format "Node: %s\n\n" node))
          (princ (format "Description:\n%s\n\n" (alist-get 'description doc)))
          (princ "Parameters:\n")
          (dolist (param (alist-get 'parameters doc))
            (princ (format "  %s: %s\n" (car param) (cdr param))))
          (princ (format "\nInput:\n%s\n" (alist-get 'input doc)))
          (princ (format "\nOutput:\n%s\n" (alist-get 'output doc)))
          (princ (format "\nExample:\n%s\n" (alist-get 'example doc))))
      (message "No documentation found for node '%s'" node))))

(provide 'glicol-docs-ui)
;;; glicol-docs-ui.el ends here
