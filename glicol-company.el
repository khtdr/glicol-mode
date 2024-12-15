;;; glicol-company.el --- Company completion for Glicol mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides company-mode completion support for glicol-mode

;;; Code:

(require 'company)
(require 'glicol-docs)

(defun company-glicol--candidates (prefix)
  "Generate completion candidates for PREFIX."
  (let ((nodes (mapcar #'symbol-name (mapcar #'car glicol-node-docs))))
    (all-completions prefix nodes)))

(defun company-glicol--annotation (candidate)
  "Return annotation for CANDIDATE."
  (when-let* ((node-sym (intern candidate))
              (doc (alist-get node-sym glicol-node-docs))
              (desc (alist-get 'description doc)))
    (concat " - " (car (split-string desc "\n")))))

(defun company-glicol--doc-buffer (candidate)
  "Display documentation buffer for CANDIDATE."
  (when-let* ((node-sym (intern candidate))
              (doc (alist-get node-sym glicol-node-docs)))
    (company-doc-buffer
     (format "%s\n\nParameters:\n%s\n\nInput: %s\nOutput: %s\n\nExample:\n%s"
             (alist-get 'description doc)
             (mapconcat (lambda (param)
                         (format "  %s: %s" (car param) (cdr param)))
                       (alist-get 'parameters doc) "\n")
             (alist-get 'input doc)
             (alist-get 'output doc)
             (alist-get 'example doc)))))

;;;###autoload
(defun company-glicol (command &optional arg &rest _ignored)
  "Company backend for Glicol completion.
COMMAND is the command to execute.
ARG is the optional command argument."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-glicol))
    (prefix (and (eq major-mode 'glicol-mode)
                (company-grab-symbol)))
    (candidates (company-glicol--candidates arg))
    (annotation (company-glicol--annotation arg))
    (doc-buffer (company-glicol--doc-buffer arg))
    (sorted t)))

(provide 'company-glicol)
;;; glicol-company.el ends here
