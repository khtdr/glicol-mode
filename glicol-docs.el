;;; glicol-docs.el --- Documentation functions for Glicol -*- lexical-binding: t -*-

(require 'json)

(defvar glicol-docs--data nil
  "Cached documentation data from glicol.json.")

(defun glicol-docs--load-data ()
  "Load the documentation data from glicol.json."
  (unless glicol-docs--data
    (let ((json-file (expand-file-name "glicol.json")))
      (when (file-exists-p json-file)
        (with-temp-buffer
          (insert-file-contents json-file)
          (setq glicol-docs--data
                (json-read-from-string (buffer-string))))))))

(defun glicol-docs-get-node (node-name)
  "Get documentation for NODE-NAME as a formatted string."
  (glicol-docs--load-data)
  (when-let* ((node-data (alist-get (intern node-name) glicol-docs--data))
              (desc (alist-get 'description node-data))
              (params (alist-get 'parameters node-data))
              (input (alist-get 'input node-data))
              (output (alist-get 'output node-data))
              (example (alist-get 'example node-data)))
    (with-temp-buffer
      (insert (format "Node: %s\n\n" node-name))
      (insert (format "Description:\n%s\n\n" desc))
      (insert "Parameters:\n")
      (dolist (param params)
        (let* ((param-name (car (car param)))
               (param-type (cdr (car param))))
          (insert (format "  %s: %s\n" param-name param-type))))
      (insert (format "\nInput: %s\n" input))
      (insert (format "\nOutput: %s\n" output))
      (unless (string-empty-p example)
        (insert "\nExample:\n")
        (insert (format "%s\n" example)))
      (buffer-string))))

(defun glicol-docs-list-nodes ()
  "Return a list of all available node names."
  (glicol-docs--load-data)
  (mapcar #'symbol-name (mapcar #'car glicol-docs--data)))

(defun glicol-docs-show (node-name)
  "Display documentation for NODE-NAME in a help buffer."
  (interactive
   (list (completing-read "Node: " (glicol-docs-list-nodes))))
  (when-let ((doc-string (glicol-docs-get-node node-name)))
    (with-help-window "*Glicol Documentation*"
      (with-current-buffer "*Glicol Documentation*"
        (insert doc-string)))))

(provide 'glicol-docs)
;;; glicol-docs.el ends here
