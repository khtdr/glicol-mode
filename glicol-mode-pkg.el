(define-package "glicol-mode" "0.1.0"
  "Major mode for Glicol audio programming language"
  '((emacs "26.1")
    (company "0.9.13"))
  :authors '(("Jose Mazzarelli" . "mazzarelli@gmail.com"))
  :maintainer '("Jose Mazzarelli" . "mazzarelli@gmail.com")
  :keywords '("languages" "multimedia")
  :url "https://github.com/khtdr/glicol-mode")

;;;###autoload
(eval-after-load 'glicol-mode
  '(progn
     (add-to-list 'glicol-mode-load-path (file-name-directory load-file-name))))
