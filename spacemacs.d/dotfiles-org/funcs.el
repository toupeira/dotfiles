(defun dotfiles/org-refile-targets ()
  (let* ((files (org-agenda-files))
         (files (reverse files)))
    (remove-if-not 'file-writable-p files)))

(defun dotfiles/org-start-task ()
  (org-todo "STARTED"))

(defun dotfiles/org-start-hamster-task ()
  (let* ((category (org-get-category))
         (task org-clock-current-task)
         (description (format "%s @%s, %s" category category task)))
    (call-process "hamster" nil nil nil "start" description)))

(defun dotfiles/org-stop-hamster-task ()
  (call-process "hamster" nil nil nil "stop"))
