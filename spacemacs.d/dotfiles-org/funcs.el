(defun dotfiles/org-refile-targets ()
  (let* ((files (org-agenda-files))
         (files (reverse files)))
    (remove-if-not 'file-writable-p files)))

(defun dotfiles/org-start-task ()
  (org-todo "STARTED"))

(defun dotfiles/org-find-project-headline (&optional pos)
  (save-excursion
    (with-current-buffer (if pos (marker-buffer pos) (current-buffer))
      (when pos (goto-char pos))
      (let ((level (org-current-level)))
        (cond
         ((not level) nil)
         ((> level 2) (outline-up-heading (- level 2)))
         ((> level 1) (outline-up-heading (- level 1)))))

      (nth 4 (org-heading-components)))))

(defun dotfiles/org-start-hamster-task ()
  (let* ((file (file-name-base (buffer-file-name (marker-buffer org-clock-marker))))
         (project (dotfiles/org-find-project-headline org-clock-marker))
         (project (car (split-string project " - ")))
         (task org-clock-current-task)
         (description (format "%s: %s @%s, %s" file project project task)))
    (call-process "hamster" nil nil nil "start" description)))

(defun dotfiles/org-stop-hamster-task ()
  (call-process "hamster" nil nil nil "stop"))
