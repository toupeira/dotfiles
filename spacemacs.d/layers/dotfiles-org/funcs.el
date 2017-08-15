(defun dotfiles/org-goto (file &optional agenda path)
  (interactive)
  (find-file (concat org-directory file ".org"))
  (evil-goto-first-line)
  (org-set-startup-visibility)
  (outline-next-heading)

  (when path (goto-char (org-find-olp path t)))
  (when agenda
    (if (eq t agenda)
        (org-agenda-list)
      (org-agenda nil agenda))))

(defun dotfiles/org-update-buffer ()
  (interactive)
  (org-save-outline-visibility 'use-markers (org-mode-restart))
  (org-redisplay-inline-images)
  (org-update-all-dblocks)
  (dotfiles/silence (save-buffer)))

(defun dotfiles/org-refile-targets ()
  (let* ((files (org-agenda-files))
         (files (reverse files)))
    (remove-if-not 'file-writable-p files)))

(defun dotfiles/org-start-task ()
  (let ((state (org-get-todo-state)))
    (when (or (string= state "TODO") (string= state "NEXT") (string= state "WAITING"))
      (dotfiles/silence
       (org-todo "STARTED")))))

(defun dotfiles/org-start-hamster-task ()
  (let* ((file (file-name-base (buffer-file-name (marker-buffer org-clock-marker))))
         (category (org-get-category))
         (task org-clock-current-task)
         (description (format "%s @%s, %s" category category task))
         (description
          (if (string= file category) description
            (concat file ": " description))))
    (call-process "hamster" nil nil nil "start" description)))

(defun dotfiles/org-stop-hamster-task ()
  (call-process "hamster" nil nil nil "stop"))
