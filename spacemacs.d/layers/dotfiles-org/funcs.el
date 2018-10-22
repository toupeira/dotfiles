(defun dotfiles/org-directory ()
  (abbreviate-file-name (file-truename org-directory)))

(defun dotfiles/org-goto (file &optional agenda path)
  (interactive)
  (find-file (concat org-directory "/" file ".org"))
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

(defun dotfiles/org-clock-string ()
  (if (org-clocking-p)
      (let* ((category (save-window-excursion (org-clock-goto) (org-get-category)))
             (duration (org-duration-from-minutes (org-clock-get-clocked-time))))
        (format "%s\n%s" category duration))
    "No task"))
