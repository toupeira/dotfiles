(defmacro dotfiles/silence (&rest body)
  `(let ((previous-message (current-message)))
     ,@body
     (when previous-message
       (message previous-message))))

(defun dotfiles/startup ()
  (remove-hook 'persp-mode-hook 'dotfiles/startup)
  (spacemacs/find-dotfile)

  (persp-switch org-directory)
  (if dotfiles/is-ocelot
      (dotfiles/org-goto "work" "w")
    (dotfiles/org-goto "organizer" "o")))

(defun dotfiles/switch-to-project-layout (&rest args)
  (when (projectile-project-p)
    (let* ((buffer (current-buffer))
           (old-persp (get-current-persp))
           (root (if (string-prefix-p dotfiles/directory (file-truename (buffer-file-name buffer)))
                     dotfiles/directory
                   (projectile-project-root)))
           (new-persp (abbreviate-file-name root)))
      (switch-to-buffer (other-buffer))
      (persp-switch new-persp)
      (persp-add-buffer buffer)
      (when (and old-persp (not (string= new-persp (persp-name old-persp))))
        (persp-remove-buffer buffer old-persp)))))

;; show file name, path and project in title
;; https://github.com/syl20bnr/spacemacs/pull/5924
(defun dotfiles/frame-title-format ()
  "Return frame title with current project name and path."
  (let* ((file (buffer-file-name))
         (path (if file (file-truename file) ""))
         (name (if file (file-name-nondirectory path) (buffer-name)))
         (directory (if file (s-chop-suffix "/" (file-name-directory path)) ""))

         (project (projectile-project-p))
         (directory (if (and project file)
                        (file-relative-name directory (projectile-project-root))
                      (abbreviate-file-name directory)))
         (directory (if (string= "." directory) "" directory)))
    (format "%s [%s%s%s]"
            name
            (if project (projectile-project-name) dotspacemacs-default-layout-name)
            (if (not (string= "" directory)) ":" "")
            directory)))

(defun dotfiles/auto-list-errors ()
  (if flycheck-current-errors
      (flycheck-list-errors)
    (-if-let (window (flycheck-get-error-list-window))
        (quit-window nil window))))

(defun dotfiles/auto-resize-errors ()
  (-when-let (window (flycheck-get-error-list-window t))
    (with-selected-window window
      (fit-window-to-buffer window 5))))

(defun dotfiles/ssh-key-loaded ()
  (eq 0 (call-process-shell-command "ssh-add -l | fgrep -q '/.ssh/id_rsa '")))

(defun dotfiles/ssh-key-load ()
  (when (not (dotfiles/ssh-key-loaded))
    (call-process "ssh-add"))

  (when (dotfiles/ssh-key-loaded)
    (remove-hook 'magit-pre-call-git-hook 'dotfiles/ssh-key-load)
    (remove-hook 'magit-pre-start-git-hook 'dotfiles/ssh-key-load)))
