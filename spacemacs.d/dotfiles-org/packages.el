(setq dotfiles-org-packages
 '(
   org
   org-indent
   org-gnome
   (org-repo-todo :excluded t)
  ))

(defun dotfiles-org/post-init-org ()
  (setq
   org-directory "~/org"
   org-enforce-todo-dependencies t
   org-log-into-drawer t
   org-log-refile t
   org-log-reschedule nil
   org-log-redeadline nil
   org-cycle-separator-lines 1
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   org-startup-align-all-tables t
   org-startup-folded 'content
   org-startup-indented t
   org-todo-keywords
   '((sequence "TODO" "STARTED" "|" "DONE"))
   org-todo-keyword-faces
   '(("STARTED" . "#AE81FF")
     ("DONE" . org-special-keyword))
   org-tag-persistent-alist
   '(("home"   . ?h)
     ("work"   . ?w)
     ("events" . ?e))

   org-agenda-files '("~/org")
   org-agenda-buffer-name "*agenda*"
   org-agenda-window-setup 'other-window
   org-agenda-include-diary nil

   org-agenda-custom-commands
   '(
      ("h" "Home context" agenda ""
       ((org-agenda-tag-filter-preset '("-work"))))
      ("w" "Work context" agenda ""
       ((org-agenda-tag-filter-preset '("-home"))
        (org-agenda-start-with-clockreport-mode t)))
      ("g" "GTD workflow"
       ((todo "STARTED")
        (todo "TODO")))
    )

   org-capture-templates
   '(
     ("t" "Todo" entry (file+headline "todo.org" "Inbox")
      "* TODO %?")
     ("w" "Work" entry (file+headline "work.org" "Inbox")
      "* TODO %?\n%a")
    )
  )

  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit))

  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-w") 'evil-window-map)
    (define-key org-agenda-mode-map (kbd "C-h") 'evil-window-left)
    (define-key org-agenda-mode-map (kbd "C-j") 'evil-window-down)
    (define-key org-agenda-mode-map (kbd "C-k") 'evil-window-up)
    (define-key org-agenda-mode-map (kbd "C-l") 'evil-window-right)

    (define-key org-agenda-mode-map (kbd "d") 'org-agenda-deadline)
    (define-key org-agenda-mode-map (kbd "s") 'org-agenda-schedule)
    (define-key org-agenda-mode-map (kbd "w") 'org-save-all-org-buffers)

    (define-key org-agenda-mode-map (kbd "D") 'org-agenda-day-view)
    (define-key org-agenda-mode-map (kbd "W") 'org-agenda-week-view)
    (define-key org-agenda-mode-map (kbd "M") 'org-agenda-month-view)
    (define-key org-agenda-mode-map (kbd "Y") 'org-agenda-year-view)
  )

  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook 'evil-insert-state)
    (define-key org-capture-mode-map [remap evil-save-and-close] 'org-capture-finalize)
    (define-key org-capture-mode-map [remap evil-save-modified-and-close] 'org-capture-finalize)
    (define-key org-capture-mode-map [remap evil-quit] 'org-capture-kill)
  )

  (add-hook
   'org-clock-in-hook
   (lambda ()
     (org-todo "STARTED")
     ))

  (when (executable-find "hamster")
    (add-hook
     'org-clock-in-hook
     (lambda ()
       (let* ((file (file-name-base (buffer-file-name (marker-buffer org-clock-marker))))
              (section (org-find-top-headline org-clock-marker))
              (task org-clock-current-task)
              (description (concat file ": " section " @" section ", " task)))
         (call-process "hamster" nil nil nil "start" description)
         )))

    (add-hook
     'org-clock-out-hook
     (lambda ()
       (call-process "hamster" nil nil nil "stop")
       ))
  )
)

(when dotfiles/is-ocelot
  (defun dotfiles-org/init-org-gnome ()
    (use-package org-gnome
      :init
      (setq org-gnome-notify-appointments t
            org-gnome-integrate-with-calendar t
            org-gnome-integrate-with-empathy nil
            org-gnome-appointment-icon "/usr/share/icons/gnome/scalable/status/appointment-soon-symbolic.svg")
      :config
      (org-gnome-turn-on))
  )
)
