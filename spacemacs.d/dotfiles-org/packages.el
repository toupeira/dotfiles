(setq dotfiles-org-packages
 '(
   org
   org-indent

   org-agenda-property
  ))

(defun dotfiles-org/post-init-org ()
  (setq
   calendar-week-start-day 1
   calendar-day-name-array (locale-info 'days)
   calendar-month-name-array (locale-info 'months)

   org-agenda-files '("~/org")
   org-agenda-buffer-name "*agenda*"
   org-agenda-window-setup 'only-window
   org-agenda-include-diary nil
   org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5)
   org-agenda-show-inherited-tags nil
   org-attach-directory "attachments/"
   org-blank-before-new-entry '((heading . auto) (plain-list-item . nil))
   org-clock-history-length 25
   org-clock-idle-time 30
   org-clock-in-resume t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist t
   org-clock-clocktable-default-properties
   '(:block today)
   org-clocktable-defaults
   '(:maxlevel 3 :scope file-with-archives :properties ("CATEGORY")
               :indent t :link t :narrow 40!)
   org-columns-default-format "%TODO %40ITEM %SCHEDULED %DEADLINE %CLOCKSUM"
   org-cycle-separator-lines 1
   org-directory (abbreviate-file-name (file-truename "~/org/"))
   org-download-method 'attach
   org-enforce-todo-dependencies t
   org-fontify-done-headline t
   org-habit-show-done-always-green t
   org-log-into-drawer t
   org-log-redeadline nil
   org-log-refile nil
   org-log-reschedule nil
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes t
   org-refile-targets '((dotfiles/org-refile-targets :maxlevel . 3))
   org-refile-use-outline-path 'file
   org-startup-align-all-tables t
   org-startup-folded 'content
   org-startup-indented t
   org-tag-persistent-alist
   '(("work"   . ?w))
   org-todo-keywords
   '((sequence "TODO" "NEXT" "STARTED(!)" "|" "DONE")
     (sequence "PENDING" "|" "DONE")
     (sequence "MAYBE" "GOAL" "FOCUS" "|" "DONE" "ABANDONED(!)"))
   org-todo-keyword-faces
   '(("NEXT" . "#FD971F")
     ("GOAL" . "#FD971F")
     ("STARTED" . "#AE81FF")
     ("FOCUS" . "#AE81FF")
     ("DONE" . org-done))
  )

  (let* ((task-list-options
          '((org-agenda-sorting-strategy '((todo timestamp-up category-keep)))
            (org-agenda-property-list '("SCHEDULED" "DEADLINE" "CLOCKSUM"))
            (org-agenda-tags-column -70)
            (org-agenda-property-column 70)))
         (task-list
          `((todo "STARTED" ,(cons '(org-agenda-overriding-header "Started tasks:") task-list-options))
            (todo "NEXT" ,(cons '(org-agenda-overriding-header "Next tasks:") task-list-options))
            (tags-todo "+SCHEDULED={.+}|+DEADLINE={.+}/TODO"
                       ,(cons '(org-agenda-overriding-header "Scheduled tasks:") task-list-options))
            (tags-todo "-SCHEDULED={.+}-DEADLINE={.+}/TODO"
                       ,(cons '(org-agenda-overriding-header "Unscheduled tasks:") task-list-options))))
         (goal-list
          `((todo "FOCUS" ,(cons '(org-agenda-overriding-header "Goals to focus on:") task-list-options))
            (todo "GOAL"  ,(cons '(org-agenda-overriding-header "Other goals:") task-list-options))
            (todo "MAYBE" ,(cons '(org-agenda-overriding-header "Other ideas:") task-list-options)))))
    (setq
     org-agenda-custom-commands
     `(
       ("o" "Agenda for home context"
        ,(cons '(agenda "") (subseq task-list 0 2))
        ((org-agenda-tag-filter-preset '("-work"))))
       ("w" "Agenda for work context"
        ,(cons '(agenda "") (subseq task-list 0 2))
        ((org-agenda-tag-filter-preset '("+work"))))

       ("O" "Review home tasks" ,task-list
        ,(cons '(org-agenda-tag-filter-preset '("-work")) task-list-options))
       ("W" "Review work tasks" ,task-list
        ,(cons '(org-agenda-tag-filter-preset '("+work")) task-list-options))
       ("G" "Review goals" ,goal-list)
      )))

  (setq
   org-capture-templates
   '(
     ("o" "inbox" entry (file+olp (concat org-directory "organizer.org") "Inbox")
      "* TODO %?"
      :empty-lines-after 1)
     ("b" "basteln" entry (file+olp (concat org-directory "organizer.org") "Projects" "Basteln")
      "* TODO %?")
     ("e" "emacs" checkitem (file+olp (concat org-directory "organizer.org") "Projects" "Emacs" "Inbox")
      "- [ ] %?")

     ("w" "work")

     ("ww" "inbox" entry (file+olp (concat org-directory "work.org") "Inbox")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:%^{CATEGORY}p\n\n"
      :empty-lines-after 1)

     ("wb" "basteln" entry (file+olp (concat org-directory "work.org") "Panter" "Basteln")
      "* TODO %?")

     ("wp" "add project" entry (file+olp (concat org-directory "work.org") "Projects")
      "* %^{category} - %^{title}\n:PROPERTIES:\n:CATEGORY: %\\1\n:END:"
      :immediate-finish t :jump-to-captured t)
    )
  )

  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'calendar-load-hook (lambda () (calendar-set-date-style 'european)))

  ;; enable org-mode for additional extensions
  (add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit)
    (org-clock-persistence-insinuate)
    ;; use T to cycle backwords through todo states
    (evil-define-key 'normal evil-org-mode-map (kbd "T") 'org-shiftleft))

  (with-eval-after-load 'org-agenda
    ;; use T to cycle backwords through todo states
    (define-key org-agenda-mode-map (kbd "T")
      (lambda () (interactive) (org-agenda-todo 'left)))

    (define-key org-agenda-mode-map (kbd "o") 'org-agenda-open-link)

    ;; use uppercase letters to switch period
    (define-key org-agenda-mode-map (kbd "D") 'org-agenda-day-view)
    (define-key org-agenda-mode-map (kbd "W") 'org-agenda-week-view)
    (define-key org-agenda-mode-map (kbd "M") 'org-agenda-month-view)
    (define-key org-agenda-mode-map (kbd "Y") 'org-agenda-year-view)

    (define-key org-agenda-mode-map (kbd "d") 'org-agenda-deadline)
    (define-key org-agenda-mode-map (kbd "s") 'org-agenda-schedule)
    (define-key org-agenda-mode-map (kbd "w") 'org-save-all-org-buffers)

    ;; don't use $ for archiving
    (define-key org-agenda-mode-map (kbd "$") 'evil-end-of-line)
  )

  ;; auto-save buffers in agenda
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (advice-add 'org-agenda-redo :before 'org-save-all-org-buffers)

  ;; shrink capture window and start in insert mode
  (add-hook
   'org-capture-mode-hook
   (lambda ()
     (fit-window-to-buffer (selected-window) 10)
     (shrink-window-if-larger-than-buffer)))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; set clocked tasks to STARTED state
  (add-hook 'org-clock-in-hook 'dotfiles/org-start-task)

  ;; save files when clocking tasks
  (add-hook 'org-clock-in-hook (lambda () (dotfiles/silence (save-buffer))) t)
  (add-hook 'org-clock-out-hook (lambda () (dotfiles/silence (save-buffer))) t)

  ;; start/stop clocked tasks in hamster
  (when (executable-find "hamster")
    (add-hook 'org-clock-in-hook 'dotfiles/org-start-hamster-task)
    (add-hook 'org-clock-out-hook 'dotfiles/org-stop-hamster-task))

  ;; keep headlines when archiving tasks
  ;; http://orgmode.org/worg/org-hacks.html
  (with-eval-after-load 'org-archive
    (defadvice org-archive-subtree (around dotfiles/org-archive-subtree activate)
      (let ((org-archive-location
             (if (save-excursion (org-back-to-heading)
                                 (> (org-outline-level) 1))
                 (concat (car (split-string org-archive-location "::"))
                         "::* "
                         (s-join " - " (org-get-outline-path)))
               org-archive-location)))
        ad-do-it))
  )
)

(defun dotfiles-org/init-org-agenda-property ()
  (use-package org-agenda-property
    :defer t
    :config
    (setq org-agenda-property-list '("LOCATION"))))
