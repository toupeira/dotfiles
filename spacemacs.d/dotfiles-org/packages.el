(setq dotfiles-org-packages
 '(
   org
   org-indent
   (org-repo-todo :excluded t)
  ))

(defun dotfiles-org/post-init-org ()
  (setq
   org-blank-before-new-entry '((heading . auto) (plain-list-item . nil))
   org-directory (abbreviate-file-name (file-truename "~/org/"))
   org-enforce-todo-dependencies t
   org-fontify-done-headline t
   org-log-into-drawer t
   org-log-refile nil
   org-log-reschedule nil
   org-log-redeadline nil
   org-cycle-separator-lines 1
   org-refile-targets '((dotfiles/org-refile-targets :maxlevel . 2))
   org-refile-use-outline-path 'file
   org-refile-allow-creating-parent-nodes t
   org-outline-path-complete-in-steps nil
   org-startup-align-all-tables t
   org-startup-folded 'content
   org-startup-indented t
   org-todo-keywords
   '((sequence "TODO" "NEXT" "STARTED" "|" "DONE"))
   org-todo-keyword-faces
   '(("NEXT" . "#FD971F")
     ("STARTED" . "#AE81FF")
     ("DONE" . org-done))
   org-tag-persistent-alist
   '(("work"   . ?w)
     ("events" . ?e))

   org-agenda-files '("~/org")
   org-agenda-buffer-name "*agenda*"
   org-agenda-window-setup 'only-window
   org-agenda-include-diary nil
   org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5)

   org-clock-history-length 25
   org-clock-in-resume t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist t

   calendar-week-start-day 1
   calendar-day-name-array (locale-info 'days)
   calendar-month-name-array (locale-info 'months)
  )

  (setq
   org-agenda-custom-commands
   '(
     ("h" "Home context"
      ((agenda ""))
      ((org-agenda-tag-filter-preset '("-work"))))
     ("w" "Work context"
      ((agenda ""))
      ((org-agenda-tag-filter-preset '("+work"))
       (org-agenda-hide-tags-regexp "^work$")))
     ("gg" "GTD over all contexts"
      ((todo "STARTED")
       (todo "NEXT")
       (todo "TODO")))
     ("gh" "GTD over home context"
      ((todo "STARTED")
       (todo "NEXT")
       (todo "TODO"))
      ((org-agenda-tag-filter-preset '("-work"))))
     ("gw" "GTD over work context"
      ((todo "STARTED")
       (todo "NEXT")
       (todo "TODO"))
      ((org-agenda-tag-filter-preset '("+work"))))
     ))

  (setq
   org-capture-templates
   '(
     ("t" "Todo" entry (file+olp (concat org-directory "todo.org") "Inbox")
      "* TODO %?" :empty-lines-after 2)
     ("w" "Work" entry (file+olp (concat org-directory "work.org") "Inbox")
      "* TODO %?\nCaptured: %U" :empty-lines-after 2)
     ("e" "Emacs" checkitem (file+olp (concat org-directory "todo.org") "Projects" "Emacs" "Inbox")
      "- [ ] %?")
     ("s" "Someday" entry (file+olp (concat org-directory "someday.org") "Inbox")
      "* %?" :empty-lines-after 2)
    )
  )

  (add-hook 'calendar-load-hook (lambda () (calendar-set-date-style 'european)))

  ;; enable org-mode for additional extensions
  (add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit)
    (org-clock-persistence-insinuate))

  (with-eval-after-load 'org-agenda
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
  (add-hook 'org-capture-mode-hook (lambda ()
                                     (fit-window-to-buffer (selected-window) 5)
                                     (shrink-window-if-larger-than-buffer)))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; start clocked tasks
  (add-hook 'org-clock-in-hook 'dotfiles/org-start-task)

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
