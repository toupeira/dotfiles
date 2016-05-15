(setq dotfiles-org-packages
      '(org org-alert))

(defun dotfiles-org/post-init-org ()
  (setq
   org-directory "~/documents"
   org-enforce-todo-dependencies t
   org-log-reschedule (quote time)
   org-log-redeadline (quote time)
   org-cycle-separator-lines 1
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   org-todo-keywords
   '((sequence "TODO" "IN PROGRESS" "|" "DONE"))

   org-agenda-files '("~/documents")
   org-agenda-buffer-name "*Org Agenda*"
   org-agenda-window-setup 'current-window
   org-agenda-include-diary nil

   org-capture-templates
   '(
     ("t" "Todo" entry (file+headline "todo.org" "Inbox")
      "* TODO %?")
     ("w" "Work" entry (file+headline "work.org" "Inbox")
      "* TODO %?\n%a")
    )
  )

  (spacemacs/set-leader-keys
    "Ct" (lambda () (interactive) (org-capture nil "t") (evil-append-line))
    "Cw" (lambda () (interactive) (org-capture nil "w") (evil-append-line)))

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "SPC" 'org-toggle-checkbox)

  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit))

  (with-eval-after-load 'org-agenda
    (progn
      (define-key org-agenda-mode-map (kbd "C-w") 'evil-window-map)
      (define-key org-agenda-mode-map (kbd "C-h") 'evil-window-left)
      (define-key org-agenda-mode-map (kbd "C-j") 'evil-window-down)
      (define-key org-agenda-mode-map (kbd "C-k") 'evil-window-up)
      (define-key org-agenda-mode-map (kbd "C-l") 'evil-window-right)

      (define-key org-agenda-mode-map (kbd "W") 'org-agenda-week-view)
      (define-key org-agenda-mode-map (kbd "M") 'org-agenda-month-view)
      (define-key org-agenda-mode-map (kbd "Y") 'org-agenda-year-view)
    ))

  (with-eval-after-load 'org-capture
    (progn
      (add-hook 'org-capture-mode-hook 'evil-insert-state)
      (define-key org-capture-mode-map [remap evil-save-and-close] 'org-capture-finalize)
      (define-key org-capture-mode-map [remap evil-save-modified-and-close] 'org-capture-finalize)
      (define-key org-capture-mode-map [remap evil-quit] 'org-capture-kill)
    ))
)

(defun dotfiles-org/init-org-alert ()
  (use-package org-alert
    :init
    (setq alert-default-style 'libnotify)
    :config
    (org-alert-disable))
)
