(setq dotfiles-org-packages
      '(org))

(defun dotfiles-org/post-init-org ()
  (setq
   org-enforce-todo-dependencies t
   org-log-reschedule (quote time)
   org-log-redeadline (quote time)
   org-agenda-files
   '("~/documents")
   org-todo-keywords
   '((sequence "TODO" "IN PROGRESS" "|" "DONE"))
  )

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "SPC" 'org-toggle-checkbox)
)
