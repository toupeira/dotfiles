(spacemacs/set-leader-keys
  "o/" 'helm-multi-swoop-org
  "oj" 'org-clock-goto

  "oa" 'org-agenda-list
  "ow" (lambda () (interactive) (dotfiles/org-goto "work" "w"))
  "oo" (lambda () (interactive) (dotfiles/org-goto "organizer" "o"))

  "oO" (lambda () (interactive) (dotfiles/org-goto "organizer" "O"))
  "oW" (lambda () (interactive) (dotfiles/org-goto "work" "W"))
  "oG" (lambda () (interactive) (dotfiles/org-goto "organizer" "G" '("Goals")))
  "oE" (lambda ()
         (interactive)
         (dotfiles/org-goto "organizer" nil '("Projects" "Emacs" "Inbox"))
         (org-show-subtree))

  "Cc" (lambda () (interactive) (org-capture nil "o"))
  "Cb" (lambda () (interactive) (org-capture nil "b"))
  "Ce" (lambda () (interactive) (org-capture nil "e"))
  "Cn" (lambda () (interactive) (org-capture nil "n"))

  "Cww" (lambda () (interactive) (org-capture nil "ww"))
  "Cwb" (lambda () (interactive) (org-capture nil "wb"))
  "Cwp" (lambda () (interactive) (org-capture nil "wp"))

  ;; remove Spacemacs bindings
  "Ci" nil
  "Cl" nil
  )

(which-key-add-key-based-replacements
  "SPC o /" "smart search org files"

  "SPC o w" "agenda for work"
  "SPC o o" "agenda for home"

  "SPC o O" "review home tasks"
  "SPC o W" "review work tasks"
  "SPC o G" "review goals"
  "SPC o E" "goto emacs inbox"

  "SPC C c" "inbox"
  "SPC C b" "basteln"
  "SPC C e" "emacs"
  "SPC C n" "clock note"

  "SPC C w"   "work"
  "SPC C w w" "inbox"
  "SPC C w b" "basteln"
  "SPC C w p" "add project"
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "u" 'dotfiles/org-update-buffer
  "C" 'org-copy)

(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
  "R" 'org-agenda-refile)
