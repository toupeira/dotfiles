(spacemacs/set-leader-keys
  "o/" 'helm-multi-swoop-org
  "oj" 'org-clock-goto
  "oO" 'org-clock-out

  "oa" 'org-agenda-list
  "ow" (lambda () (interactive) (dotfiles/org-goto "work" "w"))
  "oh" (lambda () (interactive) (dotfiles/org-goto "todo" "h"))

  "oH" (lambda () (interactive) (dotfiles/org-goto "todo" "H"))
  "oW" (lambda () (interactive) (dotfiles/org-goto "work" "W"))
  "oG" (lambda () (interactive) (dotfiles/org-goto "todo" "G" '("Goals")))
  "oE" (lambda ()
         (interactive)
         (dotfiles/org-goto "todo" nil '("Projects" "Emacs" "Inbox"))
         (org-show-subtree))

  "Ct" (lambda () (interactive) (org-capture nil "t"))
  "Ce" (lambda () (interactive) (org-capture nil "e"))

  "Cwi" (lambda () (interactive) (org-capture nil "wi"))
  "Cwt" (lambda () (interactive) (org-capture nil "wt"))
  "Cwp" (lambda () (interactive) (org-capture nil "wp"))
  )

(which-key-add-key-based-replacements
  "SPC o /" "smart search org files"

  "SPC o w" "agenda for work"
  "SPC o h" "agenda for home"

  "SPC o H" "review home tasks"
  "SPC o W" "review work tasks"
  "SPC o G" "review goals"
  "SPC o E" "goto emacs inbox"

  "SPC C t" "capture todo entry"
  "SPC C e" "capture emacs issue"

  "SPC C w i" "capture todo item"
  "SPC C w t" "capture task with project"
  "SPC C w p" "capture project"
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "u" 'dotfiles/org-update-buffer
  "C" 'org-copy)
