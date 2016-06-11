(spacemacs/set-leader-keys
  "Ct" (lambda () (interactive) (org-capture nil "t"))
  "Cw" (lambda () (interactive) (org-capture nil "w"))
  "Cs" (lambda () (interactive) (org-capture nil "s"))
  "Ce" (lambda () (interactive) (org-capture nil "e")))

(which-key-add-key-based-replacements
  "SPC C t" "org-capture-todo"
  "SPC C w" "org-capture-work"
  "SPC C s" "org-capture-someday"
  "SPC C e" "org-capture-emacs"
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "u" 'org-update-all-dblocks)
