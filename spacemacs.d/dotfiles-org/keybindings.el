(spacemacs/set-leader-keys
  "oa" 'org-agenda-list
  "oo" 'org-agenda
  "oO" 'org-clock-out
  "o/" (lambda () (interactive)
         (let ((projectile-project-root org-directory))
           (spacemacs/helm-project-smart-do-search)))

  "ot" (lambda () (interactive) (find-file (concat org-directory "todo.org")))
  "ow" (lambda () (interactive) (find-file (concat org-directory "work.org")))
  "os" (lambda () (interactive) (find-file (concat org-directory "someday.org")))

  "Ct" (lambda () (interactive) (org-capture nil "t"))
  "Cw" (lambda () (interactive) (org-capture nil "w"))
  "Cs" (lambda () (interactive) (org-capture nil "s"))
  "Ce" (lambda () (interactive) (org-capture nil "e")))

(which-key-add-key-based-replacements
  "SPC o /" "smart search org files"
  "SPC o t" "find-org todo"
  "SPC o w" "find-org work"
  "SPC o s" "find-org someday"

  "SPC C t" "org-capture todo"
  "SPC C w" "org-capture work"
  "SPC C s" "org-capture someday"
  "SPC C e" "org-capture emacs"
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "u" 'org-update-all-dblocks)
