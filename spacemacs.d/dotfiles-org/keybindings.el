(spacemacs/set-leader-keys
  "Ct" (lambda () (interactive) (org-capture nil "t") (evil-append-line))
  "Cw" (lambda () (interactive) (org-capture nil "w") (evil-append-line)))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "u" 'org-update-all-dblocks)
