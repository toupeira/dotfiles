(spacemacs/set-leader-keys
  "-"   'split-window-below-and-focus
  "|"   'split-window-right-and-focus
  "DEL" 'spacemacs/delete-window
  "RET" 'dotfiles/save-buffer
  "`"   'spacemacs/default-pop-shell

  "qz" 'spacemacs/prompt-kill-emacs
  "qZ" 'spacemacs/kill-emacs
  "qr" 'spacemacs/restart-emacs
  "qR" 'spacemacs/restart-emacs-resume-layouts

  "ws" 'split-window-below-and-focus
  "wS" 'split-window-below
  "wv" 'split-window-right-and-focus
  "wV" 'split-window-right)

(which-key-add-key-based-replacements
  "SPC -" "split-window-below"
  "SPC |" "split-window-right")

;; use qq/qQ to keep server running
(when dotspacemacs-persistent-server
  (spacemacs/set-leader-keys
    "qq" 'dotfiles/prompt-frame-killer
    "qQ" 'spacemacs/frame-killer))
