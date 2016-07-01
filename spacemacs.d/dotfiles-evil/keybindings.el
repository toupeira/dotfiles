(spacemacs/set-leader-keys
  "S"  'split-window-below-and-focus
  "V"  'split-window-right-and-focus
  "W"  'save-buffer
  "ws" 'split-window-below-and-focus
  "wS" 'split-window-below
  "wv" 'split-window-right-and-focus
  "wV" 'split-window-right)

;; use qq/qQ to keep server running
(when dotspacemacs-persistent-server
  (spacemacs/set-leader-keys
    "qq" 'dotfiles/prompt-frame-killer
    "qQ" 'spacemacs/frame-killer))

;; use qz/qZ to kill server
(spacemacs/set-leader-keys
  "qz" 'spacemacs/prompt-kill-emacs
  "qZ" 'spacemacs/kill-emacs)
