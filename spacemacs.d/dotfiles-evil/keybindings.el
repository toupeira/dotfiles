(spacemacs/set-leader-keys
  "`"  'spacemacs/default-pop-shell
  "D"  'spacemacs/delete-window
  "S"  'split-window-below-and-focus
  "V"  'split-window-right-and-focus
  "W"  'dotfiles/save-buffer
  "ws" 'split-window-below-and-focus
  "wS" 'split-window-below
  "wv" 'split-window-right-and-focus
  "wV" 'split-window-right)

(which-key-add-key-based-replacements
  "SPC S" "split-window-below"
  "SPC V" "split-window-right")

;; use qq/qQ to keep server running
(when dotspacemacs-persistent-server
  (spacemacs/set-leader-keys
    "qq" 'dotfiles/prompt-frame-killer
    "qQ" 'spacemacs/frame-killer))

;; use qz/qZ to kill server
(spacemacs/set-leader-keys
  "qz" 'spacemacs/prompt-kill-emacs
  "qZ" 'spacemacs/kill-emacs)
