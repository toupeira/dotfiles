(spacemacs/set-leader-keys
  "bD" 'kill-buffer-and-window ;; https://github.com/syl20bnr/spacemacs/issues/6334
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
