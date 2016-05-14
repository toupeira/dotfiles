(setq dotfiles-evil-packages
      '(evil helm simpleclip))

(defun dotfiles-evil/post-init-evil ()
  ;; evil settings
  (setq
   evil-cross-lines t
   evil-escape-delay 0
   evil-ex-interactive-search-highlight 'selected-window
   evil-split-window-below t
   evil-vsplit-window-right t
  )

  ;; use C-c to escape
  (define-key key-translation-map (kbd "C-c") 'dotfiles/escape-anywhere)
  (define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)

  ;; also clear search highlight on C-c
  (add-hook 'dotfiles/escape-anywhere-hook
            (lambda ()
              (evil-search-highlight-persist-remove-all)))

  ;; show file name with C-g
  (global-set-key (kbd "C-g") 'dotfiles/identify-buffer)

  ;; navigate windows with C-h/j/k/l
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; always focus new splits
  (spacemacs/set-leader-keys
    "ws" 'split-window-below-and-focus
    "wS" 'split-window-below
    "wv" 'split-window-right-and-focus
    "wV" 'split-window-right)

  ;; yank linewise with Y
  (define-key evil-normal-state-map (kbd "Y") (kbd "yy"))

  ;; duplicate region with D
  (define-key evil-visual-state-map (kbd "D") 'dotfiles/duplicate-region)

  ;; readline keys in insert mode
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

  ;; cycle numbers with C-a/x
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

  ;; emulate C-u behaviour from Vim
  (define-key evil-insert-state-map (kbd "C-u") 'dotfiles/backward-kill-line)
)

(defun dotfiles-evil/post-init-helm ()
  ;; use C-w to delete words in Helm
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-w") 'backward-kill-word))
)

(defun dotfiles-evil/init-simpleclip ()
  ;; don't use desktop clipboard for kill ring
  (simpleclip-mode t)
)

(defun dotfiles-evil/post-init-simpleclip ()
  ;; paste with C-v
  (global-set-key (kbd "C-v") 'dotfiles/paste)
  (define-key evil-normal-state-map (kbd "C-v") 'dotfiles/paste)
  (define-key evil-insert-state-map (kbd "C-v") 'dotfiles/paste)
  (define-key evil-visual-state-map (kbd "C-v") 'dotfiles/paste)

  ;; use C-q for quoted insert
  (global-set-key (kbd "C-q") 'quoted-insert)
  (define-key evil-insert-state-map (kbd "C-q") 'quoted-insert)

  ;; copy with C-c
  (add-hook 'dotfiles/escape-anywhere-hook 'dotfiles/copy)
)
