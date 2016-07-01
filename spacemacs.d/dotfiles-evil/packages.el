(setq dotfiles-evil-packages
      '(evil
        simpleclip))

(defun dotfiles-evil/post-init-evil ()
  (setq
   evil-cross-lines t
   evil-escape-delay 0
   evil-ex-interactive-search-highlight 'selected-window
   evil-split-window-below t
   evil-vsplit-window-right t
   evil-want-fine-undo t
  )

  ;; use C-c to escape
  (define-key key-translation-map (kbd "C-c") 'dotfiles/escape-anywhere)
  (define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)

  ;; also clear search highlight on C-c
  (add-hook 'dotfiles/escape-anywhere-hook 'evil-search-highlight-persist-remove-all)

  ;; remove bindings for C-n/p since we have the paste transient state
  (define-key evil-normal-state-map "\C-n" nil)
  (define-key evil-normal-state-map "\C-p" nil)

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

  ;; add C-n/p and Up/Down in search prompt
  (define-key isearch-mode-map (kbd "C-n") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "C-p") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)

  ;; yank linewise with Y
  (define-key evil-normal-state-map (kbd "Y") (kbd "yy"))

  ;; paste shortcuts
  (define-key evil-insert-state-map (kbd "C-\"") (kbd "C-r \""))
  (define-key evil-insert-state-map (kbd "C-*") (kbd "C-r *"))
  (define-key evil-normal-state-map (kbd "C-*") (kbd "\" * P"))

  ;; duplicate region with D
  (define-key evil-visual-state-map (kbd "D") 'dotfiles/duplicate-region)

  ;; cycle numbers with C-a/x
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

  ;; add readline keys in insert state
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

  ;; add readline keys in ex state
  (define-key evil-ex-completion-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-ex-completion-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-ex-completion-map (kbd "C-h") 'backward-delete-char)

  ;; add C-u behaviour from Vim
  (define-key evil-insert-state-map (kbd "C-u") 'dotfiles/backward-kill-line)

  ;; use C-\ for Emacs state
  (global-set-key (kbd "C-\\") 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "C-\\") 'evil-exit-emacs-state)
  (define-key evil-motion-state-map (read-kbd-macro evil-toggle-key) nil)

  ;; use M-j/k to swap lines in insert state
  (define-key evil-normal-state-map (kbd "M-j") (concat ":m +1"))
  (define-key evil-normal-state-map (kbd "M-k") (concat ":m -2"))
  (define-key evil-visual-state-map (kbd "M-j") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "M-k") (concat ":m '<-2" (kbd "RET") "gv=gv"))

  ;; use M-h/j/k/l to move around in insert state
  (define-key evil-insert-state-map (kbd "M-h") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "M-j") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "M-k") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "M-l") 'evil-forward-char)

  ;; add window keys in evilified buffers
  (with-eval-after-load 'evil-evilified-state
    (define-key evil-evilified-state-map-original (kbd "C-w") 'evil-window-map)
    (define-key evil-evilified-state-map-original (kbd "C-h") 'evil-window-left)
    (define-key evil-evilified-state-map-original (kbd "C-j") 'evil-window-down)
    (define-key evil-evilified-state-map-original (kbd "C-k") 'evil-window-up)
    (define-key evil-evilified-state-map-original (kbd "C-l") 'evil-window-right))

  ;; add window keys in ediff
  (with-eval-after-load 'ediff
    (add-hook 'ediff-keymap-setup-hook
              (lambda ()
                (evil-define-key 'normal ediff-mode-map (kbd "C-j") 'evil-window-down)
                (evil-define-key 'normal ediff-mode-map (kbd "C-k") 'evil-window-up))))

  (with-eval-after-load 'helm
    ;; use C-f/b to scroll
    (define-key helm-map (kbd "C-f") 'helm-next-page)
    (define-key helm-map (kbd "C-b") 'helm-previous-page)
    (define-key helm-map (kbd "C-u") 'dotfiles/backward-kill-line)

    ;; use C-w to delete words instead of yanking text at point
    (define-key helm-map [remap helm-yank-text-at-point] 'backward-kill-word)
    (define-key helm-map [remap helm-swoop-yank-thing-at-point] 'backward-kill-word)

    ;; only run in emacs state once on C-\ in helm
    (define-key helm-map (kbd "C-\\") 'evil-execute-in-emacs-state))

  (with-eval-after-load 'helm-buffers
    ;; use M-d to kill buffers
    (define-key helm-buffer-map (kbd "M-d")
      (lambda () (interactive) (helm-kill-marked-buffers nil) (helm-refresh))))

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'backward-kill-word))

  (with-eval-after-load 'evil-lisp-state
    ;; use C-c/C-g to exit lisp state
    (define-key evil-lisp-state-map (kbd "C-g") 'evil-lisp-state/quit))

  (with-eval-after-load 'git-commit
    ;; start commit buffers in insert state
    (add-hook 'git-commit-mode-hook 'evil-insert-state))
)

(defun dotfiles-evil/init-simpleclip ()
  ;; don't use desktop clipboard for kill ring
  (use-package simpleclip
    :config
    (simpleclip-mode t))
)

(defun dotfiles-evil/post-init-simpleclip ()
  ;; paste with C-v
  (global-set-key (kbd "C-v") 'dotfiles/paste)
  (define-key evil-normal-state-map (kbd "C-v") 'dotfiles/paste)
  (define-key evil-insert-state-map (kbd "C-v") 'dotfiles/paste)
  (define-key evil-visual-state-map (kbd "C-v") 'dotfiles/paste)
  (define-key evil-ex-completion-map (kbd "C-v") 'dotfiles/paste)
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-v") 'dotfiles/paste))

  ;; use C-q for quoted insert / visual block state
  (global-set-key (kbd "C-q") 'quoted-insert)
  (define-key evil-insert-state-map (kbd "C-q") 'quoted-insert)
  (define-key evil-normal-state-map (kbd "C-q") 'evil-visual-block)

  ;; copy with C-c
  (add-hook 'dotfiles/escape-anywhere-hook 'dotfiles/copy)

  ;; cut with C-x in visual state
  (define-key evil-visual-state-map (kbd "C-x") 'dotfiles/cut)
)
