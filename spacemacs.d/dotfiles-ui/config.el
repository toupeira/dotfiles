;; interface settings
(setq linum-format "%5d")

;; customize theme
(setq theming-modifications
 '((monokai
    (default :background "#1C1C17")

    ;; modeline
    (spacemacs-normal-face :background "#A6E22E" :foreground "#344D05")
    (spacemacs-visual-face :background "#FD971F" :foreground "#663801")
    (spacemacs-insert-face :background "#66D9EF" :foreground "#1D5A66")

    ;; line numbers
    (linum :background "#12120F" :foreground "#45453A")
    (fringe :inherit 'linum :background nil)

    ;; visual selection
    (region :inherit nil :background "#005F87")
    (secondary-selection :inherit 'region :background nil)

    ;; cursorline
    (hl-line :background "#262620")
    (trailing-whitespace :background "#33332B")

    ;; search highlighting
    ; (isearch :background "#993991" :foreground "white" :bold t)
    ; (lazy-highlight :inherit nil :background "#662661" :foreground nil)
    ; (evil-search-highlight-persist-highlight-face :inherit 'lazy-highlight)
    (isearch :background "#6B9C0A" :foreground "white" :bold t)
    (lazy-highlight :inherit nil :background "#3D5906" :foreground nil)
    (evil-search-highlight-persist-highlight-face :inherit 'lazy-highlight)

    ;; match highlighting
    (sp-show-pair-match-face :background "#404036" :foreground nil :inverse-video nil :bold t)
    (sp-pair-overlay-face :background nil)

    ;; eval highlighting
    (eval-sexp-fu-flash :inherit 'highlight :background nil :foreground nil)

    ;; comments
    (font-lock-comment-face :foreground "#99937A")
    (font-lock-comment-delimiter-face :foreground "#99937A")
    (font-lock-doc-face :foreground "#40CAE4")

    ;; error symbols
    (flycheck-fringe-error :background nil :foreground "#F92672")
    (flycheck-fringe-warning :background nil :foreground "#E6DB74")
    (flycheck-fringe-info :background nil :foreground "#66D9EF")

    ;; org-mode
    (org-agenda-clocking :background "#00364C")
    (org-agenda-calendar-event :inherit 'org-drawer :foreground nil)
    (org-done :inherit 'org-special-keyword :foreground nil)
    (org-headline-done :inherit 'org-done :foreground nil :strike-through t :bold nil)
    (org-agenda-done :inherit 'org-headline-done :foreground nil :strike-through nil)
    (org-agenda-dimmed-todo-face :foreground nil :bold t)
    (org-verbatim :inherit 'font-lock-comment-face)
  )))
