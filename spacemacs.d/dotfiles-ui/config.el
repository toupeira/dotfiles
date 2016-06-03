;; interface settings
(setq linum-format "%5d")

;; set default size of new windows
(unless dotfiles/is-ocelot
  (add-to-list 'default-frame-alist '(width  . 120))
  (add-to-list 'default-frame-alist '(height . 60))
)

;; show file and project in title
;; https://github.com/syl20bnr/spacemacs/pull/5924
(when dotfiles/is-gui
  (defun spacemacs//frame-title-format ()
    "Return frame title with current project name, where applicable."
    (let ((file (buffer-file-name)))
      (cond
       ((eq nil file) "%b")
       ((and (bound-and-true-p projectile-mode)
             (projectile-project-p))
        (format "%s [%s:%s]"
                (file-name-nondirectory file)
                (projectile-project-name)
                (substring (file-name-directory file) (length (projectile-project-root)))
                ))
       (t (format "%s [%s]"
                  (file-name-nondirectory file)
                  (file-name-directory file)
                  ))
        )))

  (setq frame-title-format '((:eval (spacemacs//frame-title-format))))
)

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
    (region :inherit nil :background "#005F87" :bold t)

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
  )))
