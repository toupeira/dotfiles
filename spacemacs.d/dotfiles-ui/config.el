;; interface settings
(setq linum-format "%5d ")

;; set default size of new windows
(add-to-list 'default-frame-alist '(width  . 120))
(add-to-list 'default-frame-alist '(height . 60))

;; show file and project in title
;; https://github.com/syl20bnr/spacemacs/pull/5924
(defun spacemacs//frame-title-format ()
  "Return frame title with current project name, where applicable."
  (let ((file buffer-file-name))
    (concat "emacs: "
      (cond
      ((eq nil file) "%b")
      ((and (bound-and-true-p projectile-mode)
            (projectile-project-p))
        (concat (substring file (length (projectile-project-root)))
                (format " [%s]" (projectile-project-name))))
      (t (abbreviate-file-name file))))))

(when dotfiles/is-gui
  (setq frame-title-format '((:eval (spacemacs//frame-title-format)))))

;; customize theme
(setq theming-modifications
 '((monokai
    ;; modeline
    (spacemacs-normal-face :background "#A6E22E" :foreground "#344D05")
    (spacemacs-visual-face :background "#FD971F" :foreground "#663801")
    (spacemacs-insert-face :background "#66D9EF" :foreground "#1D5A66")

    ;; line numbers
    (linum :background "#12120F" :foreground "#45453A")

    ;; visual selection
    ;; (region :inherit nil :background "#0E3436")
    (region :inherit nil :background "#000" :bold t)

    ;; cursorline
    (hl-line :background "#33332B")
    (trailing-whitespace :background "#404035")

    ;; search highlighting
    (isearch :background "#D3FBF6" :foreground "black" :bold t)
    (lazy-highlight :background "#74DBCD" :foreground "black")
    (evil-search-highlight-persist-highlight-face
      :background "#74DBCD" :foreground "black")

    ;; comments
    (font-lock-comment-face :foreground "#99937A")
    (font-lock-comment-delimiter-face :foreground "#99937A")
    (font-lock-doc-face :foreground "#40CAE4")

    ;; error symbols
    (flycheck-fringe-error :background nil :foreground "#F92672")
    (flycheck-fringe-warning :background nil :foreground "#E6DB74")
    (flycheck-fringe-info :background nil :foreground "#66D9EF")

    ;; org-mode
    (org-agenda-clocking :background "#294C48")
    (org-agenda-calendar-event :inherit 'org-drawer :foreground nil)
  )))

(configuration-layer/declare-layer
  '(theming
    :variables
    theming-headings-inherit-from-default 'all
    theming-headings-same-size 'all
    theming-headings-bold 'all))
