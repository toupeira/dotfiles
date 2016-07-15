(setq dotfiles-ui-packages
 '(
   flycheck
   persp-mode
   projectile
   smooth-scrolling
   spaceline
   which-key

   zone-nyan
  ))

(defun dotfiles-ui/post-init-persp-mode ()
  ;; (advice-add 'find-file :after 'dotfiles/switch-to-project-layout)

  ;; find-file-hook also triggers on find-file-noselect,
  ;; after-find-file fits better but doesn't provide its own hook
  (defvar after-find-file-hook nil)
  (advice-add 'find-file :after (lambda (&rest args) (run-hooks 'after-find-file-hook)))

  ;; https://github.com/Bad-ptr/persp-mode.el/issues/40 
  (def-auto-persp "projectile"
    :parameters '((dont-save-to-file . t))
    :hooks '(after-find-file-hook server-visit-hook)
    :switch 'frame
    :predicate
    (lambda (buffer)
      (when (and (buffer-file-name)
                 (projectile-project-p))
        (switch-to-buffer (other-buffer))
        t))
    :get-name-expr
    (lambda ()
      (if (string-prefix-p dotfiles/directory (file-truename (buffer-file-name (current-buffer))))
          dotfiles/directory
        (abbreviate-file-name (projectile-project-root)))))

  ;; automatically use default layout for home screen
  (advice-add 'spacemacs/home :before
              (lambda () (persp-switch dotspacemacs-default-layout-name)))

  ;; automatically use org layout
  (advice-add 'org-agenda-prepare-window :before
              (lambda (&rest args) (persp-switch org-directory)))
  (advice-add 'org-clock-goto :before
              (lambda (&rest args) (persp-switch org-directory)))

  ;; create default layouts
  (add-hook 'persp-mode-hook 'dotfiles/startup))

(defun dotfiles-ui/post-init-flycheck ()
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  (spacemacs/add-flycheck-hook 'lisp-mode-hook)
  (spacemacs/add-flycheck-hook 'shell-mode-hook)

  (add-hook 'flycheck-after-syntax-check-hook 'dotfiles/auto-list-errors)
  (advice-add 'flycheck-error-list-refresh :after 'dotfiles/auto-resize-errors))

(defun dotfiles-ui/post-init-projectile ()
  (setq frame-title-format '((:eval (dotfiles/frame-title-format)))))

(defun dotfiles-ui/post-init-smooth-scrolling ()
  (setq
   scroll-margin 5
   mouse-wheel-scroll-amount '(1 ((shift) . 1)))

  ;; disable scroll margin for shell buffers
  (add-hook 'term-mode-hook (lambda () (setq-local scroll-margin 0)))
  (add-hook 'eshell-mode-hook (lambda () (setq-local scroll-margin 0))))

;; hide dotfiles/ prefix in which-key
(defun dotfiles-ui/post-init-which-key ()
  (push '("^dotfiles/\\(.+\\\)" . "\\1") which-key-description-replacement-alist))

;; trim down modeline
(defun dotfiles-ui/post-init-spaceline ()
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-size-off))

(defun dotfiles-ui/init-zone-nyan ()
  (use-package zone-nyan :defer t))
