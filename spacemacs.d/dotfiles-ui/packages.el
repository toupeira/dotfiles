(setq dotfiles-ui-packages
 '(
   flycheck
   persp-mode
   projectile
   smooth-scrolling
   which-key
   zone-nyan
  ))

(defun dotfiles-ui/post-init-persp-mode ()
  (advice-add 'find-file :after 'dotfiles/switch-to-project-layout)
  ;; (def-auto-persp "projectile"
  ;;   :parameters '((dont-save-to-file . t))
  ;;   ;; :hooks '(after-change-major-mode-hook)
  ;;   :switch 'frame
  ;;   :predicate
  ;;   (lambda (buffer)
  ;;     (with-current-buffer buffer
  ;;       (and
  ;;        (buffer-file-name)
  ;;        (projectile-project-p))))
  ;;   :get-name-expr
  ;;   (lambda ()
  ;;     (if (string-prefix-p dotfiles/directory (file-truename (buffer-file-name (current-buffer))))
  ;;         dotfiles/directory
  ;;       (abbreviate-file-name (projectile-project-root)))))
  ;;   ;; :after-match
  ;;   ;; (lambda (persp-name persp buffer &rest args)
  ;;   ;;   (let ((old-persp (get-current-persp)))
  ;;   ;;     (when (and old-persp (not (string= persp-name (persp-name old-persp))))
  ;;   ;;       (message (format "Removing buffer '%s' from layout '%s'"
  ;;   ;;                        (buffer-name buffer) (persp-name old-persp)))
  ;;   ;;       (persp-remove-buffer buffer old-persp)))
  ;;   ;;   (persp-frame-switch persp-name)))

  ;; always use default layout for home screen
  (advice-add 'spacemacs/home :before
              (lambda () (persp-switch dotspacemacs-default-layout-name)))

  ;; always use org layout for agenda views
  (advice-add 'org-agenda-prepare-window :before
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

(defun dotfiles-ui/init-zone-nyan ()
  (use-package zone-nyan :defer t))
