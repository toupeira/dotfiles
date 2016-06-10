(setq dotfiles-ui-packages
 '(
   flycheck
   persp-mode
   projectile
   smooth-scrolling
  ))

(defun dotfiles-ui/post-init-persp-mode ()
  (advice-add 'find-file :after 'dotfiles/switch-to-project-layout)
  ;; (def-auto-persp "projectile"
  ;;   :parameters '((dont-save-to-file . t))
  ;;   :get-name-expr
  ;;   (lambda ()
  ;;     (if (string-prefix-p "/etc/dotfiles/" (file-truename (buffer-file-name buffer)))
  ;;         "/etc/dotfiles/"
  ;;       (projectile-project-root)))
  ;;   :predicate
  ;;   (lambda (foo)
  ;;     (message "%s" foo)
  ;;     (projectile-project-p)))

  ;; always use default layout for home screen
  (advice-add
   'spacemacs/home :before
   (lambda () (persp-switch dotspacemacs-default-layout-name)))

  ;; always use dotfiles layout for init.el
  (advice-add
   'spacemacs/find-dotfile :before
   (lambda () (persp-switch "/etc/dotfiles/")))

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
