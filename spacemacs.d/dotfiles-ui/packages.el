(setq dotfiles-ui-packages
 '(
   projectile
   smooth-scrolling
  ))

(defun dotfiles-ui/post-init-smooth-scrolling ()
  (setq
   scroll-margin 5
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
  ))

;; show file and project in title
;; https://github.com/syl20bnr/spacemacs/pull/5924
(when dotfiles/is-gui
  (defun spacemacs//frame-title-format ()
    "Return frame title with current project name and path."
    (let* ((file (buffer-file-name))
           (path (if file (file-truename file) ""))
           (name (if file (file-name-nondirectory path) (buffer-name)))
           (directory (if file (s-chop-suffix "/" (file-name-directory path)) ""))

           (project (projectile-project-p))
           (directory (if (and project file)
                          (file-relative-name directory (projectile-project-root))
                        (abbreviate-file-name directory)))
           (directory (if (string= "." directory) "" directory)))
      (format "%s [%s%s%s]"
              name
              (if project (projectile-project-name) dotspacemacs-default-layout-name)
              (if (not (string= "" directory)) ":" "")
              directory)))

  (defun dotfiles-ui/post-init-projectile ()
    (setq frame-title-format '((:eval (spacemacs//frame-title-format)))))
)
