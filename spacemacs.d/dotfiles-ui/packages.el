(setq dotfiles-ui-packages
 '(
   smooth-scrolling

   dracula-theme
   gotham-theme
   leuven-theme
   moe-theme
   molokai-theme
   monokai-theme
   subatomic-theme
  ))

(defun dotfiles-ui/post-init-smooth-scrolling ()
  (setq
   scroll-margin 5
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
  ))
