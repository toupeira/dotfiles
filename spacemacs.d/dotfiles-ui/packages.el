(setq dotfiles-ui-packages
 '(
   smooth-scrolling
  ))

(defun dotfiles-ui/post-init-smooth-scrolling ()
  (setq
   scroll-margin 5
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
  ))
