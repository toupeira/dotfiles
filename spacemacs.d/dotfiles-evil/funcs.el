;; save files and kill frame
(defun dotfiles/prompt-frame-killer ()
  (interactive)
  (save-some-buffers)
  (spacemacs/frame-killer))

;; copy to desktop clipboard
(defun dotfiles/copy ()
  (interactive)
  (when (and evil-mode (eq evil-state 'visual))
    (evil-yank evil-visual-beginning evil-visual-end (evil-visual-type) ?+)))

;; cut to desktop clipboard
(defun dotfiles/cut ()
  (interactive)
  (when (and evil-mode (eq evil-state 'visual))
    (evil-delete evil-visual-beginning evil-visual-end (evil-visual-type) ?+)))

;; paste from desktop clipboard
(defun dotfiles/paste ()
  (interactive)
  (when (and evil-mode (eq (evil-visual-type) 'line))
    (evil-digit-argument-or-evil-beginning-of-line))
  (if evil-mode
    (evil-paste-from-register ?+)
    (simpleclip-paste)))

;; escape wrapper that should work anywhere
;; https://www.emacswiki.org/emacs/evil#toc16
(defvar dotfiles/escape-anywhere-hook nil)
(defun dotfiles/escape-anywhere (prompt)
  (run-hooks 'dotfiles/escape-anywhere-hook)

  (cond
  ;; return Escape in Evil modes
  ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
  ;; return C-c in Emacs mode
  ((evil-emacs-state-p) (kbd "C-c"))
  ;; return C-g in other modes
  (t (kbd "C-g"))))

;; show buffer name
(defun dotfiles/identify-buffer ()
  (interactive)
  (message
   (concat
    (or (buffer-file-name) (buffer-name))
    (or (and (projectile-project-p) (format " [%s]" (projectile-project-name))) "")
    )))

;; kill to beginning of line, as in Vim
;; TODO: try deleting only newly entered characters first
;; TODO: submit upstream bug report
(defun dotfiles/backward-kill-line ()
  (interactive)
  (let ((end (point)))
    (if (= (point) (progn (back-to-indentation) (point)))
        (beginning-of-line))
    (delete-region (point) end)))

;; duplicate selected region
(defun dotfiles/duplicate-region ()
  (interactive)
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))
