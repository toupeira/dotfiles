;; copy to desktop clipboard
(defun dotfiles/copy ()
  (interactive)
  (when (and evil-mode (eq evil-state 'visual))
    (evil-visual-expand-region)
    (simpleclip-copy evil-visual-beginning evil-visual-end)))

;; paste from desktop clipboard
(defun dotfiles/paste ()
  (interactive)
  (simpleclip-paste))

;; escape from anywhere
;; https://www.emacswiki.org/emacs/Evil#toc16
(defvar dotfiles/escape-anywhere-hook nil)
(defun dotfiles/escape-anywhere (prompt)
  (run-hooks 'dotfiles/escape-anywhere-hook)

  (cond
  ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
  ;; Key Lookup will use it.
  ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
  ;; This is the best way I could infer for now to have C-c work during evil-read-key.
  ;; Note: As long as I return [escape] in normal-state, I don't need this.
  ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
  (t (kbd "C-g"))))

;; show buffer name
(defun dotfiles/identify-buffer ()
  (interactive)
  (message "%s" (or (buffer-file-name) (buffer-name))))

;; kill to beginning of line, as in Vim
;; TODO: try deleting only newly entered characters first
;; TODO: submit upstream bug report
(defun dotfiles/backward-kill-line ()
  (interactive)
  (let ((end (point)))
    (evil-beginning-of-line)
    (unless (looking-at "[[:space:]]*$")
      (evil-first-non-blank))
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
