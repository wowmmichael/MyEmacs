(global-unset-key "\C-o")

(windmove-default-keybindings)

(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(setq indent-tabs-mode nil)
(show-paren-mode)
(electric-pair-mode)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

(defconst backup-dir
  (expand-file-name (concat "backups/" (user-real-login-name) "/")
                    user-emacs-directory)
  "Directory for Emacs backups.")

(defconst autosave-dir
  (expand-file-name (concat "autosaves/" (user-real-login-name) "/")
                    user-emacs-directory)
  "Directory for Emacs auto saves.")

(unless (file-directory-p backup-dir)
  (make-directory backup-dir t))

(unless (file-directory-p autosave-dir)
  (make-directory autosave-dir t))

(setq
 backup-directory-alist (list (cons "." backup-dir))
 auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(when *is-win*
  (setq exec-path (cons "c:/cygwin64/bin" exec-path)))

(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (progn (delete-trailing-whitespace)
           (untabify (point-min) (point-max)))))

(add-hook 'before-save-hook
          'my-prog-nuke-trailing-whitespace)

(provide 'init-customization)
