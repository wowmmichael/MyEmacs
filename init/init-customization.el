(global-unset-key "\C-o")

(windmove-default-keybindings)

(set-scroll-bar-mode nil)

(setq indent-tabs-mode nil)
(show-paren-mode)
(electric-pair-mode)

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

(provide 'init-customization)
