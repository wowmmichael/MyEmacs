(require 'platform-utils)
(require 'resolve-terminal-key)

(global-unset-key "\C-o")
(global-unset-key "\C-t")
(global-unset-key "\C-d")

(if (display-graphic-p)
    (progn
      (if (fboundp 'tool-bar-mode)
          (tool-bar-mode -1))
      (if (fboundp 'scroll-bar-mode)
          (scroll-bar-mode -1)))
  (resolve-terminal-key/iterm2))

(setq indent-tabs-mode nil)
(show-paren-mode)
(electric-pair-mode)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
(add-to-list 'default-frame-alist '(background-color . "black"))

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

(when (platform-utils/is-win-p)
  (setq exec-path (cons "c:/cygwin64/bin" exec-path)))

(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (progn (delete-trailing-whitespace)
           (untabify (point-min) (point-max)))))

(add-hook 'before-save-hook
          'my-prog-nuke-trailing-whitespace)

(setq inhibit-compacting-font-caches t)
(setq inhibit-startup-screen t)

(provide 'init-customization)
