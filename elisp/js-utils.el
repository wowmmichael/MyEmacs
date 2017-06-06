(defun joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))


(defun js-utils/project-eslint-path ()
  (if (fboundp 'projectile-project-root)
      (let* ((project-path (projectile-project-root))
             (eslint-path (joindirs project-path "node_modules" ".bin" "eslint")))
        (if (file-exists-p eslint-path) eslint-path))))


(defun js-utils/update-flycheck-javascript-eslint-executable ()
  (interactive)
  (if (and (fboundp 'flycheck-set-checker-executable) (boundp 'flycheck-enabled-checkers) (fboundp 'flycheck-disable-checker))
      (let ((project-eslint (js-utils/project-eslint-path)))
        (if project-eslint
            (progn (flycheck-set-checker-executable 'javascript-eslint project-eslint)
                   (flycheck-disable-checker 'javascript-eslint t)
                   (if (boundp 'js2-mode-show-parse-errors)
                       (setq js2-mode-show-parse-errors nil))
                   (if (boundp 'js2-mode-show-strict-warnings)
                       (setq js2-mode-show-strict-warnings nil))
                   (message "flycheck-javascript-eslint has been configured."))))))


(provide 'js-utils)
