(require 'use-package)

(defvar my-ensured-packages
  '(
    use-package
    sublime-themes
    magit
    helm
    project-explorer
    projectile
    expand-region
    ace-jump-mode
    multiple-cursors
    auto-complete
    buffer-move
    yasnippet
    paredit

    web-mode
    js2-mode
    sass-mode
    jade-mode
    flycheck

	powershell ;; if on windows
    ))

(use-package sublime-themes
  :ensure t
  :demand
  :config
  (load-theme 'granger t))

(use-package magit
  :ensure t
  :bind (("C-o g" . magit-status)))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x l" . helm-occur)
         ("C-x C-o" . project-explorer-helm)))

(use-package project-explorer
  :ensure t
  :bind (("C-o e" . project-explorer-open))
  :commands (project-explorer-helm)
  :config
  (use-package projectile))

(use-package expand-region
  :ensure t
  :bind (("C-x =" . er/expand-region)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-x ?" . ace-jump-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-o n" . mc/mark-next-like-this)
         ("C-o p" . mc/mark-previous-like-this)
         ("C-o |" . mc/edit-lines)
         ("C-o a" . mc/mark-all-like-this)
         ("C-o r" . mc/set-rectangular-region-anchor)))

(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode))

(use-package buffer-move
  :ensure t
  :bind (("<M-S-up>" . buf-move-up)
         ("<M-S-down>" . buf-move-down)
         ("<M-S-left>" . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package paredit
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html?$")

(use-package sass-mode
  :ensure t
  :mode "\\.scss$")

(use-package js2-mode
  :ensure t
  :mode "\\.js$")

(use-package jade-mode
  :ensure t
  :mode "\\.jade$")

(use-package flycheck
  :ensure t
  :config
  (progn (global-flycheck-mode)
         (setq-default flycheck-temp-prefix ".flycheck")
	 (setq-default flycheck-emacs-lisp-load-path 'inherit)
         (setq-default flycheck-disabled-checkers
                       (append flycheck-disabled-checkers
			       '(emacs-lisp-checkdoc
				 json-jsonlist
				 javascript-jshint)))))

(when *is-win*
  (use-package powershell
			   :ensure t
			   :mode ".\\psm1$" ".\\ps1$"))


;;; require package in MyEmacs/elisp
(use-package sys-utils
  :bind (("C-x C-s" . su/start-cmd)))

(use-package edit-utils
  :bind (("<M-up>" . eu/swap-line-up)
	 ("<M-down>" . eu/swap-line-down)
	 ("C-o h" . eu/collapse-around)))


(provide 'init-package)
