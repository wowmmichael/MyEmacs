(require 'use-package)
(require 'platform-utils)

;;; require package in MyEmacs/elisp
(use-package sys-utils
  :bind (("C-o C-s" . su/start-cmd)
         ("C-o C-e" . su/start-explorer)))

(use-package edit-utils
  :bind (("<M-up>" . eu/swap-line-up)
         ("<M-down>" . eu/swap-line-down)
         ("C-o f" . eu/indent-buffer)
         ("C-o h" . eu/collapse-around)))

(use-package buffer-utils
  :bind (("C-o b" . buffer-utils/generate-buffer)))

(use-package js-utils)

(use-package esup
  :defer t
  :ensure t)

(use-package spaceline
  :defer 1
  :ensure t
  :config
  (progn (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
         (if (boundp 'ns-use-srgb-colorspace)
             (setq ns-use-srgb-colorspace nil))))

(use-package spaceline-config
  :defer 1
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme))


(use-package sublime-themes
  :ensure t
  :demand
  :config
  (load-theme 'granger t))

(use-package s
  :ensure t
  :demand)

(use-package magit
  :defer t
  :ensure t
  :bind (("C-o g" . magit-status))
  :config
  (progn (setq magit-commit-show-diff nil)
         (setq magit-revert-buffers 1)
         (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)))

(use-package helm
  :defer t
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x l" . helm-occur)
         ("C-?" . helm-apropos)
         ("C-o s" . helm-do-grep-ag)))


(use-package helm-projectile
  :defer t
  :ensure t
  :bind (("C-o o" . helm-projectile-find-file-dwim)))


(use-package helm-ag
  :defer t
  :ensure t
  :bind (("C-o s" . helm-ag-project-root)))

(use-package window-purpose
  :defer t
  :ensure t
  :config
  (progn (setq-default purpose-preferred-prompt 'helm)
         (define-key purpose-mode-map (kbd "C-x b") nil)
         (define-key purpose-mode-map (kbd "C-x C-f") nil)
         (setq-default purpose-user-regexp-purposes (quote (("^ ?\\*.*\\* ?$" . stars))))
         (purpose-compile-user-configuration)
         ))

(use-package shackle
  :defer t
  :ensure t
  :config
  (progn (setq helm-display-function 'pop-to-buffer) ; make helm play nice
         (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.35)))
         (shackle-mode)))

(use-package neotree
  :defer t
  :ensure t
  :config
  (progn (setq-default neo-autorefresh nil))
  :bind (("C-o n f" . neotree-find)))

(global-set-key (kbd "C-o !") (lambda ()
                                (interactive)
                                (projectile-mode)
                                (helm-projectile-on)
                                (neotree-toggle)
                                (purpose-mode)))

(use-package expand-region
  :defer t
  :ensure t
  :bind (("C-x =" . er/expand-region)))

(use-package ace-jump-mode
  :defer t
  :ensure t
  :bind (("M-'" . ace-jump-mode)))

(use-package multiple-cursors
  :defer t
  :ensure t
  :bind (("C-m a" . mc/mark-all-like-this)
         ("C-m m" . mc/mark-all-dwim)
         ("C-m n" . mc/mark-next-like-this)
         ("C-m u" . mc/mark-pop)
         ("C-m N" . mc/unmark-next-like-this)
         ("C-m l" . mc/edit-lines)))

(use-package ace-mc
  :defer t
  :ensure t
  :bind (("C-m '" . ace-mc-add-multiple-cursors)))

(use-package ace-jump-zap
  :defer t
  :ensure t
  :bind (("C-o z" . ace-jump-zap-to-char)))


(use-package company
  :defer t
  :ensure t)

(use-package company-statistics
  :defer t
  :ensure t
  :config
  (company-statistics-mode))

(use-package buffer-move
  :defer t
  :ensure t
  :bind (("<M-S-up>" . buf-move-up)
         ("<M-S-down>" . buf-move-down)
         ("<M-S-left>" . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

(use-package yasnippet
  :defer t
  :ensure t
  :config
  (yas-global-mode t))

(use-package paredit
  :defer t
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t
  :mode "\\.md$")

(use-package web-mode
  :defer t
  :ensure t
  :mode "\\.html?$")

(use-package sass-mode
  :defer t
  :ensure t
  :mode "\\.scss$"
  :config
  (setq sass-indent-offset 4))

(use-package js2-mode
  :defer t
  :ensure t
  :mode "\\.js$"
  :config
  (progn (add-hook 'js2-mode-hook (lambda () (js-utils/update-flycheck-javascript-eslint-executable)))
         (setq-default js2-basic-offset 2)))

(use-package jade-mode
  :defer t
  :ensure t
  :mode "\\.jade$")


(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json$")


(use-package haskell-mode
  :defer t
  :ensure t
  :config
  (progn (add-hook 'haskell-mode-hook
                   (lambda ()
                     (set (make-local-variable 'company-backends)
                          (append '((company-capf company-dabbrev-code))
                                  company-backends))))))

(use-package ghc
  :defer t
  :ensure t
  :config
  (progn (autoload 'ghc-init "ghc" nil t)
         (autoload 'ghc-debug "ghc" nil t)
         (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))

(use-package elpy
  :defer t
  :ensure t
  :config
  (progn (elpy-enable)
         (add-hook 'python-mode-hook
                   (lambda ()
                     (setq indent-tabs-mode nil)
                     (setq tab-width 4)
                     (setq python-indent 4)))))


(use-package flycheck-haskell
  :defer t
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))


(use-package flycheck
  :defer t
  :ensure t
  :config
  (progn (setq-default flycheck-temp-prefix ".flycheck")
         (setq-default flycheck-emacs-lisp-load-path 'inherit)
         (setq-default flycheck-disabled-checkers
                       (append flycheck-disabled-checkers
                               '(emacs-lisp-checkdoc
                                 json-jsonlist)))
         (flycheck-add-mode 'javascript-eslint 'web-mode)
         (setq-default flycheck-temp-prefix ".flycheck")
         (global-flycheck-mode)))



(when (platform-utils/is-win-p)
  (use-package powershell
    :defer t
    :ensure t
    :config
    (progn (add-to-list 'auto-mode-alist '("\\.psm1$" . powershell-mode))
           (add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode)))))



(provide 'init-package)
