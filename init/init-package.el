(require 'use-package)

(defvar my-ensured-packages
  '(
    use-package
    s
    sublime-themes
    magit
    helm

    ;; project-explorer
    neotree

    projectile
    helm-projectile
    expand-region

    ace-jump-mode
    ace-jump-zap
    ace-mc

    multiple-cursors
    ;; auto-complete
    company
    company-statistics

    buffer-move
    yasnippet
    paredit

    markdown-mode
    web-mode
    js2-mode
    js2-refactor
    xref-js2 ;; depends on the_silver_searcher
    rjsx-mode

    sass-mode
    jade-mode

    json-mode

    ;; tide
    haskell-mode
    elpy
    sphinx-doc

    flycheck

    powershell ;; if on windows
    ))


;;; require package in MyEmacs/elisp
(use-package sys-utils
  :bind (("C-o C-s" . su/start-cmd)
         ("C-o C-e" . su/start-explorer)))

(use-package edit-utils
  :bind (("<M-up>" . eu/swap-line-up)
         ("<M-down>" . eu/swap-line-down)
         ("C-o f" . eu/indent-buffer)
         ("C-o h" . eu/collapse-around)))

(use-package js-utils)


(use-package spaceline
  :ensure t
  :demand
  :config
  (progn (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
         (if (boundp 'ns-use-srgb-colorspace)
             (setq ns-use-srgb-colorspace nil))))

(use-package spaceline-config :ensure spaceline
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
  :ensure t
  :bind (("C-o g" . magit-status))
  :config
  (progn (setq magit-commit-show-diff nil)
         (setq magit-revert-buffers 1)))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x l" . helm-occur)))

(use-package helm-projectile
  :ensure t
  :bind (("C-o p" . helm-projectile-on)
         ("C-o o" . helm-projectile-find-file-dwim)))

(use-package neotree
  :ensure t
  :config
  (progn (setq-default neo-autorefresh nil))
  :bind (("C-o e" . neotree-toggle)
         ("C-o n f" . neotree-find)))

(use-package expand-region
  :ensure t
  :bind (("C-x =" . er/expand-region)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-t" . ace-jump-mode)))

(use-package multiple-cursors
  :ensure t)

(use-package ace-mc
  :ensure t
  :bind (("C-o m" . ace-mc-add-multiple-cursors)))

(use-package ace-jump-zap
  :ensure t
  :bind (("C-o z" . ace-jump-zap-to-char)))


;; (use-package auto-complete
;;   :ensure t
;;   :config
;;   (global-auto-complete-mode))

(use-package company
  :ensure t
  :bind (("C-o SPC" . company-dabbrev-code))
  :config
  (progn (global-company-mode)
         (setq company-backends
               '((company-dabbrev-code company-keywords company-capf :seperate)
                 (company-abbrev :seperate company-dabbrev)
                 (company-files
                  company-yasnippet)))
         (add-hook 'lisp-interaction-mode-hook
                   (lambda()
                     (add-to-list (make-local-variable 'company-backends)
                                  'company-elisp)))
         (custom-set-variables
          '(company-dabbrev-minimum-length 2)
          '(company-minimum-prefix-length 2))))

(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode))

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

(use-package markdown-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html?$")

(use-package sass-mode
  :ensure t
  :mode "\\.scss$"
  :config
  (setq sass-indent-offset 4))

(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :config
  (progn (add-hook 'js2-mode-hook (lambda () (js-utils/update-flycheck-javascript-eslint-executable)))))

(use-package js2-refactor
  :ensure t
  :config
  (progn (add-hook 'js2-mode-hook #'js2-refactor-mode)
         (js2r-add-keybindings-with-prefix "C-c C-j")
         (define-key js2-mode-map (kbd "C-k") #'js2r-kill)))

(use-package xref-js2
  :ensure t
  :config
  (progn (define-key js-mode-map (kbd "M-.") nil)
         (add-hook 'js2-mode-hook (lambda ()
                                    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))

(use-package rjsx-mode
  :ensure t)

(use-package jade-mode
  :ensure t
  :mode "\\.jade$")


(use-package json-mode
  :ensure t
  :mode "\\.json$")


(use-package haskell-mode
  :ensure t
  :config
  (progn (add-hook 'haskell-mode-hook
                   (lambda ()
                     (set (make-local-variable 'company-backends)
                          (append '((company-capf company-dabbrev-code))
                                  company-backends))))))

(use-package ghc
  :ensure t
  :config
  (progn (autoload 'ghc-init "ghc" nil t)
         (autoload 'ghc-debug "ghc" nil t)
         (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))

(use-package elpy
  :ensure t
  :config
  (progn (elpy-enable)
         (add-hook 'python-mode-hook
                   (lambda ()
                     (setq indent-tabs-mode nil)
                     (setq tab-width 4)
                     (setq python-indent 4)))))

(use-package sphinx-doc
  :ensure t)

(use-package flycheck-haskell
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))


(use-package flycheck
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



(when *is-win*
  (use-package powershell
    :ensure t
    :config
    (progn (add-to-list 'auto-mode-alist '("\\.psm1$" . powershell-mode))
           (add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode)))))



(provide 'init-package)
