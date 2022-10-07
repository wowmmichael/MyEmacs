(require 'use-package)
(require 'platform-utils)

;;; require package in MyEmacs/elisp
(use-package sys-utils
  :bind (("C-o C-s" . su/start-cmd)
         ("C-o C-e" . su/start-explorer)))

(use-package buffer-utils
  :bind (("C-o b" . buffer-utils/generate-buffer)))

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
  (setq magit-commit-show-diff nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

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


(use-package helm-rg
  :defer t
  :ensure t
  :bind (("C-o s" . helm-rg)))


(require 'module-window)

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


(require 'module-edit-utils)
(require 'module-completion)


;; disable as it is incompatible with org-mode
;;
;; (use-package yasnippet
;;   :defer t
;;   :ensure t
;;   :config
;;   (yas-global-mode t))
;;
;; (use-package yasnippet-snippets
;;   :defer t
;;   :ensure t)

(use-package paredit
  :defer t
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t
  :mode "\\.md$")

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json$")

(when (platform-utils/is-win-p)
  (use-package powershell
    :defer t
    :ensure t
    :config
    (progn (add-to-list 'auto-mode-alist '("\\.psm1$" . powershell-mode))
           (add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode)))))

(when (platform-utils/is-osx-p)
  (use-package osx-dictionary
    :defer t
    :ensure t
    :bind (("C-d ?" . osx-dictionary-search-word-at-point))))

(require 'module-org)
(require 'module-flycheck)

(provide 'init-package)
