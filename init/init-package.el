(require 'use-package)
(require 'platform-utils)


;; refer to 'https://jwiegley.github.io/use-package/keywords/' for the meaning of the use-package keywords


(bind-keys* :prefix-map stephenwan/speed-keys-map :prefix "<f2>")
(bind-keys* :prefix-map stephenwan/operation-keys-map :prefix "C-o")
(bind-keys* :prefix-map stephenwan/mc-keys-map :prefix "M-m")


(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x l" . helm-occur)
         ("C-x ?" . helm-apropos)))

(use-package esup
  :defer t
  :ensure t)

(use-package ts
  :ensure t
  :demand)

(use-package tzc
  :ensure t
  :demand)


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

;; (use-package s
;;   :ensure t
;;   :demand)

(use-package magit
  :defer t
  :ensure t
  :bind (("C-o g" . magit-status))
  :config
  (setq magit-commit-show-diff nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))


(use-package which-key
  :commands which-key-mode
  :defer t
  :ensure t
  :config
  (which-key-mode))


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
  :ensure t
  :hook (emacs-lisp-mode))

(use-package markdown-mode
  :ensure t
  :mode "\\.md$")

(use-package json-mode
  :ensure t
  :mode "\\.json$")

(when (platform-utils/is-win-p)
  (use-package powershell
    :ensure t
    :config
    (progn (add-to-list 'auto-mode-alist '("\\.psm1$" . powershell-mode))
           (add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode)))))

(when (platform-utils/is-osx-p)
  (use-package osx-dictionary
    :defer t
    :ensure t
    :bind (:map stephenwan/general-keys-map
           ("?" . osx-dictionary-search-word-at-point))))



(require 'module-window)
(require 'module-proj-explore)
(require 'module-edit-utils)
(require 'module-completion)
(require 'module-org)
(require 'module-flycheck)

(provide 'init-package)
