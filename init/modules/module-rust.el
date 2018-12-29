(require 'use-package)

(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs\\'"
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . flycheck-rust-setup))
  :config
  (bind-key (kbd "TAB") #'company-indent-or-complete-common rust-mode-map)
  (setq company-tooltip-align-annotations t))

(use-package cargo
  :ensure t
  :defer t)

(use-package racer
  :ensure t
  :defer t
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package flycheck-rust
  :ensure t
  :defer t)

(provide 'module-rust)
