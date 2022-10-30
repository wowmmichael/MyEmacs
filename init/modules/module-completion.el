(require 'use-package)


(use-package company
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook #'global-company-mode))


(use-package company-statistics
  :ensure t
  :hook (global-company-mode . company-statistics-mode))


(use-package company-quickhelp
  :ensure t
  :hook (global-company-mode . company-quickhelp-mode))


(provide 'module-completion)
