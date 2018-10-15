(require 'use-package)


(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'global-company-mode))

(use-package company-statistics
  :ensure t
  :defer t
  :init
  (add-hook 'global-company-mode-hook 'company-statistics-mode))


(use-package company-quickhelp
  :ensure t
  :defer t
  :init
  (add-hook 'global-company-mode-hook 'company-quickhelp-mode))


(provide 'module-completion)
