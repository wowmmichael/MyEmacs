(require 'use-package)

;; flycheck is a better replacement for flymake.

(use-package flycheck
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(provide 'module-flycheck)
