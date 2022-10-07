(require 'use-package)

;; flycheck is a better replacement for flymake.

(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-grammarly
  :ensure t
  :defer t
  :init
  (progn (setq flycheck-grammarly-check-time 0.8)))

(provide 'module-flycheck)
