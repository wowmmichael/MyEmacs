(require 'use-package)


(use-package projectile
  :ensure t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  )

(use-package helm-rg
  :ensure t
  :after (helm)
  :requires (helm)
  :bind (:map
         stephenwan/operation-keys-map
              ("s" . helm-rg)))


;; Ref: https://github.com/bbatsov/helm-projectile
(use-package helm-projectile
  :ensure t
  :after (helm)
  :requires (helm)
  :bind (:map stephenwan/operation-keys-map
              ("o" . helm-projectile-find-file-dwim)))


(use-package neotree
  :ensure t
  :defer t
  :config
  (setq-default neo-autorefresh nil)
  :bind (:map stephenwan/operation-keys-map
              ("f" . neotree-find)))




(provide 'module-proj-explore)
;;; module-proj-explore.el ends here
