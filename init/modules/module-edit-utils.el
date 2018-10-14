(require 'use-package)

(bind-keys* :prefix-map stephenwan/mc-keys-map
           :prefix "M-m")

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("M-0" . mc/mark-next-like-this)
         ("M--" . mc/skip-to-next-like-this)
         ("M-_" . mc/unmark-next-like-this)
         :map stephenwan/mc-keys-map
              ("M-m" . mc/mark-all-dwim)
              ("a" . mc/mark-all-like-this)
              ("r" . mc/mark-all-in-region)
              ("l" . mc/edit-lines)
              ("p" . mc/mark-pop)))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("M-\\" . ace-jump-mode)
         ("M-|" . ace-jump-line-mode)))

(use-package ace-mc
  :ensure t
  :defer t
  :bind (:map stephenwan/mc-keys-map
              ("M-\\" . ace-mc-add-multiple-cursors)))

(use-package expand-region
  :defer t
  :ensure t
  :bind (("M-=" . er/expand-region)))

(use-package edit-utils
  :bind (("<M-up>" . eu/swap-line-up)
         ("<M-down>" . eu/swap-line-down)
         ("C-o f" . eu/indent-buffer)
         ("C-o h" . eu/collapse-around)))

(provide 'module-edit-utils)
