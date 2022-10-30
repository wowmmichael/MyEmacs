(require 'use-package)
(require 'edit-utils)


(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (:map stephenwan/mc-keys-map
              ("M-m" . mc/mark-all-dwim)
              ("a" . mc/mark-all-like-this)
              ("r" . mc/mark-all-in-region)
              ("l" . mc/edit-lines)
              ("p" . mc/mark-pop)
              ("n" . mc/mark-next-like-this)
              ("s" . mc/skip-to-next-like-this)
              ("d" . mc/unmark-next-like-this)))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (:map stephenwan/speed-keys-map
              ("<f2>" . ace-jump-mode)
              ("l" . ace-jump-line-mode)))


(use-package expand-region
  :defer t
  :ensure t
  :bind (:map stephenwan/speed-keys-map
              ("<SPC>" . er/expand-region)))


(use-package edit-utils
  :bind (("<M-up>" . stephenwan/swap-line-up)
         ("<M-down>" . stephenwan/swap-line-down)))



(provide 'module-edit-utils)
