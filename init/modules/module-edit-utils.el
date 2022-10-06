(require 'use-package)

(bind-keys* :prefix-map stephenwan/mc-keys-map
           :prefix "M-m")

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("M-=" . mc/mark-next-like-this)
         ("M-+" . mc/mark-next-like-this)
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
  :bind (("<f2> <f2>" . ace-jump-mode)
         ("<f2> l" . ace-jump-line-mode)
         ("<f2> w" . ace-jump-word-mode)))

(use-package ace-mc
  :ensure t
  :defer t
  :bind (:map stephenwan/mc-keys-map
              ("<f2> <f2>" . ace-mc-add-multiple-cursors)))

(use-package expand-region
  :defer t
  :ensure t
  :bind (("C-w" . er/expand-region)))

(use-package edit-utils
  :bind (("<M-up>" . eu/swap-line-up)
         ("<M-down>" . eu/swap-line-down)
         ("C-o f" . eu/indent-buffer)
         ("C-o h" . eu/collapse-around)))

(defun stephenwan/outline-magic-all-in-one ()
  (interactive)
  (unless (bound-and-true-p outline-minor-mode)
    (outline-minor-mode))
  (outline-cycle))

(use-package outline-magic
  :ensure t
  :defer t
  :bind (("M-|" . stephenwan/outline-magic-all-in-one)))

(provide 'module-edit-utils)
