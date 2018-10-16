(require 'use-package)

(defun stephenwan/python-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq python-indent-offset 4)
  (elpy-enable)
  (elpy-mode))


(use-package elpy
  :ensure t
  :defer t
  :hook
  (python-mode . stephenwan/python-mode-hook)
  :config
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt"))

(provide 'module-python)
