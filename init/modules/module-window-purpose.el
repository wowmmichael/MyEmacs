(require 'use-package)

(use-package window-purpose
  :ensure t
  :defer t
  :bind (:map purpose-mode-prefix-map
              ("b" . switch-buffer-without-purpose)
              ("u" . purpose-set-window-purpose)
              ("f" . find-file-without-purpose)
              ("M-p" . purpose-toggle-window-buffer-dedicated)
              ("p" . purpose-toggle-window-purpose-dedicated))
  :config
  (setq-default purpose-preferred-prompt 'helm)
  (unbind-key "C-x b" purpose-mode-map)
  (unbind-key "C-x C-f" purpose-mode-map)
  (unbind-key "C-c ," purpose-mode-map)
  (bind-key "M-p" 'purpose-mode-prefix-map purpose-mode-map)
  (add-to-list 'purpose-user-mode-purposes '(python-mode . py))
  (add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
  (add-to-list 'purpose-user-regexp-purposes '("^test_.*$" . test))
  (add-to-list 'purpose-user-regexp-purposes '("^ ?\\*.*\\* ?$" . stars))
  (add-to-list 'purpose-user-regexp-purposes '("\\.dat$" . edit))
  (purpose-compile-user-configuration))

(provide 'module-window-purpose)
