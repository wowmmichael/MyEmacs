(require 'use-package)


(global-set-key (kbd "C-d <left>")  'windmove-left)
(global-set-key (kbd "C-d <right>") 'windmove-right)
(global-set-key (kbd "C-d <up>")  'windmove-up)
(global-set-key (kbd "C-d <down>")  'windmove-down)



(use-package window-purpose
  :ensure t
  :defer t
  :bind (:map purpose-mode-prefix-map
              ("b" . switch-buffer-without-purpose)
              ("p" . purpose-set-window-purpose)
              ("f" . find-file-without-purpose)
              ("M-d" . purpose-toggle-window-buffer-dedicated)
              ("d" . purpose-toggle-window-purpose-dedicated))
  :config
  (setq-default purpose-preferred-prompt 'helm)
  (unbind-key "C-x b" purpose-mode-map)
  (unbind-key "C-x C-f" purpose-mode-map)
  (unbind-key "C-c ," purpose-mode-map)
  (bind-key "C-d p" 'purpose-mode-prefix-map purpose-mode-map)
  (add-to-list 'purpose-user-regexp-purposes '("^ ?\\*.*\\* ?$" . stars))
  (purpose-compile-user-configuration))

(provide 'module-window)
