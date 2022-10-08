(require 'use-package)


(global-set-key (kbd "<f2> <left>")  'windmove-left)
(global-set-key (kbd "<f2> <right>") 'windmove-right)
(global-set-key (kbd "<f2> <up>")  'windmove-up)
(global-set-key (kbd "<f2> <down>")  'windmove-down)


;; disable first as there seems to be some compatibility issues

;; (use-package window-purpose
;;   :ensure t
;;   :defer t
;;   :bind (:map purpose-mode-prefix-map
;;               ("b" . switch-buffer-without-purpose)
;;               ("p" . purpose-set-window-purpose)
;;               ("f" . find-file-without-purpose)
;;               ("M-d" . purpose-toggle-window-buffer-dedicated)
;;               ("d" . purpose-toggle-window-purpose-dedicated))
;;   :config
;;   (setq-default purpose-preferred-prompt 'helm)
;;   (unbind-key "C-x b" purpose-mode-map)
;;   (unbind-key "C-x C-f" purpose-mode-map)
;;   (unbind-key "C-c ," purpose-mode-map)
;;   (bind-key "<f2> p" 'purpose-mode-prefix-map purpose-mode-map)
;;   (add-to-list 'purpose-user-regexp-purposes '("^ ?\\*.*\\* ?$" . stars))
;;   (purpose-compile-user-configuration))

(provide 'module-window)
