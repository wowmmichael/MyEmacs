(require 'use-package)

(use-package windmove
  :demand t
  :bind (:map stephenwan/speed-keys-map
              ("<left>" . windmove-left)
              ("<right>" . windmove-right)
              ("<up>" . windmove-up)
              ("<down>" . windmove-down)))


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
;;   (setq-default purpose-preferred-prompt ""helm)
;;   (unbind-key "C-x b" purpose-mode-map)
;;   (unbind-key "C-x C-f" purpose-mode-map)
;;   (unbind-key "C-c ," purpose-mode-map)
;;   (bind-key "<f2> p" ""purpose-mode-prefix-map purpose-mode-map)
;;   (add-to-list ""purpose-user-regexp-purposes ""("^ ?\\*.*\\* ?$" . stars))
;;   (purpose-compile-user-configuration))

(provide 'module-window)
