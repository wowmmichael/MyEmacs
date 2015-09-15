(add-to-list 'load-path "~/.emacs.d/emacs-related/elisp/")

(require 'edit-utils)
(global-set-key (kbd "C-l") 'eu/collapse-around)

(require 'sys-utils)
(global-set-key (kbd "C-x C-s") 'su/start-cmd)
