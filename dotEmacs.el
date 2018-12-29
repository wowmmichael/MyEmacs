(defconst *my-emacs* "MyEmacs"
  "Relative path to my emacs files.")

(let ((default-directory (file-name-as-directory (concat user-emacs-directory *my-emacs*))))
  (normal-top-level-add-subdirs-to-load-path))

(setq gc-cons-threshold (* 5 1000 1000))

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; (setq load-prefer-newer t)
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'init-customization)
(require 'init-package)
