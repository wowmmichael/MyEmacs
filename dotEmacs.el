(defconst *my-emacs* "MyEmacs"
  "Relative path to my emacs files.")

(let ((default-directory (file-name-as-directory (concat user-emacs-directory *my-emacs*))))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'init-customization)
(require 'init-package)


(put 'dired-find-alternate-file 'disabled nil)
