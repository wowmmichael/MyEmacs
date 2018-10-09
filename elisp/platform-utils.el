(defun platform-utils/is-win-p()
  (string-equal system-type "window-nt"))

(defun platform-utils/is-osx-p()
  (string-equal system-type "darwin"))

(provide 'platform-utils)
