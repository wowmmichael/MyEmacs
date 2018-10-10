(defun buffer-utils/generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(provide 'buffer-utils)
