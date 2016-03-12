(defun su/os-windows-p ()
  (string-equal system-type "windows-nt"))


(defun su/start-cmd ()
  "Start a cmd/shell at the current directory location."
  (interactive)
  (if (su/os-windows-p)
      (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
	(set-process-query-on-exit-flag proc nil))))


(provide 'sys-utils)
