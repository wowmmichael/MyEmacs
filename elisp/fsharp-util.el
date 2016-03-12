;;; fsharp-utils.el ---

(require 'edit-utils)
(require 'inf-fsharp-mode)

(defun fsharp-utils/send-code-block-to-eval ()
  (interactive)
   (let* ((original-point (point))
	  (block-start (save-excursion 
			 (eu/skip-non-empty-lines nil)
			 (point)))
	  (block-end (save-excursion
		       (eu/skip-non-empty-lines t)
		       (point))))
     (if (and (= block-start block-end))
	 (message "Code block is empty.")
       (inferior-fsharp-eval-region block-start block-end)
       (goto-char original-point))))


(provide 'fsharp-utils)
