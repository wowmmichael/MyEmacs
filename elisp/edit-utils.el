;;; edit-utils.el ---
;;; Functions below are from org-metaup/org-metadown

(defun stephenwan/swap-line-up ()
  "Swap the current line with the line above."
  (interactive)
  (transpose-lines 1)
  (beginning-of-line -1))

(defun stephenwan/swap-line-down ()
  "Swap current line with the line below."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))


(provide 'edit-utils)
;;; edit-utils.el ends here
