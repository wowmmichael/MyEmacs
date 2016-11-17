;;; edit-utils.el --- 

(eval-when-compile
  (require 'cl))

(defmacro eu/skip-empty-lines (forwarding)
  `(let ((line-empty t))
     (while (and line-empty (= 0 (forward-line ,(if forwarding 1 -1))))        
       (setq line-empty (looking-at "\\s-*$")))))

(defmacro eu/collapse-lines (forwarding)
  (let* ((bstart (make-symbol "bstart"))
        (bend (make-symbol "bend"))
        (bops (list '(eu/skip-empty-lines nil)
                     `(setq ,bstart (+ 1 (line-end-position)))))
        (fops (list '(eu/skip-empty-lines t)
                     `(setq ,bend (point))))
        (ops (if forwarding (append fops bops) (append bops fops))))
    `(save-excursion
       (let ( ,bstart ,bend)         
         ,@ops
         (if (> ,bend ,bstart) (delete-region ,bstart ,bend))))))

(defun eu/skip-non-empty-lines (forwarding)
  (move-beginning-of-line nil)
  (let ((last-line-beginning (point))
	(cur-line-beginning (point))
	(line-empty (looking-at "^\\s-*$")))
    (while (and (not line-empty) (= 0 (forward-line (if forwarding 1 -1))))        
      (setq line-empty (looking-at "^\\s-*$"))
      (setq last-line-beginning cur-line-beginning)
      (setq cur-line-beginning (point)))
    (if (not forwarding) (goto-char last-line-beginning))))

(defun eu/mark-non-empty-lines ()
  (interactive)
  (let* ((original-line-start (line-beginning-position))
	 (block-start (save-excursion 
			(eu/skip-non-empty-lines nil)
	 		(point)))
	 (block-end (save-excursion
		      (eu/skip-non-empty-lines t)
		      (point))))
    (if (and (= block-start block-end))
	(message "The line is empty.")
      (push-mark (point))
      (push-mark block-end nil t)
      (goto-char block-start))))        

(defun eu/collapse-ahead ()
  (interactive)
  (eu/collapse-lines t))

(defun eu/collapse-back ()
  (interactive)
  (eu/collapse-lines nil))

(defun eu/collapse-around ()
  (interactive)
  (eu/collapse-ahead)
  (eu/collapse-back))

;;; Functions below are from org-metaup/org-metadown

(defun eu/swap-line-up ()
  "Swap the current line with the line above."
  (interactive)
  (transpose-lines 1)
  (beginning-of-line -1))

(defun eu/swap-line-down ()
  "Swap current line with the line below."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))

;;;

(defun eu/indent-buffer ()
  "Reindent the current buffer without moving point."
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'edit-utils)
;;; edit-utils.el ends here
