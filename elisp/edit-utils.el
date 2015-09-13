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

(provide 'edit-utils)
;;; edit-utils.el ends here
