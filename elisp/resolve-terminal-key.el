(defun resolve-terminal-key/iterm2()
  (progn
    (define-key input-decode-map "\e[1;10C" [M-S-right])
    (define-key input-decode-map "\e[1;10D" [M-S-left])
    (define-key input-decode-map "\e[1;10A" [M-S-up])
    (define-key input-decode-map "\e[1;10B" [M-S-down])
    (define-key input-decode-map "\e[1;9C" [M-right])
    (define-key input-decode-map "\e[1;9D" [M-left])
    (define-key input-decode-map "\e[1;9A" [M-up])
    (define-key input-decode-map "\e[1;9B" [M-down])))

(provide 'resolve-terminal-key)
