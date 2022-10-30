(require 'use-package)
(require 'org)
(require 'org-id)
(require 'org-clock)
(require 'org-expiry)
(require 'org-capture)
(require 'org-tempo)
(require 'org-keys)


(defvar org-my-dir
  (let* ((home-dir (file-name-as-directory (getenv "HOME")))
         (doc-dir (file-name-as-directory (concat home-dir "Documents"))))
    (file-name-as-directory (concat doc-dir "org_anize"))))

(defvar org-my-image-dir
  (file-name-as-directory (concat org-my-dir "images")))


(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-indent
  :hook (org-mode . org-indent-mode))

(use-package org-download
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-download-enable)
  :custom
  (org-download-image-dir org-my-image-dir)
  (org-download-display-inline-images nil))


(setq org-directory org-my-dir)
(setq org-agenda-files (list org-my-dir))

(defvar org-gtd-file (expand-file-name "gtd.org" org-my-dir))
(defvar org-refile-file (expand-file-name "refile.org" org-my-dir))
(defvar org-reading-file (expand-file-name "reading.org" org-my-dir))


(defun gtd ()
  "Open the GTD file."
  (interactive)
  (find-file org-gtd-file))

(defun refile ()
  "Open the GTD refile file."
  (interactive)
  (find-file org-refile-file))

(defun reading ()
  "Open the GTD reading file."
  (interactive)
  (find-file org-reading-file))



;; (setf org-agenda-files
;;       (quote (org-gtd-file
;;               org-refile-file
;;               org-reading-file)))



;; key binding, ref: https://orgmode.org/manual/Activation.html
(global-set-key (kbd "C-o c") #'org-capture)
(global-set-key (kbd "<f3>") #'org-agenda)
(global-set-key (kbd "C-<f3>") #'org-store-link)



(defun stephen/org-get-link-from-properties ()
  "insert PROPERTY value of reading link"
  (interactive)
  (let* ((title (org-entry-get (point) "TITLE"))
        (link (org-entry-get (point) "LINK"))
        (formatted (concat "[[" link "][" title "]]")))
    (insert formatted)))



(defun stephen/org-clock-toggle ()
  "toggle the clock on an org item"
  (interactive)
  (if (org-clocking-p)
      (when (and (marker-buffer org-clock-marker)
                 (< (point) org-clock-marker)
                 (> (org-with-wide-buffer (org-entry-end-position))
                    org-clock-marker))
        (let ((org-log-note-clock-out nil))
          (org-clock-out)))
      (progn (org-clock-in))))


(defun stephen/org-my-keybindings ()
  "set local keybindings"
  (local-set-key (kbd "C-o l") #'stephen/org-get-link-from-properties)
  (local-set-key (kbd "C-o [") #'org-clock-in)
  (local-set-key (kbd "C-o ]") #'org-clock-out)
  (local-set-key (kbd "C-o C-o") #'stephen/org-clock-toggle)
  (local-set-key (kbd "C-o t") #'stephen/org-mode-timestamp-toggle-timezone))


(add-to-list 'org-mode-hook #'stephen/org-my-keybindings)


;; agenda setup

(setq org-agenda-inhibit-startup t) ;; ~50x speedup
(setq org-agenda-span 'day)
(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
(setq org-agenda-window-setup 'current-window)



;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(defvar org-capture-templates)


(setq org-capture-templates
      (quote
       (("t" "todo" entry (file org-refile-file)
         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
        ("r" "reading" entry (file org-reading-file)
         "* TODO [#C] Read -%?\n%^{TITLE}p\n%^{LINK}p\n%^{AUTHOR}p\n" :clock-resume t :prepend t :empty-lines 1)
        ("n" "note" entry (file org-refile-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        )))

;; Configure it a bit to my liking
(setq
 org-expiry-created-property-name "CREATED" ; Name of property when an item is created
 org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
 )

(defun stephen/insert-created-timestamp ()
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  (insert " ")
)

;; Whenever a TODO entry is created, I want a timestamp
;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
(defadvice org-todo (after stephen/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (when (member (org-get-todo-state) org-todo-keywords-1)
    (stephen/insert-created-timestamp)))

(ad-activate 'org-todo)

(defadvice org-capture (after stephen/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  ; Test if the captured entry is a TODO, if so insert the created
  ; timestamp property, otherwise ignore
  (when (member (org-get-todo-state) org-todo-keywords-1)
    (stephen/insert-created-timestamp)))

(ad-activate 'org-capture)


;; state and tag

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")
              (sequence "PAUSE(p)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

;; check defined-colors for all available colors
;; check font-weight-table for all available weights
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "white" :weight bold)
              ("IN-PROGRESS" :foreground "lightblue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("PAUSE" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
             )))


(setq org-treat-S-cursor-todo-selection-as-state-change nil)


;; (setq org-tag-alist '((:startgroup . nil)
;;                       ("supply" . ?u) ("search" . ?s)
;;                       (:endgroup . nil)
;;                       (:startgroup . nil)
;;                       ("DR" . ?d) ("ILD" . ?i)
;;                       (:endgroup . nil)
;;                       ("reading" . ?l)))


;; org-faces
;;

(defun stephen/org-custom-faces ()
  "customize a few faces"
;;  (set-face-attribute 'org-level-1 nil :height 1.5)
;;  (set-face-attribute 'org-level-2 nil :height 1.22)
;;  (set-face-attribute 'org-level-3 nil :height 1.15)
  (set-face-attribute 'org-drawer nil :height 0.8)
;;  (set-face-attribute 'org-special-keyword nil :height 0.8)
;;  (set-face-attribute 'org-property-value nil :height 0.8)
  ;;  (set-face-attribute 'org-meta-line nil :height 0.8)
  )

(add-hook 'org-mode-hook #'stephen/org-custom-faces)

;;org-clock
;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state "IN-PROGRESS")
(setq org-clock-out-switch-to-state "PAUSE")
(setq org-clock-out-when-done t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; miscellaneous


(setq org-use-speed-commands t)
(setq org-adapt-indentation 'headline-data)
(setq org-edit-src-content-indentation 0)
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
(setq org-id-link-to-org-use-id t)
(setq org-log-into-drawer t)
(add-to-list 'org-file-apps '("\\.png\\'" . default))


;; custom datetime utility


(defconst stephen/default-uk-zone
  "Europe/London"
  "Default UK timezone in tzdb.")


(defconst stephen/default-local-zone
  "Asia/Hong_Kong"
  "Default local timezone in tzdb.")


(defun stephen/convert-org-ts-local-to-uk ()
  (let ((from-zone stephen/default-local-zone)
        (to-zone stephen/default-uk-zone))
    (stephen/convert-org-ts-timezone from-zone to-zone)))


(defun stephen/convert-org-ts-uk-to-local ()
  (let ((from-zone stephen/default-uk-zone)
        (to-zone stephen/default-local-zone))
    (stephen/convert-org-ts-timezone from-zone to-zone)))


(defun stephen/convert-org-ts-timezone (from-zone to-zone)
  (if (org-in-regexp org-tsr-regexp-both)
      (let* ((start (match-string 1))
             (end (match-string 3))
             (start* (stephen/convert-timestamp-timezone start from-zone to-zone))
             (end* (if end (stephen/convert-timestamp-timezone end from-zone to-zone)))
             (to-zone* (stephen/format-tz-string to-zone))
             (convert-range (lambda () (if (and start* end*) (format "%s--%s %s" start* end* to-zone*))))
             (convert-ts (lambda () (if start* (format "%s %s" start* to-zone*))))
             (o (if end* (funcall convert-range) (funcall convert-ts))))
        (if o
            (progn (message o)
                   (kill-new o))
          (message "not convertible org datetime or datetime range")))
    (message "not a valid org datetime or datetime range")))


(defconst stephen/timestamp-regexp
  "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^0-9]+\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"
  "Regex for extracting date and time parts of a timestamp.")


(defun stephen/org-timestamp-has-time? (s)
  (save-match-data
    (and (string-match stephen/timestamp-regexp s)
         (and (match-string 1 s) (match-string 2 s)))))


(defun stephen/convert-timestamp-timezone (s from-zone to-zone)
  (if (stephen/org-timestamp-has-time? s)
      (let ((time-pair (stephen/extract-times-from-timerange s)))
        (if time-pair
            (let* ((ts-dt-s (ts-parse-org (car time-pair)))
                   (ts-dt-e (ts-parse-org (cdr time-pair)))
                   (ts-dt-s* (stephen/ts--convert-timezone ts-dt-s from-zone to-zone))
                   (ts-dt-e* (stephen/ts--convert-timezone ts-dt-e from-zone to-zone))
                   (same-date (= (ts-d ts-dt-s*) (ts-d ts-dt-e*)))
                   (s_ (stephen/format-ts-to-timestamp ts-dt-s*)))
              (if same-date
                  (format "[%s-%s]" s_ (ts-format "%H:%M" ts-dt-e*))
                (format "[%s]--[%s]" s_ (stephen/format-ts-to-timestamp ts-dt-e*))))
          (let* ((ts-dt (ts-parse-org s))
                 (ts-dt* (stephen/ts--convert-timezone ts-dt from-zone to-zone)))
            (format "[%s]" (stephen/format-ts-to-timestamp ts-dt*)))))))


(defconst stephen/timerange-regexp
  "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^0-9]+\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)--?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"
  "Regex for a pair of time value.")


(defun stephen/extract-times-from-timerange (s)
  (save-match-data
    (if (string-match stephen/timerange-regexp s)
        (let ((date-part (match-string 1 s))
              (time-part-s (match-string 2 s))
              (time-part-e (match-string 3 s)))
          (cons
           (concat date-part " " time-part-s)
           (concat date-part " " time-part-e))))))



(defun stephen/ts--convert-timezone (ts-dt from-zone to-zone)
  (let* ((time-string (ts-format "%H:%M %z" ts-dt))
         (tzc-t (tzc--get-converted-time time-string from-zone to-zone))
         (minute (nth 0 tzc-t))
         (hour (nth 1 tzc-t))
         (day (nth 2 tzc-t)))
    (ts-apply :hour hour :minute minute (ts-adjust 'day day ts-dt))))


(defun stephen/format-ts-to-timestamp (ts-dt)
  (ts-format "%Y-%m-%d %a %H:%M" ts-dt))


(defun stephen/format-tz-string (timezone)
  (format-time-string "%z(%Z)" nil timezone))

(defconst stephen/timezone-regexp
  " *\\([+-][0-9]\\{4\\}\\)(\\([A-Z]\\{3\\}\\))"
  "Regular expression for matching timezone information.")


(defun stephen/timezone-after-org-timestamp ()
  (save-match-data
    (if (org-in-regexp org-tsr-regexp-both)
        (save-excursion
          (goto-char (match-end 0))
          (if (looking-at stephen/timezone-regexp)
              (match-string 2))))))


(format-time-string "%Z" nil stephen/default-uk-zone)


(defun stephen/org-mode-timestamp-toggle-timezone ()
  (interactive)
  (let ((z (stephen/timezone-after-org-timestamp)))
    (cond ((and z (string= z (format-time-string "%Z" nil stephen/default-uk-zone)))
           (stephen/convert-org-ts-uk-to-local))
          ((and z (string= z (format-time-string "%Z" nil stephen/default-local-zone)))
           (stephen/convert-org-ts-local-to-uk))
          (z (message "Unsupported timezone detected."))
          (t (stephen/convert-org-ts-local-to-uk)))))



;; skyscanner links

(setq org-link-abbrev-alist
      '(("jira"        . "https://gojira.skyscanner.net/browse/%s")
        ("github" . "https://github.skyscannertools.net/search?q=%h")
        ("duckduckgo"      . "https://duckduckgo.com/?q=%s")))


(provide 'module-org)
