(require 'use-package)
(require 'org)
(require 'org-id)
(require 'org-clock)
(require 'org-expiry)
(require 'org-capture)


(defvar org-my-dir
  (let* ((home-dir (file-name-as-directory (getenv "HOME")))
         (doc-dir (file-name-as-directory (concat home-dir "Documents"))))
    (file-name-as-directory (concat doc-dir "org_anize"))))

(defvar org-my-image-dir
  (file-name-as-directory (concat org-my-dir "images")))

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
(global-set-key (kbd "<f2> c") #'org-capture)
(global-set-key (kbd "<f2> a") #'org-agenda)
(global-set-key (kbd "<f2> l") #'org-store-link)

(defun stephen/org-get-link-from-properties (&optional p)
  "insert PROPERTY value of reading link"
  (interactive)
  (let* ((title (org-entry-get (point) "TITLE"))
        (link (org-entry-get (point) "LINK"))
        (formatted (concat "[[" link "][" title "]]")))
    (insert formatted)))


(defun stephen/org-my-keybindings ()
  "set local keybindings"
  (local-set-key (kbd "C-o l") #'stephen/org-get-link-from-properties)
  (local-set-key (kbd "<f2> i") #'org-clock-in)
  (local-set-key (kbd "<f2> o") #'org-clock-out))


(add-to-list 'org-mode-hook #'stephen/org-my-keybindings)


;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(defvar org-capture-templates)


(setq org-capture-templates
      (quote
       (("t" "todo" entry (file org-refile-file)
         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
        ("r" "reading" entry (file org-reading-file)
         "* TODO Read - %?\n%^{TITLE}p\n%^{LINK}p\n%^{AUTHOR}p\n" :clock-resume t :prepend t :empty-lines 1)
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

(setq org-clock-in-switch-to-state "IN-PROGRESS")
(setq org-clock-out-switch-to-state "PAUSE")
(setq org-clock-out-when-done t)

;; (setq org-tag-alist '((:startgroup . nil)
;;                       ("supply" . ?u) ("search" . ?s)
;;                       (:endgroup . nil)
;;                       (:startgroup . nil)
;;                       ("DR" . ?d) ("ILD" . ?i)
;;                       (:endgroup . nil)
;;                       ("reading" . ?l)))


;; miscellaneous

(setq org-id-link-to-org-use-id t)
(setq org-log-into-drawer t)
(add-to-list 'org-file-apps '("\\.png\\'" . default))



;; skyscanner links

(setq org-link-abbrev-alist
      '(("jira"        . "https://gojira.skyscanner.net/browse/%s")
        ("github" . "https://github.skyscannertools.net/search?q=%h")
        ("duckduckgo"      . "https://duckduckgo.com/?q=%s")))


(provide 'module-org)
