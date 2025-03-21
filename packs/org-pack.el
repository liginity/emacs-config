;; -*- lexical-binding: t; -*-
;; org-mode configuration

(with-eval-after-load 'org
  (setq
   org-goto-auto-isearch nil
   ;; use "C-c C-z" to log something into a logbook.
   ;; `org-log-into-drawer' would enable using a logbook.
   org-log-into-drawer t

   org-edit-src-content-indentation 0
   )
  (add-hook 'org-mode-hook  #'visual-line-mode)

  ;; add custom structure template
  (add-to-list 'org-structure-template-alist '("tt" . "src text"))

  ;; It had "myliving-" as prefix.
  (defun personal-insert-date-of-property ()
    "Insert a timestamp for the date in the property of current heading.
For now, the property is named date.
Probably use this for all my writing."
    ;; TODO extend the function, so when there is a prefix argument, interactively set the date.
    ;; as org doesn't provide a function to generate the timestamp, I decide to borrow the code from `org-insert-time-stamp'
    (interactive)
    (let ((time (current-time))
          (fmt (car org-time-stamp-formats))
          (property-name "date")
          stamp)
      (setq fmt (concat "[" fmt "]"))
      (setq stamp (format-time-string fmt time))
      (org-set-property property-name stamp)))


  (define-key org-mode-map (kbd "C-c d") #'personal-insert-date-of-property)

  )

(provide 'org-pack)
