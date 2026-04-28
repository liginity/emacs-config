;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'use-package)



;; security settings
;; NOTE remove the default key binding for `eval-last-sexp'.
(define-key (current-global-map) (kbd "C-x C-e") nil)

(setq enable-local-variables :safe)
(setq enable-local-eval nil)
(setq safe-local-variable-values nil)
(setq safe-local-eval-forms nil)

;; set scratch buffer mode
(setq initial-major-mode 'text-mode)



;; some variables
(setq Man-notify-method 'aggressive)
(setq backup-inhibited t)

(setq require-final-newline t)
(setq set-mark-command-repeat-pop t)
(setq sentence-end-double-space nil)
(setq scroll-error-top-bottom t)
(setq view-read-only t)

;; make emacs-pgtk work with wayland clipboard paste. from emacswiki.
(setq select-active-regions nil)


;; some modes
(setq-default indent-tabs-mode nil)

(electric-pair-mode)

(blink-cursor-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(column-number-mode)

;; disable version control
(setq vc-handled-backends nil)
(setq file-file-hook (delete #'vc-refresh-state find-file-hook))

;; recentf-mode
(setq recentf-max-saved-items 200)
(recentf-mode)

(defun personal-check-personal-file-list()
  (when (not (boundp 'personal-file-list))
    (let ((file-path (expand-file-name "personal-file-list"
                                       user-emacs-directory)))
      (if (file-exists-p file-path)
          (setq personal-file-list (load-file file-path))
        (setq personal-file-list nil)))))

(defun personal-recentf-open-file (file)
  "Open recentf-mode files with `completing-read'."
  (interactive
   (list (let (file-list)
           (personal-check-personal-file-list)
           (setq file-list (append personal-file-list recentf-list))
           (completing-read "Open file: " file-list))))
  (find-file file))

(define-key (current-global-map) (kbd "C-c r") #'personal-recentf-open-file)
;; TODO check `bind-key'.



;; start a server
(require 'server)
(unless (server-running-p)
  (server-start))



;; custom commands
(defun personal-backward-delete-word (arg)
  "Delete word without kill it."
  (interactive "p")
  (let ((old-point (point)))
    (delete-region
     (progn (backward-word arg) (point))
     old-point)))

(define-key (current-global-map) (kbd "C-<backspace>") #'personal-backward-delete-word)

(defun personal-move-beginning-of-line (arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (eq last-command #'personal-move-beginning-of-line)
      (move-beginning-of-line arg)
    (back-to-indentation)))

;; (define-key (current-global-map) (kbd "C-a") #'personal-move-beginning-of-line)

(defun personal-make-workspace-frame ()
  "Make a new frame to use as a workspace."
  (interactive)
  (let ((tab-count 6)
        (buffer (get-scratch-buffer-create)))
    (with-current-buffer buffer
      (with-selected-frame (make-frame)
        (cl-dotimes (_ tab-count) (tab-new))
        (tab-bar-select-tab 1)))))


;; local verion org-mode
(add-to-list 'load-path
             (file-name-concat user-emacs-directory "site-lisp" "org"))
(add-to-list 'load-path
             (file-name-concat user-emacs-directory "site-lisp" "markdown-mode"))


(add-to-list 'load-path
             (file-name-concat user-emacs-directory "inits"))
(add-to-list 'load-path
             (file-name-concat user-emacs-directory "packs"))

;; init code
(require 'init-ui)
(require 'init-keybindings)
(require 'init-writing)
(require 'init-completion)

(require 'org-pack)
(require 'temporary-pack nil t)

(with-eval-after-load 'cc-mode
  (add-to-list 'c-default-style '(c++-mode . "stroustrup"))
  (add-to-list 'c-default-style '(c-mode . "stroustrup")))


;; emacs Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)
