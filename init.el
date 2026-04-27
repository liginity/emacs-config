;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'use-package)



;; create tabs on startup.
(progn
  (cl-dotimes (i 8)
    (tab-new))
  (tab-bar-select-tab 3)
  (switch-to-buffer (get-buffer-create "*ai-chat*"))
  (tab-bar-select-tab 1))
(global-set-key (kbd "C-<tab>") #'tab-bar-switch-to-recent-tab)


(setq Man-notify-method 'aggressive)

;; NOTE remove the default key binding for `eval-last-sexp'.
(define-key (current-global-map) (kbd "C-x C-e") nil)

(setq enable-local-variables :safe)
(setq enable-local-eval nil)
(setq safe-local-variable-values nil)
(setq safe-local-eval-forms nil)


;; set scratch buffer mode
(setq initial-major-mode 'text-mode)
;; some variables
(setq backup-inhibited t)

(setq require-final-newline t)
(setq set-mark-command-repeat-pop t)
(setq sentence-end-double-space nil)
(setq scroll-error-top-bottom t)
(setq view-read-only t)
(setq set-mark-command-repeat-pop t)



;; some modes
(setq-default indent-tabs-mode nil)

(electric-pair-mode 1)
(blink-cursor-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; recentf-mode
(setq recentf-max-saved-items 200)
(recentf-mode 1)
(defun personal-recentf-open-file (file)
  "Open recentf-mode files with `completing-read'."
  (interactive
   (list (let (file-list)
           (when (not (boundp 'personal-file-list))
             (load-file (expand-file-name "personal-file-list"
                                          user-emacs-directory)))
           (setq file-list (copy-sequence personal-file-list))
           (setcdr (last file-list) recentf-list)
           (completing-read "Open file: " file-list))))
  (find-file file))

(define-key (current-global-map) (kbd "C-c r") #'personal-recentf-open-file)
;; TODO check `bind-key'.



;; start a server
(require 'server)
(unless (server-running-p)
  (server-start))


;; completion settings
(fido-vertical-mode 1)
(setq completion-cycle-threshold 3)

(setq tab-always-indent 'complete)
;; (setq completion-styles '())



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
        (cl-dotimes (_ tab-count) (tab-new))))))


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
(require 'init-keybindings)
(require 'init-writing)

(require 'org-pack)
(require 'temporary-pack nil t)

(with-eval-after-load 'cc-mode
  (add-to-list 'c-default-style '(c++-mode . "stroustrup"))
  (add-to-list 'c-default-style '(c-mode . "stroustrup")))


;; version control related
;; disable version control
(setq vc-handled-backends nil)


;; emacs Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)
