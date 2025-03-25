;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; disable site-wide files.
(setq site-run-file nil)
;; do not load default.el
(setq inhibit-default-init t)
;; there is no equivalent config for command option --no-site-lisp.


;; ui
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
;; (tooltip-mode -1)
