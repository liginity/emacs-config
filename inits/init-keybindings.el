;;; init-keybindings.el --- init key bindings for emacs builtins  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  liginity

;; Author: liginity <liginity@outlook.com>
;; Keywords: keybinding

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; MIT License for more details.

;; You should have received a copy of the MIT License
;; along with this program.  If not, see <https://opensource.org/license/mit>.

;;; Commentary:

;; Init key bindings for emacs builtins.
;; For global key bindings.

;;; Code:

;; for manual page
(global-set-key (kbd "C-h M") #'describe-mode)
(global-set-key (kbd "C-h m") #'man)

(global-set-key (kbd "C-h p") #'describe-package)
(global-set-key (kbd "C-h P") #'finder-by-keyword)

;; "C-/" is `undo' by default.
(global-set-key (kbd "C-/") #'comment-line)
(global-set-key (kbd "C-z") #'undo)

(global-set-key (kbd "C-<tab>") #'tab-bar-switch-to-recent-tab)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
