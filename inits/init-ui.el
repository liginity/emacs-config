;;; init-ui.el --- init code for ui                  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  liginity

;; Author: liginity <liginity@outlook.com>
;; Keywords: text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; MIT License for more details.

;; You should have received a copy of the MIT License
;; along with this program.  If not, see <https://opensource.org/license/mit>.

;;; Commentary:

;; Init code for user interface.

;;; Code:

;; tab-bar-mode
;; use "M-1" to select tab 1 in `tab-bar-mode'.
(setq tab-bar-select-tab-modifiers '(meta))
;; show numbers on tabs
(setq tab-bar-tab-hints t)

(tab-bar-mode)

;; create tabs on startup.
(progn
  (cl-dotimes (i 8)
    (tab-new))
  (tab-bar-select-tab 3)
  (switch-to-buffer (get-buffer-create "*ai-chat*"))
  (tab-bar-select-tab 1))

;; theme settings
(setq modus-themes-headings
      '((1 . (variable-pitch 2.0))
        (2 . (variable-pitch 1.7))
        (3 . (variable-pitch 1.4))
        (4 . (variable-pitch 1.1))
        (t . (bold))))

(provide 'init-ui)
;;; init-ui.el ends here
