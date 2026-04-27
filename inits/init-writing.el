;;; init-writing.el --- init code for writing        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  liginity

;; Author: liginity <liginity@outlook.com>>
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

;; Init code for writing.
;; markdown-mode
;; not org-mode here.

;;; Code:

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\)\\'" . markdown-mode))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c c") markdown-mode-style-map)
  (define-key markdown-mode-map (kbd "C-M-c") #'markdown-insert-code)
  (define-key markdown-mode-map (kbd "C-s-c") #'markdown-insert-gfm-code-block)

  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil))))

;; The code blocks for these langs would have lang-mode highlight.
(setq markdown-code-lang-modes
      '(("C" . c-mode) ("cpp" . c++-mode) ("C++" . c++-mode)
        ("python" . python-mode)))

(setq markdown-header-scaling t
      markdown-asymmetric-header t
      markdown-fontify-code-blocks-natively t)

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-<return>") #'markdown-insert-header-dwim))

(provide 'init-writing)
;;; init-writing.el ends here
