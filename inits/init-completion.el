;;; init-completion.el --- init code for completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  liginity

;; Author: liginity <liginity@outlook.com>
;; Keywords: completion, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; MIT License for more details.

;; You should have received a copy of the MIT License
;; along with this program.  If not, see <https://opensource.org/license/mit>.

;;; Commentary:

;; Init code for completion.

;;; Code:

;; (fido-vertical-mode)

(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)
(setq completion-styles '(flex))
(setq read-extended-command-predicate #'command-completion-default-include-p)

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 15) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-quit-at-boundary t)
  (corfu-auto-delay 0.1)
  :hook ((prog-mode . (lambda ()
                        (when (trusted-content-p)
                          ;; only enable `corfu-auto' for trusted content.
                          (setq-local corfu-auto t)
                          (corfu-mode))))))

(provide 'init-completion)
;;; init-completion.el ends here
