;;; init-help.el --- init code for help, info, manual  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  liginity

;; Author: liginity <liginity@outlook.com>
;; Keywords: help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; MIT License for more details.

;; You should have received a copy of the MIT License
;; along with this program.  If not, see <https://opensource.org/license/mit>.

;;; Commentary:

;; Init code for `help-mode', `Info-mode', `Man-mode'.

;;; Code:

(use-package help-mode
  :bind (:map help-mode-map
              ("f" . describe-function)
              ("v" . describe-variable)))

(provide 'init-help)
;;; init-help.el ends here
