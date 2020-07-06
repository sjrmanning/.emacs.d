;;; sm-utils.el --- General utilities init.        -*- lexical-binding: t; -*-

;;; Commentary:

;; This file defines and configures modules related to general utilities used
;; in the config, such as terminals, file managers, browsers, etc.

;;; Code:

(use-package vterm
  :commands vterm)

(use-package vterm-toggle
  :commands (vterm-toggle vterm-toggle-cd)
  :bind
  (("C-c z" . vterm-toggle)
   (:map vterm-mode-map
         ("C-<return>" . vterm-toggle-insert-cd))))

;;; _
(provide 'sm-utils)
;;; sm-utils.el ends here
