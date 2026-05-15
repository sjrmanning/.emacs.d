;;; swift-ts-mode.el --- Major mode for Swift code -*- lexical-binding: t; -*-

;; Keywords: languages swift
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the
;; Swift programming language (https://swift.org).

;;; Code:
(require 'treesit)

(defconst swift-ts-mode-version
  "0.0.1"
  "The current version of `swift-ts-mode'.")

(defvar swift--treesit-keywords
  '("as" "async" "await" "break" "case" "catch" "class" "continue"
    "default" "do" "else" "finally" "struct"
    "for" "from" "func" "if")
  "Swift keywords for tree-sitter font-locking.")

(defvar swift--treesit-settings
  (treesit-font-lock-rules
   :language 'swift
   :feature 'keyword
   `([,@swift--treesit-keywords] @font-lock-keyword-face
     ;; [(this) (super)] @font-lock-keyword-face)
     )

   :language 'swift
   :feature 'string
   '((regex pattern: (regex_pattern)) @font-lock-regexp-face
     (line_string_literal) @font-lock-string-face)

   :feature 'function-name
   :language 'swift
   '((function_declaration
      name: (identifier) @font-lock-function-name-face))

   :feature 'regex
   :language 'swift
   :override t
   '((regex_lit marker: _ @font-lock-property-face))

   :feature 'number
   :language 'swift
   '((num_lit) @font-lock-number-face)

   :feature 'constant
   :language 'swift
   '([(bool_lit) (nil_lit)] @font-lock-constant-face)

   :feature 'keyword
   :language 'swift
   '((kwd_ns) @font-lock-type-face
     (kwd_name) @font-lock-type-face
     )

   :feature 'symbol
   :language 'swift
   '((sym_ns) @font-lock-type-face)

   :feature 'variable
   :language 'swift
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-variable-name-face))))

   :feature 'type
   :language 'swift
   `(((list_lit :anchor (sym_lit (sym_name) @font-lock-keyword-face)
                :anchor (sym_lit (sym_name) @font-lock-type-face))
      ))

   :feature 'quote
   :language 'swift
   '((quoting_lit
      marker: _ @font-lock-delimiter-face)
     (var_quoting_lit
      marker: _ @font-lock-delimiter-face)
     (syn_quoting_lit
      marker: _ @font-lock-delimiter-face)
     (unquoting_lit
      marker: _ @font-lock-delimiter-face)
     (unquote_splicing_lit
      marker: _ @font-lock-delimiter-face)
     (var_quoting_lit
      marker: _ @font-lock-delimiter-face))

   :language 'swift
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ))

(defvar swift-ts-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;;;###autoload
(add-to-list
 'treesit-language-source-alist
 '(swift "https://github.com/alex-pinkus/tree-sitter-swift.git"))

(define-derived-mode swift-ts-mode prog-mode "Swift[ts]"
  "Major mode for editing Swift code.
Requires Emacs 29 and libtree-sitter-swift.[dylib|so] available somewhere in
`treesit-extra-load-path'.
\\{swift-ts-mode-map}"
  (unless (treesit-language-available-p 'swift nil)
    (treesit-install-language-grammar 'swift))
  (setq-local comment-start "/")
  (when (treesit-ready-p 'swift)
    (treesit-parser-create 'swift)
    (setq-local treesit-font-lock-settings swift--treesit-settings)
    (setq-local treesit-defun-prefer-top-level t
                treesit-defun-tactic 'top-level)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string)
                  ;; (keyword constant symbol bracket builtin)
                  ;; (deref quote metadata definition variable type doc regex tagged-literals)
                  ))
    ;; (setq-local treesit-simple-indent-rules swift-ts-mode--fixed-indent-rules)
    (setq treesit--indent-verbose t)
    (treesit-major-mode-setup)
    (treesit-inspect-mode)
    ))

(provide 'swift-ts-mode)

;;; swift-ts-mode.el ends here
