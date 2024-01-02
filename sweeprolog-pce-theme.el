;;; sweeprolog-pce-theme.el --- Theme emulating PceEmacs, the SWI-Prolog built-in editor -*- lexical-binding:t -*-

;; Copyright (C) 2023-2024 Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <~eshel/dev@lists.sr.ht>
;; Keywords: prolog languages extensions
;; URL: https://git.sr.ht/~eshel/sweep

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

;;;###theme-autoload
(deftheme sweeprolog-pce
  "Theme emulating PceEmacs, the SWI-Prolog built-in editor."
  :kind 'color-scheme)

(let* ((blue '((((background light)) :foreground "blue")
               (((background dark))  :foreground "cyan")))
       (navy '((((background light)) :foreground "navyblue")
               (((background dark))  :foreground "darkcyan")))
       (faces
        `((function
           ,blue)
          (no-function
           ((t :foreground "red")))
          (functor
           ,navy)
          (arity
           ,navy)
          (predicate-indicator
           ,navy)
          (built-in
           ,blue)
          (neck
           ((t :weight bold)))
          (string
           ,navy)
          (comment
           ((((background light)) :foreground "darkgreen")
            (((background dark))  :foreground "green")))
          (head-local
           ((t :weight bold)))
          (head-meta
           ((t)))
          (head-dynamic
           ((t :foreground "magenta" :weight bold)))
          (head-multifile
           ((default :weight bold)
            (((background light)) :foreground "navyblue")
            (((background dark)) :foreground "palegreen")))
          (head-extern
           ((default :weight bold) . ,blue))
          (head-test
           ((default :foreground "#01bdbd" :weight bold)))
          (head-unreferenced
           ((default :foreground "red" :weight bold)))
          (head-exported
           ((default :weight bold) . ,blue))
          (head-hook
           ((default :underline t) . ,blue))
          (head-iso
           ((t :background "orange" :weight bold)))
          (head-def-iso
           ((default :weight bold) . ,blue))
          (head-def-swi
           ((default :weight bold) . ,blue))
          (head-imported
           ((default :foreground "darkgoldenrod4" :weight bold)))
          (head-undefined
           ((t :weight bold)))
          (head-public
           ((t :foreground "#016300" :weight bold)))
          (head-constraint
           ,navy)
          (meta-spec
           ((t :inherit font-lock-preprocessor-face)))
          (recursion
           ((t :underline t)))
          (local
           ,navy)
          (expanded
           ((default :underline t) . ,blue))
          (autoload
            ,navy)
          (imported
           ,blue)
          (extern
           ((default :underline t) . ,blue))
          (foreign
           ((t :foreground "darkturquoise")))
          (meta
           ((t :foreground "red4")))
          (undefined
           ((((background light)) :foreground "red")
            (((background dark)) :foreground "orange")))
          (thread-local
           ((t :foreground "magenta" :underline t)))
          (not-callable
           ((t :background "orange")))
          (constraint
           ((((background light)) :foreground "navyblue")
            (((background dark)) :foreground "palegreen")))
          (global
           ((((background light)) :foreground "magenta")
            (((background dark)) :foreground "darkcyan")))
          (multifile
           ((((background light)) :foreground "navyblue")
            (((background dark)) :foreground "palegreen")))
          (dynamic
           ((t :foreground "magenta")))
          (undefined-import
           ((t :foreground "red")))
          (html-attribute
           ((t :foreground "magenta4")))
          (html-call
           ((t :foreground "magenta4" :weight bold)))
          (option-name
           ((t :foreground "#3434ba")))
          (no-option-name
           ((((background light)) :foreground "red")
            (((background dark)) :foreground "orange")))
          (flag-name
           ,blue)
          (no-flag-name
           ((t :foreground "red")))
          (qq-type
           ((t :weight bold)))
          (qq-sep
           ((t :weight bold)))
          (qq-open
           ((t :weight bold)))
          (qq-content
           ((t :foreground "red4")))
          (qq-close
           ((t :weight bold)))
          (op-type
           ,blue)
          (dict-tag
           ((t :weight bold)))
          (dict-key
           ((t :weight bold)))
          (dict-sep
           ((t :weight bold)))
          (dict-return-op
           ,blue)
          (dict-function
           ,navy)
          (func-dot
           ((t :weight bold)))
          (file
           ((default :underline t)
            (((background light)) :foreground "blue")
            (((background dark)) :foreground "cyan")))
          (file-no-depend
           ((default :underline t :background "pink") . ,blue))
          (unused-import
           ((default :background "pink") . ,blue))
          (identifier
           ((t :weight bold)))
          (hook
           ((default :underline t) . ,blue))
          (module
           ((((background light)) :foreground "darkslateblue")
            (((background dark)) :foreground "lightslateblue")))
          (singleton
           ((default :weight bold)
            (((background light)) :foreground "red4")
            (((background dark)) :foreground "orangered1")))
          (fullstop
           ((t :inherit font-lock-negation-char-face)))
          (nil
           ((t :inherit font-lock-keyword-face)))
          (variable-at-point
           ((t :underline t)))
          (variable
           ((((background light)) :foreground "red4")
            (((background dark)) :foreground "orangered1")))
          (ext-quant
           ((t :inherit font-lock-keyword-face)))
          (keyword
           ,blue)
          (control
           ((t :inherit font-lock-keyword-face)))
          (atom
           ((t :inherit font-lock-constant-face)))
          (int
           ((t :inherit font-lock-constant-face)))
          (float
           ((t :inherit font-lock-constant-face)))
          (rational
           ((t :foreground "steelblue")))
          (chars
           ((((background light)) :foreground "navyblue")
            (((background dark)) :foreground "palegreen")))
          (codes
           ((((background light)) :foreground "navyblue")
            (((background dark)) :foreground "palegreen")))
          (error
           ((t :background "orange")))
          (type-error
           ((t :background "orange")))
          (instantiation-error
           ((t :background "orange")))
          (syntax-error
           ((t :background "orange")))
          (string-comment
           ((((background light)) :foreground "darkgreen")
            (((background dark)) :foreground "green")))
          (structured-comment
           ((((background light)) :foreground "darkgreen")
            (((background dark)) :foreground "green")))
          (hole
           ((t :box t)))
          (macro ((default :underline t) . ,blue))
          (declaration-option
           ((t :weight bold)))
          (dcg-string
           ((((background light)) :foreground "navyblue")
            (((background dark)) :foreground "palegreen")))
          (around-syntax-error
           ((t)))
          (clause
           ((t)))
          (grammar-rule
           ((t)))
          (term
           ((t)))
          (body
           ((t)))
          (directive
           ((t))))))

  (apply #'custom-theme-set-faces 'sweeprolog-pce
         (mapcar (lambda (face)
                   (cons (intern (concat "sweeprolog-"
                                         (symbol-name (car face))))
                         (cdr face)))
                 faces)))

(provide-theme 'sweeprolog-pce)
;;; sweeprolog-pce-theme.el ends here
