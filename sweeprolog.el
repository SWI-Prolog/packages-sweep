;;; sweeprolog.el --- Embedded SWI-Prolog -*- lexical-binding:t -*-

;; Copyright (C) 2022-2023 Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <~eshel/dev@lists.sr.ht>
;; Keywords: prolog languages extensions
;; URL: https://git.sr.ht/~eshel/sweep
;; Package-Version: 0.18.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; sweep is an embedding of SWI-Prolog in Emacs.  It uses the C
;; interfaces of both SWI-Prolog and Emacs Lisp to create a
;; dynamically loaded Emacs module that contains the SWI-Prolog
;; runtime.  sweep provides an interface for interacting with the
;; embedded Prolog via a set of Elisp functions, as well as user
;; facing modes and commands for writing and running Prolog within
;; Emacs.
;;
;; For more information, see the sweep manual at
;; <https://eshelyaron.com/sweep.html>.  The manual can also be read
;; locally by evaluating (info "(sweep) Top")

;;; Code:

(require 'comint)
(require 'xref)
(require 'autoinsert)
(require 'eldoc)
(require 'flymake)
(require 'help-mode)
(require 'find-func)
(require 'shr)
(require 'info-look)


;;;; Global variables

(defvar sweeprolog--directory (file-name-directory load-file-name))

(defvar sweeprolog--initialized nil)

(defvar sweeprolog-prolog-server-port nil)

(defvar sweeprolog-read-predicate-history nil)

(defvar sweeprolog-read-module-history nil)

(defvar sweeprolog-read-functor-history nil)

(defvar sweeprolog-top-level-signal-goal-history nil)

(defvar sweeprolog--extra-init-args nil)

(defvar sweeprolog-insert-term-functions
  '(sweeprolog-maybe-insert-next-clause
    sweeprolog-maybe-define-predicate)
  "Hook of functions that insert a Prolog term in a certain context.

Each hook function is called with four arguments describing the
current context.  The first argument, POINT, is the buffer
position in which insertion should take place.  The rest of the
arguments, KIND, BEG and END, describe the previous non-comment
Prolog token as returned from `sweeprolog-last-token-boundaries'.")

(defvar sweeprolog-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?~ "." table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?! "w" table)
    table))

(defvar sweeprolog-top-level-mode-syntax-table sweeprolog-mode-syntax-table)

(defvar sweeprolog-module-documentation-regexp (rx bol (zero-or-more whitespace)
                                                   ":-" (zero-or-more whitespace)
                                                   "module("))
;;;; User options

(defgroup sweeprolog nil
  "SWI-Prolog Embedded in Emacs."
  :group 'prolog)

(defcustom sweeprolog-swipl-sources t
  "Location of the SWI-Prolog source code root directory.

When non-nil, the function `sweeprolog-predicate-location' will
attempt to find the C definitions of SWI-Prolog native built-in
predicates.

The value of this option can be a string, in which case it should
be a path to the SWI-Prolog source code root directory.  Any
other non-nil value instructs `sweeprolog-predicate-location' to
try and find the SWI-Prolog sources among known project roots
obtained from `project-known-project-roots', which see."
  :package-version '((sweeprolog . "0.7.1"))
  :type '(choice (const  :tag "Detect"  t)
                 (string :tag "Manual")
                 (const  :tag "Disable" nil))
  :group 'sweeprolog)

(defcustom sweeprolog-module-header-comment-skeleton ?\n
  "Additional content for the topmost comment in module headers.

The SWI-Prolog module header inserted by \\[auto-insert] includes
a multiline comment at the very start of the buffer which
contains the name and mail address of the author based on the
user options `user-full-name' and `user-mail-address'
respectively, followed by the value of this variable, is
interpreted as a skeleton (see `skeleton-insert').  In its
simplest form, this may be a string or a character.

This user option may be useful, for example, to include copyright
notices with the module header."
  :package-version '((sweeprolog . "0.4.6"))
  :type 'any
  :group 'sweeprolog)

(defcustom sweeprolog-indent-offset 4
  "Number of columns to indent with in `sweeprolog-mode' buffers."
  :package-version '((sweeprolog . "0.3.1"))
  :type 'integer
  :group 'sweeprolog)

(defcustom sweeprolog-qq-mode-alist '(("graphql"    . graphql-mode)
                                      ("javascript" . js-mode)
                                      ("html"       . html-mode))
  "Association between Prolog quasi-quotation types and Emacs modes.

This is a list of pairs of the form (TYPE . MODE), where TYPE is
a Prolog quasi-quotation type given as a string, and MODE is a
symbol specifing a major mode."
  :package-version '((sweeprolog . "0.4.3"))
  :type '(alist :key-type string :value-type symbol)
  :group 'sweeprolog)

(defcustom sweeprolog-enable-cycle-spacing t
  "If non-nil and `cycle-spacing-actions' is defined, extend it.

This makes the first invocation of \\[cycle-spacing] in
`sweeprolog-mode' buffers update whitespace around point using
`sweeprolog-align-spaces', which see."
  :package-version '((sweeprolog . "0.5.3"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-analyze-buffer-on-idle t
  "If non-nil, analyze `sweeprolog-mode' buffers on idle."
  :package-version '((sweeprolog . "0.8.2"))
  :type 'boolean
  :group 'sweeprolog)

(make-obsolete-variable 'sweeprolog-colourise-buffer-on-idle
                        "Use `sweeprolog-analyze-buffer-on-idle' instead"
                        "sweeprolog version 0.8.2")

(defcustom sweeprolog-analyze-buffer-max-size 100000
  "Maximum buffer size to analyze on idle."
  :package-version '((sweeprolog . "0.8.2"))
  :type 'integer
  :group 'sweeprolog)

(make-obsolete-variable 'sweeprolog-colourise-buffer-max-size
                        "Use `sweeprolog-analyze-buffer-max-size' instead"
                        "sweeprolog version 0.8.2")

(defcustom sweeprolog-analyze-buffer-min-interval 1.5
  "Minimum idle time to wait before analyzing the buffer."
  :package-version '((sweeprolog . "0.8.2"))
  :type 'float
  :group 'sweeprolog)

(make-obsolete-variable 'sweeprolog-colourise-buffer-min-interval
                        "Use `sweeprolog-analyze-buffer-min-interval' instead"
                        "sweeprolog version 0.8.2")

(defcustom sweeprolog-swipl-path nil
  "Path to the swipl executable.
When non-nil, this is used by the embedded SWI-Prolog runtime to
locate its \"home\" directory.  Otherwise, `executable-find' is
used to find the swipl executable."
  :package-version '((sweeprolog . "0.1.1"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-libswipl-path nil
  "Path to the libswipl shared object.
On Linux and other ELF-based platforms, `sweep' first loads
libswipl before loading `sweep-module'.  When this option is
nil (the default), libswipl is located automatically, otherwise
the value of this option is used as its path."
  :package-version '((sweeprolog . "0.6.1"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-messages-buffer-name "*sweep Messages*"
  "The name of the buffer to use for logging Prolog messages."
  :package-version '((sweeprolog . "0.1.1"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-read-flag-prompt "Flag: "
  "Prompt used for reading a Prolog flag name from the minibuffer."
  :package-version '((sweeprolog . "0.1.2"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-read-module-prompt "Module: "
  "Prompt used for reading a Prolog module name from the minibuffer."
  :package-version '((sweeprolog . "0.1.0"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-read-predicate-prompt "Predicate: "
  "Prompt used for reading a Prolog predicate name from the minibuffer."
  :package-version '((sweeprolog . "0.1.0"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-read-exportable-predicate-prompt "Export predicate: "
  "Prompt used for reading an exportable predicate name."
  :package-version '((sweeprolog . "0.6.2"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-read-pack-prompt "Pack: "
  "Prompt used for reading a Prolog pack name from the minibuffer."
  :package-version '((sweeprolog . "0.1.0"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-top-level-display-action nil
  "Display action used for displaying the `sweeprolog-top-level' buffer."
  :package-version '((sweeprolog . "0.1.0"))
  :type '(choice (const :tag "Default" nil)
                 (cons  :tag "Action"
                        (choice (function :tag "Display Function")
                                (repeat :tag "Display Functions" function))
                        alist))
  :group 'sweeprolog)

(defcustom sweeprolog-top-level-min-history-length 3
  "Minimum input length to record in the `sweeprolog-top-level' history.

Inputs shorther than the value of this variable will not be
inserted to the input history in `sweeprolog-top-level-mode' buffers."
  :package-version '((sweeprolog . "0.2.1"))
  :type 'string
  :group 'sweeprolog)

(defcustom sweeprolog-init-on-load t
  "If non-nil, initialize Prolog when `sweeprolog' is loaded."
  :package-version '((sweeprolog "0.1.0"))
  :type 'boolean
  :group 'sweeprolog)

(make-obsolete-variable 'sweeprolog-init-on-load
                        (concat
                         "Prolog is initialized on-demand,"
                         " regardless of the value of this option.")
                        "sweeprolog version 0.7.2")

(defcustom sweeprolog-init-args (list "-q"
                                      "--no-signals"
                                      "-g"
                                      "create_prolog_flag(sweep,true,[access(read_only),type(boolean)])"
                                      "-l"
                                      (expand-file-name
                                       "sweep.pl"
                                       sweeprolog--directory))
  "List of strings used as initialization arguments for Prolog."
  :package-version '((sweeprolog "0.5.2"))
  :type '(repeat string)
  :group 'sweeprolog)

(defcustom sweeprolog-enable-flymake t
  "If non-nil, enable `flymake' suport in `sweeprolog-mode' buffers."
  :package-version '((sweeprolog "0.6.0"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-note-implicit-autoloads t
  "If non-nil, `flymake' notes implicitly autoload predicates."
  :package-version '((sweeprolog "0.9.2"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-enable-eldoc t
  "If non-nil, enable `eldoc' suport in `sweeprolog-mode' buffers."
  :package-version '((sweeprolog "0.4.7"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-enable-cursor-sensor t
  "If non-nil, enable `cursor-sensor-mode' in `sweeprolog-mode'.

When enabled, `sweeprolog-mode' leverages `cursor-sensor-mode' to
highlight all occurrences of the variable at point in the current
clause."
  :package-version '((sweeprolog "0.4.2"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-new-predicate-location-function
  #'sweeprolog-default-new-predicate-location
  "Function used to choose a location for a new predicate definition.

It should take three arguments describing the new predicate,
FUNCTOR, ARITY and NECK, and move point to a suitable position in
the current buffer where the new predicate definition should be
inserted.

FUNCTOR is the predicate name given as a string, ARITY is its
arity given as an integer, and NECK is the neck operator of the
predicate (e.g. \":-\" for regular clauses and \"-->\" for DCG
non-terminals)."
  :package-version '((sweeprolog "0.8.11"))
  :type '(choice (const    :tag "Below Current Predicate"
                        sweeprolog-default-new-predicate-location)
                 (const    :tag "Above Current Predicate"
                        sweeprolog-new-predicate-location-above-current)
                 (function :tag "Custom Function"))
  :group 'sweeprolog)

(defcustom sweeprolog-top-level-signal-default-goal "sweep_interrupt"
  "Prolog goal used by default for signaling top-level threads."
  :package-version '((sweeprolog "0.8.12"))
  :type 'string
  :group 'sweeprolog-top-level)

(defcustom sweeprolog-highlight-holes t
  "If non-nil, highlight holes in a dedicated face."
  :package-version '((sweeprolog "0.8.12"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-read-predicate-documentation-function
  #'sweeprolog-read-predicate-documentation-default-function
  "Function returning information for initial predicate documentation.

The function should take two arguments, the functor name and
arity of the predicate, and return a list of three strings.  The
first string is the predicate's head template, the second is its
determinism specification, and the third is a summary line."
  :package-version '((sweeprolog "0.10.1"))
  :type '(choice
          (const
           :tag "Prompt in Minibuffer"
           sweeprolog-read-predicate-documentation-default-function)
          (const
           :tag "Use Holes"
           sweeprolog-read-predicate-documentation-with-holes)
          (function
           :tag "Custom Function"))
  :group 'sweeprolog)

(defcustom sweeprolog-enable-help-echo t
  "If non-nil, annotate Prolog tokens with the `help-echo' property.

When enabled, `sweeprolog-mode' adds a short description to each
token via its `help-echo' text property."
  :package-version '((sweeprolog "0.12.0"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-rename-variable-allow-existing 'confirm
  "If non-nil, allow renaming variables to existing variable names.
If it is the symbol `confirm', allow but ask for confirmation
first."
  :package-version '((sweeprolog "0.15.1"))
  :type '(choice (const :tag "Allow"   t)
                 (const :tag "Confirm" confirm)
                 (const :tag "Refuse"  nil))
  :group 'sweeprolog)

(defcustom sweeprolog-dependency-directive 'infer
  "Prolog directive to use for adding dependencies.
This determines whether `sweeprolog-update-dependencies' uses
`autoload/2' or `use_module/2' directives to make implicit
dependencies into explicit dependencies.

If set to the symbol `use-module', then
`sweeprolog-update-dependencies' only uses `use_module/2'
directives.  If set to the symbol `infer', then
`sweeprolog-update-dependencies' uses `autoload/2' directives
unless the buffer already contains dependency directives and all
of them are `use_module/2' directives.  Any other values means to
use `autoload/2' for all added directives."
  :package-version '((sweeprolog "0.17.0"))
  :type '(choice (const :tag "Prefer use_module/2" use-module)
                 (const :tag "Prefer autoload/2"  autoload)
                 (const :tag "Infer" infer))
  :group 'sweeprolog)

(defcustom sweeprolog-highlight-breakpoints t
  "If non-nil, highlight breakpoints with a dedicated face."
  :package-version '((sweeprolog "0.17.0"))
  :type 'boolean
  :group 'sweeprolog)

;;;; Keymaps

(defvar sweeprolog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") #'sweeprolog-set-breakpoint)
    (define-key map (kbd "C-c C-c") #'sweeprolog-analyze-buffer)
    (define-key map (kbd "C-c C-d") #'sweeprolog-document-predicate-at-point)
    (define-key map (kbd "C-c C-e") #'sweeprolog-export-predicate)
    (define-key map (kbd "C-c TAB") #'sweeprolog-forward-hole)
    (define-key map (kbd "C-c C-i") #'sweeprolog-forward-hole)
    (define-key map (kbd "C-c <backtab>") #'sweeprolog-backward-hole)
    (define-key map (kbd "C-c C-S-i") #'sweeprolog-backward-hole)
    (define-key map (kbd "C-c C-l") #'sweeprolog-load-buffer)
    (define-key map (kbd "C-c C-m") #'sweeprolog-insert-term-with-holes)
    (define-key map (kbd "C-c C-o") #'sweeprolog-find-file-at-point)
    (define-key map (kbd "C-c C-q") #'sweeprolog-top-level-send-goal)
    (define-key map (kbd "C-c C-r") #'sweeprolog-rename-variable)
    (define-key map (kbd "C-c C-s") #'sweeprolog-term-search)
    (define-key map (kbd "C-c C-t") #'sweeprolog-top-level)
    (define-key map (kbd "C-c C-u") #'sweeprolog-update-dependencies)
    (define-key map (kbd "C-c C-`")
                (if (fboundp 'flymake-show-buffer-diagnostics) ;; Flymake 1.2.1+
                    #'sweeprolog-show-diagnostics
                  #'flymake-show-diagnostics-buffer))
    (define-key map (kbd "C-c C-&") #'sweeprolog-async-goal)
    (define-key map (kbd "C-c C--") #'sweeprolog-decrement-numbered-variables)
    (define-key map (kbd "C-c C-+") #'sweeprolog-increment-numbered-variables)
    (define-key map (kbd "C-M-^")   #'kill-backward-up-list)
    (define-key map (kbd "C-M-m")   #'sweeprolog-insert-term-dwim)
    (define-key map (kbd "M-p")     #'sweeprolog-backward-predicate)
    (define-key map (kbd "M-n")     #'sweeprolog-forward-predicate)
    (define-key map (kbd "M-h")     #'sweeprolog-mark-predicate)
    map)
  "Keymap for `sweeprolog-mode'.")

(defvar sweeprolog-forward-hole-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'sweeprolog-forward-hole)
    (define-key map (kbd "C-i") #'sweeprolog-forward-hole)
    (define-key map (kbd "<backtab>") #'sweeprolog-backward-hole)
    (define-key map (kbd "C-S-i") #'sweeprolog-backward-hole)
    (define-key map (kbd "C-m") #'sweeprolog-insert-term-with-holes)
    map)
  "Repeat map for \\[sweeprolog-forward-hole].")

(defvar sweeprolog-top-level-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'sweeprolog-top-level-signal-current)
    (define-key map (kbd "C-c C-i") #'sweeprolog-forward-hole)
    map)
  "Keymap for `sweeprolog-top-level-mode'.")

(defvar sweeprolog-top-level-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'sweeprolog-top-level-menu-go-to)
    (define-key map (kbd "k")   #'sweeprolog-top-level-menu-kill)
    (define-key map (kbd "t")   #'sweeprolog-top-level-menu-new)
    (define-key map (kbd "s")   #'sweeprolog-top-level-menu-signal)
    map)
  "Local keymap for `sweeprolog-top-level-menu-mode' buffers.")

;;;###autoload
(defvar sweeprolog-help-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'sweeprolog-describe-module)
    (define-key map "p" #'sweeprolog-describe-predicate)
    map)
  "Keymap for `sweeprolog' help commands.")

;;;###autoload
(defvar sweeprolog-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "B" #'sweeprolog-list-breakpoints)
    (define-key map "F" #'sweeprolog-set-prolog-flag)
    (define-key map "P" #'sweeprolog-pack-install)
    (define-key map "R" #'sweeprolog-restart)
    (define-key map "T" #'sweeprolog-list-top-levels)
    (define-key map "X" #'sweeprolog-xref-project-source-files)
    (define-key map "e" #'sweeprolog-view-messages)
    (define-key map "h" sweeprolog-help-prefix-map)
    (define-key map "l" #'sweeprolog-load-buffer)
    (define-key map "m" #'sweeprolog-find-module)
    (define-key map "p" #'sweeprolog-find-predicate)
    (define-key map "q" #'sweeprolog-top-level-send-goal)
    (define-key map "t" #'sweeprolog-top-level)
    (define-key map "&" #'sweeprolog-async-goal)
    map)
  "Keymap for `sweeprolog' global commands.")

(defvar sweeprolog-forward-hole-on-tab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'sweeprolog-indent-or-forward-hole)
    map)
  "Keymap for moving to next hole with TAB.")

;;;; Menu bar

(easy-menu-define sweeprolog-menu (list sweeprolog-mode-map
                                        sweeprolog-top-level-mode-map)
  "`sweep' menu."
  '("Sweep"
    [ "Load Prolog Buffer" sweeprolog-load-buffer t ]
    [ "Find Prolog Module" sweeprolog-find-module t ]
    [ "Find Predicate" sweeprolog-find-predicate t ]
    [ "Export Predicate"
      sweeprolog-export-predicate
      (and (eq major-mode 'sweeprolog-mode)
           (sweeprolog-definition-at-point)) ]
    [ "Insert Test-set Template"
      sweeprolog-plunit-testset-skeleton
      (eq major-mode 'sweeprolog-mode) ]
    [ "Insert Module Template"
      auto-insert
      (eq major-mode 'sweeprolog-mode) ]
    [ "Document Predicate"
      sweeprolog-document-predicate-at-point
      (and (eq major-mode 'sweeprolog-mode)
           (sweeprolog-definition-at-point)) ]
    [ "Update Autoload Directives" sweeprolog-update-dependencies
      (eq major-mode 'sweeprolog-mode) ]
    [ "Infer Indentation Style" sweeprolog-infer-indent-style
      (eq major-mode 'sweeprolog-mode) ]
    [ "Search Term" sweeprolog-term-search
      (derived-mode-p 'sweeprolog-mode)]
    [ "Count Holes" sweeprolog-count-holes
      (derived-mode-p 'sweeprolog-mode)]
    "--"
    [ "Set Prolog Flag" sweeprolog-set-prolog-flag t ]
    [ "Install Prolog Package" sweeprolog-pack-install t ]
    "--"
    [ "Set Breakpoint" sweeprolog-set-breakpoint
      (derived-mode-p 'sweeprolog-mode) ]
    [ "Delete Breakpoint" sweeprolog-delete-breakpoint
      (sweeprolog-current-breakpoints) ]
    [ "List Breakpoints" sweeprolog-list-breakpoints t ]
    "--"
    [ "Open Top-level" sweeprolog-top-level t ]
    [ "Signal Top-level"
      sweeprolog-top-level-signal
      (seq-filter (lambda (b)
                    (with-current-buffer b
                      (and (derived-mode-p 'sweeprolog-top-level-mode)
                           sweeprolog-top-level-thread-id)))
                  (buffer-list)) ]
    [ "Send Goal to Top-level" sweeprolog-top-level-send-goal t ]
    [ "Run Async Goal" sweeprolog-async-goal t ]
    [ "Open Top-level Menu" sweeprolog-list-top-levels t ]
    "--"
    [ "Describe Predicate" sweeprolog-describe-predicate t ]
    [ "Describe Prolog Module" sweeprolog-describe-module t ]
    "--"
    [ "Update Project Cross References"
      sweeprolog-xref-project-source-files
      (project-current) ]
    "--"
    [ "Reset Sweep" sweeprolog-restart t ]
    [ "View Messages" sweeprolog-view-messages t ]
    [ "Read the Sweep Manual" sweeprolog-info-manual t]
    [ "Sweep News" sweeprolog-view-news t]))


;;;; Local variables

(defvar-local sweeprolog--diagnostics nil)

(defvar-local sweeprolog--diagnostics-report-fn nil)

(defvar-local sweeprolog--diagnostics-changes-beg nil)

(defvar-local sweeprolog--diagnostics-changes-end nil)

(defvar-local sweeprolog--timer nil)

(defvar-local sweeprolog--analyze-buffer-duration 0.2)

(defvar-local sweeprolog--html-footnotes nil)

(defvar-local sweeprolog-top-level-timer nil "Buffer-local timer.")

(defvar-local sweeprolog-top-level-thread-id nil
  "Prolog top-level thread ID corresponding to this buffer.")

(defvar-local sweeprolog--buffer-last-modified-time nil)

(defvar-local sweeprolog--buffer-modified nil)

(defvar-local sweeprolog--analyze-point nil)


;;;; Declarations for functions defined in `sweep-module'

(declare-function sweeprolog-initialize    "sweep-module")
(declare-function sweeprolog-initialized-p "sweep-module")
(declare-function sweeprolog-open-query    "sweep-module")
(declare-function sweeprolog-next-solution "sweep-module")
(declare-function sweeprolog-cut-query     "sweep-module")
(declare-function sweeprolog-close-query   "sweep-module")
(declare-function sweeprolog-cleanup       "sweep-module")


;;;; Initialization

(defun sweeprolog--load-module (line)
  (save-match-data
    (when (string-match (rx bos
                            (or "L" "M")
                            (one-or-more whitespace)
                            (group-n 1 (one-or-more not-newline))
                            eos)
                        line)
      (module-load (match-string 1 line)))))

(defun sweeprolog--ensure-module ()
  "Locate and load `sweep-module', unless already loaded."
  (unless (featurep 'sweep-module)
    (let ((sweep-pl (expand-file-name
                     "sweep.pl"
                     sweeprolog--directory)))
      (unless (file-readable-p sweep-pl)
        (error "Missing file `sweep.pl' in `sweeprolog' directory"))
      (if-let ((lines (save-match-data
                        (split-string
                         (with-output-to-string
                           (with-current-buffer standard-output
                             (call-process
                              (or sweeprolog-swipl-path "swipl")
                              nil '(t nil) nil
                              "-q" "-g" "write_sweep_module_location"
                              "-t" "halt"
                              sweep-pl)))
                         "\n" t))))
          (mapc #'sweeprolog--load-module lines)
        (error (concat "Failed to locate `sweep-module'. "
                       "Make sure SWI-Prolog is installed "
                       "and up to date"))))))

(defun sweeprolog-ensure-initialized ()
  (sweeprolog--ensure-module)
  (sweeprolog-init))

(defun sweeprolog-init (&rest args)
  "Initialize and setup the embedded Prolog runtime.

If specified, ARGS should be a list of string passed to Prolog as
extra initialization arguments."
  (unless sweeprolog--initialized
    (apply #'sweeprolog-initialize
           (cons (or sweeprolog-swipl-path (executable-find "swipl"))
                 (append sweeprolog-init-args
                         (append sweeprolog--extra-init-args
                                 args))))
    (setq sweeprolog--initialized t)
    (sweeprolog-setup-message-hook)))

(defun sweeprolog-restart (&rest args)
  "Restart the embedded Prolog runtime.

ARGS is a list of strings appended to the value of
`sweeprolog-init-args' to produce the Prolog initialization
arguments.

Interactively, with a prefix arguments, prompt for ARGS.
Otherwise set ARGS to nil."
  (interactive
   (and
    current-prefix-arg
    (fboundp 'split-string-shell-command)
    (split-string-shell-command (read-string "swipl arguments: "))))
  (when-let ((top-levels (seq-filter (lambda (buffer)
                                       (eq 'sweeprolog-top-level-mode
                                           (buffer-local-value 'major-mode
                                                               buffer)))
                                     (buffer-list))))
    (if (y-or-n-p "Stop running sweep top-level processes?")
        (dolist (buffer top-levels)
          (let ((process (get-buffer-process buffer)))
            (when (process-live-p process)
              (delete-process process))))
      (user-error "Cannot restart sweep with running top-level processes")))
  (message "Stoping sweep.")
  (sweeprolog-cleanup)
  (setq sweeprolog--initialized       nil
        sweeprolog-prolog-server-port nil)
  (message "Starting sweep.")
  (apply #'sweeprolog-init args))

(defun sweeprolog--open-query (ctx mod fun arg &optional rev)
  "Ensure that Prolog is initialized and execute a new query.

CTX, MOD and FUN are strings.  CTX is the context Prolog module
in which the query in invoked.  MOD is the Prolog module in which
the invoked predicate is defined.  FUN is the functor of the
invoked predicate.

ARG is converted to a Prolog term and used as the input argument
for the query.  When REV is a nil, the input argument is the
first argument, and the output argument is second.  Otherwise,
the order of the arguments is reversed."
  (sweeprolog-ensure-initialized)
  (sweeprolog-open-query ctx mod fun arg rev))

(define-error 'prolog-exception "Prolog exception")

(defun sweeprolog--query-once (mod pred arg &optional rev)
  (sweeprolog--open-query "user" mod pred arg rev)
  (let ((sol (sweeprolog-next-solution)))
    (sweeprolog-close-query)
    (pcase sol
      (`(exception . ,exception-term)
       (signal 'prolog-exception exception-term))
      (`(,_ . ,result) result))))

(defun sweeprolog-start-prolog-server ()
  "Start the `sweep' Prolog top-level embedded server."
  (setq sweeprolog-prolog-server-port
        (sweeprolog--query-once "sweep" "sweep_top_level_server" nil)))

(defun sweeprolog-setup-message-hook ()
  "Setup `thread_message_hook/3' to redirecet Prolog messages."
  (with-current-buffer (get-buffer-create sweeprolog-messages-buffer-name)
    (setq-local window-point-insertion-type t)
    (compilation-minor-mode 1))
  (sweeprolog--query-once "sweep" "sweep_setup_message_hook" nil))


;;;; Prolog messages

(defface sweeprolog-debug-prefix
  '((default :inherit shadow))
  "Face used to highlight the \"DEBUG\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-debug-prefix-face 'sweeprolog-debug-prefix
  "Name of the face used to highlight the \"DEBUG\" message prefix.")

(defface sweeprolog-debug-topic
  '((default :inherit shadow))
  "Face used to highlight the topic in debug messages."
  :group 'sweeprolog-faces)

(defvar sweeprolog-debug-topic-face 'sweeprolog-debug-topic
  "Name of the face used to highlight the topic in debug messages.")

(defface sweeprolog-info-prefix
  '((default :inherit default))
  "Face used to highlight the \"INFO\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-info-prefix-face 'sweeprolog-info-prefix
  "Name of the face used to highlight the \"INFO\" message prefix.")

(defface sweeprolog-warning-prefix
  '((default :inherit font-lock-warning-face))
  "Face used to highlight the \"WARNING\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-warning-prefix-face 'sweeprolog-warning-prefix
  "Name of the face used to highlight the \"WARNING\" message prefix.")

(defface sweeprolog-error-prefix
  '((default :inherit error))
  "Face used to highlight the \"ERROR\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-error-prefix-face 'sweeprolog-error-prefix
  "Name of the face used to highlight the \"ERROR\" message prefix.")

(defun sweeprolog-view-messages ()
  "View the log of recent Prolog messages."
  (interactive)
  (with-current-buffer (get-buffer-create sweeprolog-messages-buffer-name)
    (goto-char (point-max))
    (let ((win (display-buffer (current-buffer))))
      (set-window-point win (point))
      win)))

(defun sweeprolog-message (message)
  "Emit the Prolog message MESSAGE to the `sweep' messages buffer."
  (with-current-buffer (get-buffer-create sweeprolog-messages-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (let ((kind (car message))
            (content (cdr message)))
        (pcase kind
          (`("debug" . ,topic)
           (insert (propertize "DEBUG" 'face sweeprolog-debug-prefix-face))
           (insert "[")
           (insert (propertize topic 'face sweeprolog-debug-topic-face))
           (insert "]: ")
           (insert content))
          ("informational"
           (insert (propertize "INFO" 'face sweeprolog-info-prefix-face))
           (insert ": ")
           (insert content))
          ("warning"
           (insert (propertize "WARNING" 'face sweeprolog-warning-prefix-face))
           (insert ": ")
           (insert content))
          ("error"
           (insert (propertize "ERROR" 'face sweeprolog-error-prefix-face))
           (insert ": ")
           (insert content))))
      (newline))))


;;;; Flags

(defun sweeprolog-current-prolog-flags (&optional prefix)
  "Return the list of defined Prolog flags defined with prefix PREFIX."
  (sweeprolog--query-once "sweep" "sweep_current_prolog_flags" (or prefix "")))

(defun sweeprolog-read-prolog-flag ()
  "Read a Prolog flag from the minibuffer, with completion."
  (let* ((col (sweeprolog-current-prolog-flags))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (let* ((val (cdr (assoc-string key col))))
                    (if val
                        (concat (make-string
                                 (max (- 32 (length key)) 1) ? )
                                val)
                      nil))))))
    (completing-read sweeprolog-read-flag-prompt col)))

(defun sweeprolog-set-prolog-flag (flag value)
  "Set the Prolog flag FLAG to VALUE.
FLAG and VALUE are specified as strings and read as Prolog terms."
  (interactive (let ((f (sweeprolog-read-prolog-flag)))
                 (list f (read-string (concat "Set " f " to: ")))))
  (if (sweeprolog--query-once "sweep" "sweep_set_prolog_flag" (cons flag value))
      (message "Prolog flag %s set to %s" flag value)
    (user-error "Setting %s to %s failed!" flag value)))


;;;; Predicates

(defun sweeprolog-predicates-collection (&optional prefix)
  "Return a list of prediacte completion candidates matchitng PREFIX."
  (sweeprolog--query-once "sweep" "sweep_predicates_collection" prefix))

;;;###autoload
(defun sweeprolog-xref-project-source-files (&optional project)
  "Update cross reference data for all Prolog files in PROJECT.

If PROJECT is nil, update data for the current project.

If called interactively with a prefix argument, prompt for
PROJECT (only on Emacs 28 or later)."
  (interactive (list (or (and current-prefix-arg
                              (fboundp 'project-prompt-project-dir)
                              (let ((default-directory
                                     (project-prompt-project-dir)))
                                (project-current)))
                         (or (project-current)
                             (user-error "No current project")))))
  (when-let ((proj (or project (project-current)))
             (files (seq-filter
                     (lambda (path)
                       (string= "pl" (file-name-extension path)))
                     (project-files proj))))
    (dolist-with-progress-reporter
        (file (seq-filter (lambda (file)
                            (string= "pl" (file-name-extension file)))
                          (project-files proj)))
        "Analyzing Prolog files in project... "
      (sweeprolog--query-once "sweep" "sweep_xref_source" file))))

(defun sweeprolog-predicate-references (mfn)
  "Find source locations where the predicate MFN is called."
  (sweeprolog-xref-project-source-files)
  (sweeprolog--query-once "sweep" "sweep_predicate_references" mfn))

(defun sweeprolog--mfn-to-functor-arity (mfn)
  (pcase (sweeprolog--query-once "system" "term_string" mfn t)
    (`(compound ":"
                (atom . ,_)
                (compound "/"
                          (atom . ,functor)
                          ,arity))
     (cons functor arity))
    (`(compound "/"
                (atom . ,functor)
                ,arity)
     (cons functor arity))))

(defun sweeprolog--swipl-source-directory ()
  (when sweeprolog-swipl-sources
    (if (stringp sweeprolog-swipl-sources)
        sweeprolog-swipl-sources
      (when (fboundp 'project-known-project-roots)
        (car (seq-filter
              (lambda (root)
                (member (file-name-base (directory-file-name root))
                        '("swipl" "swipl-devel")))
              (project-known-project-roots)))))))

(defun sweeprolog-native-predicate-location (mfn)
  (let ((functor-arity (sweeprolog--mfn-to-functor-arity mfn)))
    (when-let ((default-directory (sweeprolog--swipl-source-directory))
               (match
                (car (xref-matches-in-files
                      (rx (or "PRED_IMPL" "FRG")
                          (zero-or-more whitespace)
                          "(\""
                          (zero-or-more whitespace)
                          (literal (car functor-arity))
                          "\""
                          (zero-or-more whitespace)
                          ","
                          (zero-or-more whitespace)
                          (literal (number-to-string (cdr functor-arity))))
                      (project-files (project-current)
                                     (list (expand-file-name "src"
                                                             default-directory))))))
               (location (if (fboundp 'xref-match-item-location)
                             (xref-match-item-location match)
                           (xref-item-location match))))
      (if (fboundp 'xref-file-location-file)
          (cons (xref-file-location-file location)
                (xref-file-location-line location))
        (with-slots ((file file)
                     (line line))
            location
          (cons file line))))))

(defun sweeprolog-predicate-location (mfn)
  "Return the source location where the predicate MFN is defined.

For native built-in predicates, the behavior of this function
depends on the value of the user option
`sweeprolog-swipl-sources', which see."
  (or (sweeprolog--query-once "sweep" "sweep_predicate_location" mfn)
      (sweeprolog-native-predicate-location mfn)))

(defun sweeprolog-predicate-apropos (pattern)
  "Return a list of predicates whose name resembeles PATTERN."
  (sweeprolog--query-once "sweep" "sweep_predicate_apropos" pattern))

(defun sweeprolog-read-functor (&optional arity)
  "Read a Prolog functor/arity pair from the minibuffer.

If ARITY is nil, prompt for the arity after reading the functor.
Otherwise, read only the functor, with completion candidates are
restricted to functors with arity ARITY, and return ARITY as the
arity.

Return a cons cell of the functor as a string and the arity."
  (let* ((col (sweeprolog--query-once "sweep" "sweep_current_functors"
                                      arity))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (when-let ((val (cdr (assoc-string key col))))
                    (concat "/" (number-to-string val)))))))
    (let ((functor (completing-read "Functor: "
                                    col nil nil nil
                                    'sweeprolog-read-functor-history)))
      (cons functor
            (or arity (read-number (concat functor "/")))))))

(defun sweeprolog-read-predicate (&optional prompt)
  "Read a Prolog predicate from the minibuffer with prompt PROMPT.
If PROMPT is nil, `sweeprolog-read-predicate-prompt' is used by
default."
  (let* ((col (sweeprolog-predicates-collection))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (let* ((val (cdr (assoc-string key col))))
                    (if val
                        (concat (make-string (- 64 (length key)) ? ) (car val))
                      nil))))))
    (completing-read (or prompt sweeprolog-read-predicate-prompt)
                     col nil nil nil
                     'sweeprolog-read-predicate-history
                     (sweeprolog-identifier-at-point))))

(defun sweeprolog-predicate-prefix-boundaries (&optional point)
  (let ((case-fold-search nil))
    (save-mark-and-excursion
      (save-match-data
        (when point (goto-char point))
        (unless (bobp) (backward-char))
        (while (looking-at-p "[[:alnum:]_]")
          (backward-char))
        (when (looking-at-p ":[[:lower:]]")
          (unless (bobp) (backward-char))
          (while (looking-at-p "[[:alnum:]_]")
            (backward-char)))
        (forward-char)
        (when (looking-at-p "[[:lower:]]")
          (let ((start (point)))
            (while (looking-at-p "[[:alnum:]:_]")
              (forward-char))
            (cons start (point))))))))

;;;###autoload
(defun sweeprolog-find-predicate (mfn)
  "Jump to the definition of the Prolog predicate MFN.
MFN must be a string of the form \"M:F/N\" where M is a Prolog
module name, F is a functor name and N is its arity."
  (interactive (list (sweeprolog-read-predicate)))
  (if-let ((loc (sweeprolog-predicate-location mfn)))
      (let ((path (car loc))
            (line (or (cdr loc) 1)))
        (find-file path)
        (goto-char (point-min))
        (forward-line (1- line)))
    (user-error "Unable to locate predicate %s" mfn)))

(defun sweeprolog-identifier-at-point (&optional point)
  (when (derived-mode-p 'sweeprolog-mode 'sweeprolog-top-level-mode)
    (setq point (or point (point)))
    (save-excursion
      (goto-char point)
      (let ((id-at-point nil))
        (sweeprolog-analyze-term-at-point
         (lambda (beg end arg)
           (when (<= beg point end)
             (pcase arg
               ((or `("head_term" ,_ ,f ,a)
                    `("goal_term" ,_ ,f ,a)
                    `("head" ,_ ,f ,a)
                    `("goal" ,_ ,f ,a))
                (setq id-at-point (list f a)))))))
        (when (and id-at-point
                   (not (eq (car id-at-point) 'variable)))
          (sweeprolog--query-once "sweep" "sweep_functor_arity_pi"
                                  (append id-at-point (buffer-file-name))))))))


;;;; Modules

(defun sweeprolog-modules-collection ()
  (sweeprolog--query-once "sweep" "sweep_modules_collection" nil))

(defun sweeprolog-module-path (mod)
  (sweeprolog--query-once "sweep" "sweep_module_path" mod))

(defun sweeprolog-read-module-name ()
  "Read a Prolog module name from the minibuffer, with completion."
  (let* ((col (sweeprolog-modules-collection))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (let* ((val (cdr (assoc-string key col)))
                         (pat (car val))
                         (des (cdr val)))
                    (concat (make-string (max 0 (- 32 (length key))) ? )
                            (if des
                                (concat pat (make-string (max 0 (- 80 (length pat))) ? ) des)
                              pat)))))))
    (completing-read sweeprolog-read-module-prompt col nil nil nil
                     'sweeprolog-read-module-history)))

;;;###autoload
(defun sweeprolog-find-module (mod &optional other-window)
  "Jump to the source file of the Prolog module MOD.

If OTHER-WINDOW is non-nil, find it in another window.

Interactively, OTHER-WINDOW is the prefix argument and this
command prompts for MOD."
  (interactive (list (sweeprolog-read-module-name)))
  (let ((file (sweeprolog-module-path mod)))
    (if other-window
        (find-file-other-window file)
      (find-file file))))


;;;; Completion at point

(defvar sweeprolog-completion-at-point-functions
  '(sweeprolog-atom-completion-at-point
    sweeprolog-predicate-completion-at-point
    sweeprolog-variable-completion-at-point))

(defun sweeprolog-atoms-collection (&optional sub)
  "Return a list of atom completion candidates matchitng SUB."
  (sweeprolog--query-once "sweep" "sweep_atom_collection" sub))

(defun sweeprolog-local-variables-collection (&rest exclude)
  "Return a list of variable names that occur in the current clause.

EXCLUDE is a list of variables name to be excluded from the
resulting list even when found in the current clause."
  (let* ((beg (save-mark-and-excursion
                (unless (sweeprolog-at-beginning-of-top-term-p)
                  (sweeprolog-beginning-of-top-term))
                (max (1- (point)) (point-min))))
         (end (save-mark-and-excursion
                (sweeprolog-end-of-top-term)
                (point)))
         (vars nil))
    (save-excursion
      (goto-char beg)
      (save-match-data
        (while (search-forward-regexp (rx bow (or "_" upper)
                                          (* alnum))
                                      end t)
          (unless (nth 8 (syntax-ppss))
            (let ((match (match-string-no-properties 0)))
              (unless (or (member match exclude)
                          (member match vars))
                (push (match-string-no-properties 0) vars)))))))
    vars))

(defun sweeprolog--char-uppercase-p (char)
  (if (fboundp 'char-uppercase-p)
      (char-uppercase-p char)
    (cond ((unicode-property-table-internal 'lowercase)
           (characterp (get-char-code-property char 'lowercase)))
          ((and (>= char ?A) (<= char ?Z))))))

(defun sweeprolog-variable-completion-at-point ()
  "Prolog variable name completion backend for `completion-at-point'."
  (when-let ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (car bounds))
             (end (cdr bounds)))
    (when (and (<= beg (point) end)
               (let ((first (char-after beg)))
                 (or (sweeprolog--char-uppercase-p first)
                     (= first ?_))))
      (when-let ((col (sweeprolog-local-variables-collection
                       (buffer-substring-no-properties beg end))))
        (list beg end col
              :exclusive 'no
              :annotation-function
              (lambda (_) " Var"))))))

(defun sweeprolog-atom-completion-at-point ()
  "Prolog atom name completion backend for `completion-at-point'."
  (when-let ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (car bounds))
             (end (cdr bounds)))
    (when (and (<= beg (point) end)
               (let ((first (char-after beg)))
                 (not (or (sweeprolog--char-uppercase-p first)
                          (= first ?_)))))
      (when-let ((sub (buffer-substring-no-properties beg end))
                 (col (seq-filter (lambda (atom)
                                    (not (string= atom sub)))
                                  (sweeprolog-atoms-collection sub))))
        (list beg end col
              :exclusive 'no
              :annotation-function
              (lambda (_) " atom"))))))

(defun sweeprolog--parse-context (&optional point)
  (save-excursion
    (sweeprolog-backward-term 0)
    (let ((pos (or point (point)))
          (commas 0)
          (context nil))
      (while
          (pcase (sweeprolog-last-token-boundaries pos)
            ('nil nil)
            (`(open ,obeg ,oend)
             (push (cons (buffer-substring-no-properties obeg oend)
                         (1+ commas))
                   context)
             (setq pos obeg)
             (setq commas 0))
            (`(functor ,obeg ,oend)
             (push (cons (buffer-substring-no-properties obeg (1- oend))
                         (1+ commas))
                   context)
             (setq pos obeg)
             (setq commas 0))
            ((or `(operator ,obeg ,oend)
                 `(symbol ,obeg ,oend))
             (let* ((op (buffer-substring-no-properties obeg oend))
                    (ipre (sweeprolog-op-infix-precedence op))
                    (ppre (sweeprolog-op-prefix-precedence op)))
               (cond
                ((and (string= "." op)
                      (or (not (char-after (1+ obeg)))
                          (member (char-syntax (char-after (1+ obeg)))
                                  '(?> ? ))))
                 nil)
                ((string= "," op)
                 (setq pos
                       (save-excursion
                         (goto-char obeg)
                         (setq commas (1+ commas))
                         (sweeprolog-backward-term 999)
                         (point))))
                (ipre
                 (push (cons op 2) context)
                 (setq pos
                       (save-excursion
                         (goto-char obeg)
                         (sweeprolog-backward-term (1- ipre))
                         (point)))
                 (setq commas 0))
                (ppre
                 (push (cons op 1) context)
                 (setq pos obeg)
                 (setq commas 0)))))))
      context)))

(defun sweeprolog-context-callable-p ()
  "Check if point is in a position where a goal should appear."
  (sweeprolog--query-once "sweep" "sweep_context_callable"
                          (sweeprolog--parse-context)))

(defun sweeprolog-predicate-completion-at-point ()
  "Prolog predicate completion backend for `completion-at-point'."
  (when-let ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (car bounds))
             (end (cdr bounds)))
    (when (and (<= beg (point) end)
               (let ((first (char-after beg)))
                 (not (or (sweeprolog--char-uppercase-p first)
                          (= first ?_)))))
      (when-let
          ((col (sweeprolog--query-once
                 "sweep" "sweep_predicate_completion_candidates"
                 (cons (sweeprolog-context-callable-p)
                       (buffer-substring-no-properties beg end)))))
        (list beg end col
              :exclusive 'no
              :annotation-function
              (lambda (_) " Predicate")
              :exit-function
              (lambda (string status)
                (pcase status
                  ('finished
                   (pcase (cdr (assoc-string string col))
                     (`(compound
                        "term_position"
                        0 ,length
                        ,_fbeg ,_fend
                        ,holes)
                      (with-silent-modifications
                        (dolist (hole holes)
                          (pcase hole
                            (`(compound "-" ,hbeg ,hend)
                             (add-text-properties
                              (- (point) length (- hbeg))
                              (- (point) length (- hend))
                              (list
                               'sweeprolog-hole t
                               'font-lock-face (list (sweeprolog-hole-face))
                               'rear-nonsticky '(sweeprolog-hole
                                                 cursor-sensor-functions
                                                 font-lock-face)))))))
                      (backward-char length)
                      (sweeprolog-forward-hole)))))))))))


;;;; Packages

(defun sweeprolog-packs-collection ()
  (sweeprolog--query-once "sweep" "sweep_packs_collection" ""))

(defun sweeprolog-read-pack-name ()
  "Read a Prolog pack name from the minibuffer, with completion."
  (let* ((col (sweeprolog-packs-collection))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (let* ((val (cdr (assoc-string key col)))
                         (des (car val))
                         (ver (cadr val)))
                    (concat (make-string (max 0 (- 32 (length key))) ? )
                            (if des
                                (concat ver (make-string (max 0 (- 16 (length ver))) ? ) des)
                              ver)))))))
    (completing-read sweeprolog-read-pack-prompt col)))

;;;###autoload
(defun sweeprolog-pack-install (pack)
  "Install or upgrade Prolog package PACK."
  (interactive (list (sweeprolog-read-pack-name)))
  (if (sweeprolog--query-once "sweep" "sweep_pack_install" pack)
      (message "Package install successful.")
    (user-error "Pacakge installation failed!")))


;;;; Faces

(defgroup sweeprolog-faces nil
  "Faces used to highlight Prolog code."
  :group 'sweeprolog)

(defcustom sweeprolog-faces-style nil
  "Style of faces to use for highlighting Prolog code."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Light"   light)
                 (const :tag "Dark"    dark))
  :package-version '((sweeprolog . "0.3.2"))
  :group 'sweeprolog-faces)

(eval-when-compile
  (defmacro sweeprolog-defface (name def light dark doc)
    "Define sweeprolog face FACE with doc DOC."
    (declare
     (indent defun)
     (doc-string 5))
    (let* ((sn (symbol-name name))
           (func (intern (concat "sweeprolog-" sn "-face")))
           (facd (intern (concat "sweeprolog-" sn "-dark")))
           (facl (intern (concat "sweeprolog-" sn "-light")))
           (face (intern (concat "sweeprolog-" sn))))
      `(progn
         (defface ,facl
           '((default              . ,light))
           ,(concat "Light face used to highlight " (downcase doc))
           :group 'sweeprolog-faces)
         (defface ,facd
           '((default              . ,dark))
           ,(concat "Dark face used to highlight " (downcase doc))
           :group 'sweeprolog-faces)
         (defface ,face
           '((default              . ,def))
           ,(concat "Face used to highlight " (downcase doc))
           :group 'sweeprolog-faces)
         (defun ,func ()
           ,(concat "Return the face used to highlight " (downcase doc))
           (pcase sweeprolog-faces-style
             ('light ',facl)
             ('dark  ',facd)
             (_      ',face)))))))

(sweeprolog-defface
  function
  (:inherit font-lock-function-name-face)
  (:foreground "blue")
  (:foreground "cyan")
  "Arithmetic functions.")

(sweeprolog-defface
  no-function
  (:inherit font-lock-warning-face)
  (:foreground "red")
  (:foreground "red")
  "Unknown arithmetic functions.")

(sweeprolog-defface
 functor
 (:inherit font-lock-function-name-face)
 (:foreground "navyblue")
 (:foreground "darkcyan")
 "Functors.")

(sweeprolog-defface
  arity
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "darkcyan")
  "Arities.")

(sweeprolog-defface
  predicate-indicator
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "darkcyan")
  "Predicate indicators.")

(sweeprolog-defface
  built-in
  (:inherit font-lock-keyword-face)
  (:foreground "blue")
  (:foreground "cyan")
  "Built in predicate calls.")

(sweeprolog-defface
  neck
  (:inherit font-lock-preprocessor-face)
  (:weight bold)
  (:weight bold)
  "Necks.")

(sweeprolog-defface
  goal
  (:inherit font-lock-function-name-face)
  (:inherit font-lock-function-name-face)
  (:inherit font-lock-function-name-face)
  "Unspecified predicate goals.")

(sweeprolog-defface
  string
  (:inherit font-lock-string-face)
  (:foreground "navyblue")
  (:foreground "palegreen")
  "Strings.")

(sweeprolog-defface
  comment
  (:inherit font-lock-comment-face)
  (:foreground "darkgreen")
  (:foreground "green")
  "Comments.")

(sweeprolog-defface
  head-built-in
  (:background "orange" :weight bold)
  (:background "orange" :weight bold)
  (:background "orange" :weight bold)
  "Built-in predicate definitons.")

(sweeprolog-defface
 method
  (:weight bold)
  (:weight bold)
  (:weight bold)
  "PCE classes.")

(sweeprolog-defface
  class
  (:underline t)
  (:underline t)
  (:underline t)
  "PCE classes.")

(sweeprolog-defface
  no-file
  (:foreground "red")
  (:foreground "red")
  (:foreground "red")
  "Non-existsing file specifications.")

(sweeprolog-defface
  head-local
  (:inherit font-lock-builtin-face)
  (:weight bold)
  (:weight bold)
  "Local predicate definitions.")

(sweeprolog-defface
  head-meta
  (:inherit font-lock-preprocessor-face)
  nil nil
  "Meta predicate definitions.")

(sweeprolog-defface
  head-dynamic
  (:inherit font-lock-constant-face)
  (:foreground "magenta" :weight bold)
  (:foreground "magenta" :weight bold)
  "Dynamic predicate definitions.")

(sweeprolog-defface
  head-multifile
  (:inherit font-lock-type-face)
  (:foreground "navyblue" :weight bold)
  (:foreground "palegreen" :weight bold)
  "Multifile predicate definitions.")

(sweeprolog-defface
  head-extern
  (:inherit font-lock-type-face)
  (:foreground "blue" :weight bold)
  (:foreground "cyan" :weight bold)
  "External predicate definitions.")

(sweeprolog-defface
  head-test
  (:inherit font-lock-preprocessor-face)
  (:foreground "#01bdbd" :weight bold)
  (:foreground "#01bdbd" :weight bold)
  "Unreferenced predicate definitions.")

(sweeprolog-defface
  head-unreferenced
  (:inherit font-lock-warning-face)
  (:foreground "red" :weight bold)
  (:foreground "red" :weight bold)
  "Unreferenced predicate definitions.")

(sweeprolog-defface
  head-exported
  (:inherit font-lock-builtin-face)
  (:foreground "blue" :weight bold)
  (:foreground "cyan" :weight bold)
  "Exported predicate definitions.")

(sweeprolog-defface
  head-hook
  (:inherit font-lock-type-face)
  (:foreground "blue" :underline t)
  (:foreground "cyan" :underline t)
  "Hook definitions.")

(sweeprolog-defface
  head-iso
  (:inherit font-lock-keyword-face)
  (:background "orange" :weight bold)
  (:background "orange" :weight bold)
  "ISO specified predicate definitions.")

(sweeprolog-defface
  head-def-iso
  (:inherit font-lock-builtin-face)
  (:foreground "blue" :weight bold)
  (:foreground "cyan" :weight bold)
  "Built-in ISO specified predicate definitions.")

(sweeprolog-defface
  head-def-swi
  (:inherit font-lock-builtin-face)
  (:foreground "blue" :weight bold)
  (:foreground "cyan" :weight bold)
  "Built-in SWI-Prolog predicate definitions.")

(sweeprolog-defface
  head-imported
  (:inherit font-lock-function-name-face)
  (:foreground "darkgoldenrod4" :weight bold)
  (:foreground "darkgoldenrod4" :weight bold)
  "Imported head terms.")

(sweeprolog-defface
  head-undefined
  (:inherit font-lock-warning-face)
  (:weight bold)
  (:weight bold)
  "Undefind head terms.")

(sweeprolog-defface
  head-public
  (:inherit font-lock-builtin-face)
  (:foreground "#016300" :weight bold)
  (:foreground "#016300" :weight bold)
  "Public definitions.")

(sweeprolog-defface
  head-constraint
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "palegreen")
  "Constraint definitions.")

(sweeprolog-defface
  meta-spec
  (:inherit font-lock-preprocessor-face)
  (:inherit font-lock-preprocessor-face)
  (:inherit font-lock-preprocessor-face)
  "Meta argument specifiers.")

(sweeprolog-defface
  recursion
  (:inherit font-lock-builtin-face)
  (:underline t)
  (:underline t)
  "Recursive calls.")

(sweeprolog-defface
  local
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "darkcyan")
  "Local predicate calls.")

(sweeprolog-defface
  expanded
  (:inherit font-lock-function-name-face)
  (:foreground "blue" :underline t)
  (:foreground "cyan" :underline t)
  "Expanded predicate calls.")

(sweeprolog-defface
  autoload
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "darkcyan")
  "Autoloaded predicate calls.")

(sweeprolog-defface
  imported
  (:inherit font-lock-function-name-face)
  (:foreground "blue")
  (:foreground "cyan")
  "Imported predicate calls.")

(sweeprolog-defface
  extern
  (:inherit font-lock-function-name-face)
  (:foreground "blue" :underline t)
  (:foreground "cyan" :underline t)
  "External predicate calls.")

(sweeprolog-defface
  foreign
  (:inherit font-lock-keyword-face)
  (:foreground "darkturquoise")
  (:foreground "darkturquoise")
  "Foreign predicate calls.")

(sweeprolog-defface
  meta
  (:inherit font-lock-type-face)
  (:foreground "red4")
  (:foreground "red4")
  "Meta predicate calls.")

(sweeprolog-defface
  undefined
  (:inherit font-lock-warning-face)
  (:foreground "red")
  (:foreground "orange")
  "Undefined predicate calls.")

(sweeprolog-defface
  thread-local
  (:inherit font-lock-constant-face)
  (:foreground "magenta" :underline t)
  (:foreground "magenta" :underline t)
  "Thread local predicate calls.")

(sweeprolog-defface
  not-callable
  (:inherit font-lock-warning-face)
  (:background "orange")
  (:background "orange")
  "Terms that are not callable.")

(sweeprolog-defface
  constraint
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "palegreen")
  "Constraint calls.")

(sweeprolog-defface
  global
  (:inherit font-lock-keyword-face)
  (:foreground "magenta")
  (:foreground "darkcyan")
  "Global predicate calls.")

(sweeprolog-defface
  multifile
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "palegreen")
  "Multifile predicate calls.")

(sweeprolog-defface
  dynamic
  (:inherit font-lock-constant-face)
  (:foreground "magenta")
  (:foreground "magenta")
  "Dynamic predicate calls.")

(sweeprolog-defface
  undefined-import
  (:inherit font-lock-warning-face)
  (:foreground "red")
  (:foreground "red")
  "Undefined imports.")

(sweeprolog-defface
  html-attribute
  (:inherit font-lock-function-name-face)
  (:foreground "magenta4")
  (:foreground "magenta4")
  "HTML attributes.")

(sweeprolog-defface
  html-call
  (:inherit font-lock-keyword-face)
  (:foreground "magenta4" :weight bold)
  (:foreground "magenta4" :weight bold)
  "HTML calls.")

(sweeprolog-defface
  option-name
  (:inherit font-lock-constant-face)
  (:foreground "#3434ba")
  (:foreground "#3434ba")
  "Option names.")

(sweeprolog-defface
  no-option-name
  (:inherit font-lock-warning-face)
  (:foreground "red")
  (:foreground "orange")
  "Non-existent option names.")

(sweeprolog-defface
  flag-name
  (:inherit font-lock-constant-face)
  (:foreground "blue")
  (:foreground "cyan")
  "Flag names.")

(sweeprolog-defface
  no-flag-name
  (:inherit font-lock-warning-face)
  (:foreground "red")
  (:foreground "red")
  "Non-existent flag names.")

(sweeprolog-defface
  qq-type
  (:inherit font-lock-type-face)
  (:weight bold)
  (:weight bold)
  "Quasi-quotation types.")

(sweeprolog-defface
  qq-sep
  (:inherit font-lock-type-face)
  (:weight bold)
  (:weight bold)
  "Quasi-quotation separators.")

(sweeprolog-defface
  qq-open
  (:inherit font-lock-type-face)
  (:weight bold)
  (:weight bold)
  "Quasi-quotation open sequences.")

(sweeprolog-defface
  qq-content
  nil
  (:foreground "red4")
  (:foreground "red4")
  "Quasi-quotation content.")

(sweeprolog-defface
  qq-close
  (:inherit font-lock-type-face)
  (:weight bold)
  (:weight bold)
  "Quasi-quotation close sequences.")

(sweeprolog-defface
  op-type
  (:inherit font-lock-type-face)
  (:foreground "blue")
  (:foreground "cyan")
  "Operator types.")

(sweeprolog-defface
  dict-tag
  (:inherit font-lock-constant-face)
  (:weight bold)
  (:weight bold)
  "Dict tags.")

(sweeprolog-defface
  dict-key
  (:inherit font-lock-keyword-face)
  (:weight bold)
  (:weight bold)
  "Dict keys.")

(sweeprolog-defface
  dict-sep
  (:inherit font-lock-keyword-face)
  (:weight bold)
  (:weight bold)
  "Dict separators.")

(sweeprolog-defface
  dict-return-op
  (:inherit font-lock-preprocessor-face)
  (:foreground "blue")
  (:foreground "cyan")
  "Dict return operators.")

(sweeprolog-defface
  dict-function
  (:inherit font-lock-function-name-face)
  (:foreground "navyblue")
  (:foreground "darkcyan")
  "Dict functions.")

(sweeprolog-defface
  func-dot
  (:inherit font-lock-preprocessor-face)
  (:weight bold)
  (:weight bold)
  "Dict function dots.")

(sweeprolog-defface
  file
  (:inherit button)
  (:foreground "blue" :underline t)
  (:foreground "cyan" :underline t)
  "File specifiers.")

(sweeprolog-defface
  file-no-depend
  (:inherit font-lock-warning-face)
  (:foreground "blue" :underline t :background "pink")
  (:foreground "cyan" :underline t :background "pink")
  "Unused file specifiers.")

(sweeprolog-defface
  unused-import
  (:inherit font-lock-warning-face)
  (:foreground "blue" :background "pink")
  (:foreground "cyan" :background "pink")
  "Unused imports.")

(sweeprolog-defface
  identifier
  (:inherit font-lock-type-face)
  (:weight bold)
  (:weight bold)
  "Identifiers.")

(sweeprolog-defface
  hook
  (:inherit font-lock-preprocessor-face)
  (:foreground "blue" :underline t)
  (:foreground "cyan" :underline t)
  "Hooks.")

(sweeprolog-defface
  module
  (:inherit font-lock-type-face)
  (:foreground "darkslateblue")
  (:foreground "lightslateblue")
  "Module names.")

(sweeprolog-defface
  singleton
  (:inherit font-lock-warning-face)
  (:foreground "red4" :weight bold)
  (:foreground "orangered1" :weight bold)
  "Singletons.")

(sweeprolog-defface
  fullstop
  (:inherit font-lock-negation-char-face)
  (:inherit font-lock-negation-char-face)
  (:inherit font-lock-negation-char-face)
  "Fullstops.")

(sweeprolog-defface
  nil
  (:inherit font-lock-keyword-face)
  (:inherit font-lock-keyword-face)
  (:inherit font-lock-keyword-face)
  "The empty list.")

(sweeprolog-defface
  variable-at-point
  (:underline t)
  (:underline t)
  (:underline t)
  "Variables.")

(sweeprolog-defface
  variable
  (:inherit font-lock-variable-name-face)
  (:foreground "red4")
  (:foreground "orangered1")
  "Variables.")

(sweeprolog-defface
  ext-quant
  (:inherit font-lock-keyword-face)
  (:inherit font-lock-keyword-face)
  (:inherit font-lock-keyword-face)
  "Existential quantifiers.")

(sweeprolog-defface
  keyword
  (:inherit font-lock-keyword-face)
  (:foreground "blue")
  (:foreground "cyan")
  "Control constructs.")

(sweeprolog-defface
  control
  (:inherit font-lock-keyword-face)
  (:inherit font-lock-keyword-face)
  (:inherit font-lock-keyword-face)
  "Control constructs.")

(sweeprolog-defface
  atom
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
  "Atoms.")

(sweeprolog-defface
  int
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
  "Integers.")

(sweeprolog-defface
  float
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
  "Floats.")

(sweeprolog-defface
  rational
  (:inherit font-lock-constant-face)
  (:foreground "steelblue")
  (:foreground "steelblue")
  "Rationals.")

(sweeprolog-defface
  chars
  (:inherit font-lock-constant-face)
  (:foreground "navyblue")
  (:foreground "palegreen")
  "Chars.")

(sweeprolog-defface
  codes
  (:inherit font-lock-constant-face)
  (:foreground "navyblue")
  (:foreground "palegreen")
  "Codes.")

(sweeprolog-defface
  error
  (:inherit font-lock-warning-face)
  (:background "orange")
  (:background "orange")
  "Unspecified errors.")

(sweeprolog-defface
  type-error
  (:inherit font-lock-warning-face)
  (:background "orange")
  (:background "orange")
  "Type errors.")

(sweeprolog-defface
  instantiation-error
  (:inherit font-lock-warning-face)
  (:background "orange")
  (:background "orange")
  "Instantiation errors.")

(sweeprolog-defface
  syntax-error
  (:inherit error)
  (:background "orange")
  (:background "orange")
  "Syntax errors.")

(sweeprolog-defface around-syntax-error nil nil nil
  "Text around a syntax error.")

(sweeprolog-defface clause nil nil nil
  "Predicate clauses.")

(sweeprolog-defface grammar-rule nil nil nil
  "DCG grammar rules.")

(sweeprolog-defface term nil nil nil
  "Top terms.")

(sweeprolog-defface body nil nil nil
  "Clause and query bodies.")

(sweeprolog-defface directive nil nil nil
  "Directives.")

(sweeprolog-defface
  string-comment
  (:inherit font-lock-doc-face)
  (:inherit font-lock-doc-face :foreground "darkgreen")
  (:inherit font-lock-doc-face :foreground "green")
  "String comments.")

(sweeprolog-defface
  structured-comment
  (:inherit font-lock-doc-face)
  (:inherit font-lock-doc-face :foreground "darkgreen")
  (:inherit font-lock-doc-face :foreground "green")
  "Structured comments.")

(sweeprolog-defface
  hole
  (:box t)
  (:box t)
  (:box t)
  "Holes.")

;;;; Font-lock

(defun sweeprolog-analyze-start-font-lock (beg end)
  (with-silent-modifications
    (remove-list-of-text-properties beg end '(font-lock-face))))

(defun sweeprolog-analyze-start-help-echo (beg end)
  (with-silent-modifications
    (remove-list-of-text-properties beg end '(help-echo))))

(defun sweeprolog-maybe-syntax-error-face (end)
  (or (and (or (derived-mode-p 'sweeprolog-top-level-mode)
               (and sweeprolog--analyze-point
                    (<= (save-excursion
                          (goto-char sweeprolog--analyze-point)
                          (sweeprolog-beginning-of-top-term)
                          (1- (point)))
                        (1+ end) sweeprolog--analyze-point))
               (< (save-excursion
                    (goto-char sweeprolog--analyze-point)
                    (sweeprolog-end-of-top-term) (point))
                  (save-excursion
                    (goto-char sweeprolog--analyze-point)
                    (sweeprolog-beginning-of-next-top-term) (point))
                  (point-max)))
           (sweeprolog-syntax-error-face))
      (sweeprolog-around-syntax-error-face)))

(defun sweeprolog-analyze-fragment-to-faces (beg end arg)
  (pcase arg
    (`("comment" . "structured")
     (list (list beg end nil)
           (list beg end (sweeprolog-structured-comment-face))))
    (`("comment" . "string")
     (list (list beg end nil)
           (list beg end (sweeprolog-string-comment-face))))
    (`("comment" . ,_)
     (list (list beg end nil)
           (list beg end (sweeprolog-comment-face))))
    (`("head" "unreferenced" . ,_)
     (list (list beg end (sweeprolog-head-unreferenced-face))))
    (`("head" "test" . ,_)
     (list (list beg end (sweeprolog-head-test-face))))
    (`("head" "meta" . ,_)
     (list (list beg end (sweeprolog-head-meta-face))))
    (`("head" "def_iso" . ,_)
     (list (list beg end (sweeprolog-head-def-iso-face))))
    (`("head" "def_swi" . ,_)
     (list (list beg end (sweeprolog-head-def-swi-face))))
    (`("head" "iso" . ,_)
     (list (list beg end (sweeprolog-head-iso-face))))
    (`("head" "exported" . ,_)
     (list (list beg end (sweeprolog-head-exported-face))))
    (`("head" "hook" . ,_)
     (list (list beg end (sweeprolog-head-hook-face))))
    (`("head" "built_in" . ,_)
     (list (list beg end (sweeprolog-head-built-in-face))))
    (`("head" ("imported" . ,_) . ,_)
     (list (list beg end (sweeprolog-head-imported-face))))
    (`("head" ("extern" . ,_) . ,_)
     (list (list beg end (sweeprolog-head-extern-face))))
    (`("head" "public" . ,_)
     (list (list beg end (sweeprolog-head-public-face))))
    (`("head" "dynamic" . ,_)
     (list (list beg end (sweeprolog-head-dynamic-face))))
    (`("head" "multifile" . ,_)
     (list (list beg end (sweeprolog-head-multifile-face))))
    (`("head" "local" . ,_)
     (list (list beg end (sweeprolog-head-local-face))))
    (`("head" "constraint" . ,_)
     (list (list beg end (sweeprolog-head-constraint-face))))
    (`("goal" ("autoload" . ,_) . ,_)
     (list (list beg end (sweeprolog-autoload-face))))
    (`("goal" "expanded" . ,_)
     (list (list beg end (sweeprolog-expanded-face))))
    (`("goal" "recursion" . ,_)
     (list (list beg end (sweeprolog-recursion-face))))
    (`("goal" "meta"      . ,_)
     (list (list beg end (sweeprolog-meta-face))))
    (`("goal" "built_in"  . ,_)
     (list (list beg end (sweeprolog-built-in-face))))
    (`("goal" "undefined" . ,_)
     (list (list beg end (sweeprolog-undefined-face))))
    (`("goal" "global" . ,_)
     (list (list beg end (sweeprolog-global-face))))
    (`("goal" "not_callable" . ,_)
     (list (list beg end (sweeprolog-not-callable-face))))
    (`("goal" "dynamic" . ,_)
     (list (list beg end (sweeprolog-dynamic-face))))
    (`("goal" "foreign" . ,_)
     (list (list beg end (sweeprolog-foreign-face))))
    (`("goal" "multifile" . ,_)
     (list (list beg end (sweeprolog-multifile-face))))
    (`("goal" "thread_local" . ,_)
     (list (list beg end (sweeprolog-thread-local-face))))
    (`("goal" ("extern" . ,_) . ,_)
     (list (list beg end (sweeprolog-extern-face))))
    (`("goal" ("imported" . ,_) . ,_)
     (list (list beg end (sweeprolog-imported-face))))
    (`("goal" ("global" . ,_) . ,_)
     (list (list beg end (sweeprolog-global-face))))
    (`("goal" "local" . ,_)
     (list (list beg end (sweeprolog-local-face))))
    (`("goal" "constraint" . ,_)
     (list (list beg end (sweeprolog-constraint-face))))
    ("instantiation_error"
     (list (list beg end (sweeprolog-instantiation-error-face))))
    (`("type_error" . ,_)
     (list (list beg end (sweeprolog-type-error-face))))
    (`("syntax_error" ,_ ,eb ,ee)
     (let ((eb (min eb beg))
           (ee (max ee end)))
       (save-excursion
         (goto-char (min ee (point-max)))
         (let ((ws nil)
               (cur (point)))
           (while (and (forward-comment 1)
                       (forward-comment -1))
             (push (list cur (point) nil) ws)
             (forward-comment 1)
             (setq cur (point)))
           (skip-chars-forward " \t\n")
           (push (list cur (point) nil) ws)
           (setq cur (point))
           (let ((face (sweeprolog-maybe-syntax-error-face end)))
             (cons (list beg cur nil)
                   (append (list (list eb ee nil)
                                 (list eb ee (sweeprolog-around-syntax-error-face))
                                 (list beg end face))
                           ws)))))))
    ("unused_import"
     (list (list beg end (sweeprolog-unused-import-face))))
    ("undefined_import"
     (list (list beg end (sweeprolog-undefined-import-face))))
    ("error"
     (list (list beg end (sweeprolog-error-face))))
    ("keyword"
     (list (list beg end (sweeprolog-keyword-face))))
    ("html_attribute"
     (list (list beg end (sweeprolog-html-attribute-face))))
    ("html"
     (list (list beg end (sweeprolog-html-call-face))))
    ("dict_tag"
     (list (list beg end (sweeprolog-dict-tag-face))))
    ("dict_key"
     (list (list beg end (sweeprolog-dict-key-face))))
    ("dict_sep"
     (list (list beg end (sweeprolog-dict-sep-face))))
    ("dict_function"
     (list (list beg end (sweeprolog-dict-function-face))))
    ("dict_return_op"
     (list (list beg end (sweeprolog-dict-return-op-face))))
    ("func_dot"
     (list (list beg end (sweeprolog-func-dot-face))))
    ("meta"
     (list (list beg end (sweeprolog-meta-spec-face))))
    ("flag_name"
     (list (list beg end (sweeprolog-flag-name-face))))
    ("no_flag_name"
     (list (list beg end (sweeprolog-flag-name-face))))
    ("ext_quant"
     (list (list beg end (sweeprolog-ext-quant-face))))
    ("atom"
     (list (list beg end (sweeprolog-atom-face))))
    ("float"
     (list (list beg end (sweeprolog-float-face))))
    ("rational"
     (list (list beg end (sweeprolog-rational-face))))
    ("int"
     (list (list beg end (sweeprolog-int-face))))
    ("singleton"
     (list (list beg end (sweeprolog-singleton-face))))
    ("option_name"
     (list (list beg end (sweeprolog-option-name-face))))
    ("no_option_name"
     (list (list beg end (sweeprolog-no-option-name-face))))
    ("control"
     (list (list beg end (sweeprolog-control-face))))
    ("var"
     (list (list beg end (sweeprolog-variable-face))))
    ("fullstop"
     (save-excursion
       (goto-char (min end (point-max)))
       (let ((ws nil)
             (cur (point)))
         (while (and (forward-comment 1)
                     (forward-comment -1))
           (push (list cur (point) nil) ws)
           (forward-comment 1)
           (setq cur (point)))
         (skip-chars-forward " \t\n")
         (push (list cur (point) nil) ws)
         (cons (list beg end nil)
               (cons (list beg end (sweeprolog-fullstop-face))
                     ws)))))
    ("functor"
     (list (list beg end (sweeprolog-functor-face))))
    ("arity"
     (list (list beg end (sweeprolog-arity-face))))
    ("predicate_indicator"
     (list (list beg end (sweeprolog-predicate-indicator-face))))
    ("chars"
     (list (list beg end (sweeprolog-chars-face))))
    ("codes"
     (list (list beg end (sweeprolog-codes-face))))
    ("string"
     (list (list beg end (sweeprolog-string-face))))
    (`("module" . ,_)
     (list (list beg end (sweeprolog-module-face))))
    ("neck"
     (list (list beg end (sweeprolog-neck-face))))
    (`("hook" . ,_)
     (list (list beg end (sweeprolog-hook-face))))
    ("hook"
     (list (list beg end (sweeprolog-hook-face))))
    (`("qq_content" . ,type)
     (let ((mode (cdr (assoc-string type sweeprolog-qq-mode-alist))))
       (if (and mode (fboundp mode))
           (let ((res nil)
                 (string (buffer-substring-no-properties beg end)))
             (with-current-buffer
                 (get-buffer-create
                  (format " *sweep-qq-content:%s*" type))
               (with-silent-modifications
                 (erase-buffer)
                 (insert string " "))
               (unless (eq major-mode mode) (funcall mode))
               (font-lock-ensure)
               (let ((pos (point-min)) next)
                 (while (setq next (next-property-change pos))
                   (dolist (prop '(font-lock-face face))
                     (let ((new-prop (get-text-property pos prop)))
                       (when new-prop
                         (push (list (+ beg (1- pos)) (1- (+ beg next)) new-prop)
                               res))))
                   (setq pos next)))
               (set-buffer-modified-p nil)
               res))
         (list (list beg end (sweeprolog-qq-content-face))))))
    ("qq_type"
     (list (list beg end (sweeprolog-qq-type-face))))
    ("qq_sep"
     (list (list beg end (sweeprolog-qq-sep-face))))
    ("qq_open"
     (list (list beg end (sweeprolog-qq-open-face))))
    ("qq_close"
     (list (list beg end (sweeprolog-qq-close-face))))
    ("identifier"
     (list (list beg end (sweeprolog-identifier-face))))
    (`("file" . ,_)
     (list (list beg end (sweeprolog-file-face))))
    (`("file_no_depend" . ,_)
     (list (list beg end (sweeprolog-file-no-depend-face))))
    ("function"
     (list (list beg end (sweeprolog-function-face))))
    ("no_function"
     (list (list beg end (sweeprolog-no-function-face))))
    ("nofile"
     (list (list beg end (sweeprolog-no-file-face))))
    ("op_type"
     (list (list beg end (sweeprolog-op-type-face))))
    ("directive"
     (list (list beg end nil) (list beg end (sweeprolog-directive-face))))
    ("body"
     (list (list beg end nil) (list beg end (sweeprolog-body-face))))
    ("clause"
     (list (list beg end nil) (list beg end (sweeprolog-clause-face))))
    ("term"
     (list (list beg end nil) (list beg end (sweeprolog-term-face))))
    ("grammar_rule"
     (list (list beg end nil) (list beg end (sweeprolog-grammar-rule-face))))
    ("method"
     (list (list beg end nil) (list beg end (sweeprolog-method-face))))
    ("class"
     (list (list beg end (sweeprolog-class-face))))))

(defun sweeprolog-analyze-fragment-font-lock (beg end arg)
  (when-let ((face-fragments (sweeprolog-analyze-fragment-to-faces
                              beg end arg)))
    (with-silent-modifications
      (dolist (face-fragment face-fragments)
        (let ((frag-beg (car face-fragment))
              (frag-end (cadr face-fragment))
              (frag-face (caddr face-fragment)))
          (if frag-face
              (font-lock--add-text-property frag-beg frag-end
                                            'font-lock-face frag-face
                                            (current-buffer) nil)
            (remove-list-of-text-properties frag-beg frag-end
                                            '(font-lock-face))))))))

(defun sweeprolog-analyze-end-font-lock (beg end)
  (when sweeprolog-highlight-holes
    (with-silent-modifications
      (save-excursion
        (goto-char beg)
        (save-restriction
          (narrow-to-region beg end)
          (let ((hole (sweeprolog--next-hole)))
            (while hole
              (let ((hbeg (car hole))
                    (hend (cdr hole)))
                (font-lock--add-text-property hbeg hend
                                              'font-lock-face
                                              (sweeprolog-hole-face)
                                              (current-buffer)
                                              nil)
                (goto-char hend)
                (setq hole (sweeprolog--next-hole)))))))))
  (when (and sweeprolog-highlight-breakpoints
             (sweeprolog-buffer-loaded-since-last-modification-p))
    (with-silent-modifications
      (dolist (bp (sweeprolog-current-breakpoints-in-region beg end))
        (sweeprolog--highlight-breakpoint (car bp) (cdr bp))))))

(defun sweeprolog--help-echo-for-comment (kind)
  (pcase kind
    ("string" "XPCE method summary")
    ("structured" "PlDoc structured comment")
    (_ "Comment")))

(defun sweeprolog--help-echo-for-dependency (file)
  (lambda (_ buf _)
    (let ((preds
           (sweeprolog--query-once "sweep" "sweep_predicate_dependencies"
                                   (cons (buffer-file-name buf)
                                         file))))
      (format "Dependency on %s, resolves calls to %s"
              file
              (mapconcat (lambda (pi)
                           (propertize pi 'face
                                       (sweeprolog-predicate-indicator-face)))
                         preds ", ")))))

(defun sweeprolog--help-echo-for-unused-dependency (file)
  (format "Unused dependency on %s" file))

(defun sweeprolog--help-echo-for-module (module)
  (format "Module %s" module))

(defun sweeprolog--help-echo-for-type-error (error-type)
  (format "Type error (expected %s)" error-type))

(defun sweeprolog--help-echo-for-head-functor (kind functor arity)
  (pcase kind
    ("unreferenced" (format "Unreferenced predicate %s/%s head term"
                            functor arity))
    ("test" "PlUnit test")
    ("meta" (format "Meta predicate %s/%s head term"
                    functor arity))
    ("def_iso" (format "Built-in ISO specified predicate %s/%s head term"
                       functor arity))
    ("def_swi" (format "Built-in SWI-Prolog predicate %s/%s head term"
                       functor arity))
    ("iso" (format "ISO specified predicate %s/%s head term"
                   functor arity))
    ("exported" (format "Exported predicate %s/%s head term"
                        functor arity))
    ("hook" (format "Hook predicate %s/%s head term"
                    functor arity))
    ("built_in" (format "Built-in predicate %s/%s head term"
                        functor arity))
    (`("imported" . ,file) (format "Predicate %s/%s head term imported from %s"
                                   functor arity file))
    (`("extern" ,module . ,_) (format "External predicate %s/%s head term from module %s"
                                      functor arity module))
    ("public" (format "Public predicate %s/%s head term"
                      functor arity))
    ("dynamic" (format "Public predicate %s/%s head term"
                       functor arity))
    ("multifile" (format "Multifile predicate %s/%s head term"
                         functor arity))
    ("local" (format "Local predicate %s/%s head term"
                     functor arity))))

(defun sweeprolog--help-echo-for-goal-functor (kind functor arity)
  (pcase kind
    ("built_in" (format "Call to built-in predicate %s/%s"
                        functor arity))
    (`("imported" . ,file) (format "Call to predicate %s/%s imported from %s"
                                   functor arity file))
    (`("autoload" . ,file) (format "Call to predicate %s/%s autoloaded from %s"
                                   functor arity file))
    ("global" (format "Call to global predicate %s/%s"
                      functor arity))
    (`("global" . ,type) (format "Call to %s global predicate %s/%s"
                                 type functor arity))
    ("undefined" (format "Call to undefined predicate %s/%s"
                         functor arity))
    ("thread_local" (format "Call to thread-local predicate %s/%s"
                            functor arity))
    ("dynamic" (format "Call to dynamic predicate %s/%s"
                       functor arity))
    ("multifile" (format "Call to multifile predicate %s/%s"
                         functor arity))
    ("expanded" (format "Call to expanded predicate %s/%s"
                        functor arity))
    (`("extern" ,module . ,_) (format "Call to external predicate %s/%s from module %s"
                                      functor arity module))
    ("recursion" (format "Recursive call to predicate %s/%s"
                         functor arity))
    ("meta" (format "Call to meta predicate %s/%s"
                    functor arity))
    ("foreign" (format "Call to foreign predicate %s/%s"
                       functor arity))
    ("local" (format "Call to local predicate %s/%s"
                     functor arity))
    ("constraint" (format "Call to constraint %s/%s"
                          functor arity))
    ("not_callable" "Call to a non-callable term")))

(defun sweeprolog-analyze-fragment-help-echo (beg end arg)
  (when-let
      (help-echo
       (pcase arg
         (`("comment" . ,kind)
          (sweeprolog--help-echo-for-comment kind))
         (`("head" ,kind ,functor ,arity)
          (sweeprolog--help-echo-for-head-functor kind functor arity))
         (`("goal" ,kind ,functor ,arity)
          (sweeprolog--help-echo-for-goal-functor kind functor arity))
         ("instantiation_error" "Instantiation error")
         (`("type_error" . ,kind)
          (sweeprolog--help-echo-for-type-error kind))
         ("unused_import" "Unused import")
         ("undefined_import" "Undefined import")
         ("error" "Unknown error")
         ("html_attribute" "HTML attribute")
         ("html" "HTML")
         ("dict_tag" "Dict tag")
         ("dict_key" "Dict key")
         ("dict_sep" "Dict separator")
         ("meta" "Meta predicate argument specification")
         ("flag_name" "Flag name")
         ("no_flag_name" "Unknown flag")
         ("ext_quant" "Existential quantification")
         ("atom" "Atom")
         ("float" "Float")
         ("int" "Integer")
         ("empty_list" "Empty list")
         ("singleton" "Singleton variable")
         ("option_name" "Option name")
         ("no_option_name" "Unknown option")
         ("control" "Control construct")
         ("var" "Variable")
         ("fullstop" "Fullstop")
         ("functor" "Functor")
         ("arity" "Arity")
         ("predicate_indicator" "Predicate indicator")
         ("string" "String")
         ("codes" "Codes")
         ("chars" "Chars")
         (`("module" . ,module)
          (sweeprolog--help-echo-for-module module))
         ("neck" "Neck")
         (`("hook" . ,_) "Hook")
         ("hook" "Hook")
         ("qq_type" "Quasi-quotation type specifier")
         ("qq_sep" "Quasi-quotation separator")
         ("qq_open" "Quasi-quotation opening delimiter")
         ("qq_close" "Quasi-quotation closing delimiter")
         ("identifier" "Identifier")
         (`("file" . ,file)
          (sweeprolog--help-echo-for-dependency file))
         (`("file_no_depend" . ,file)
          (sweeprolog--help-echo-for-unused-dependency file))
         ("nofile" "Unknown file specification")
         ("function" "Arithmetic function")
         ("no_function" "Unknown arithmetic function")
         ("op_type" "Operator type")
         ("keyword" "Keyword")
         ("rational" "Rational")
         ("dict_function" "Dict function")
         ("dict_return_op" "Dict return operator")
         ("func_dot" "Dict function dot")))
    (with-silent-modifications
      (put-text-property beg end 'help-echo help-echo))))

(defun sweeprolog-analyze-fragment-fullstop (beg end arg)
  (pcase arg
    ((or "term"
         "clause"
         "grammar_rule"
         "directive"
         "method"
         `("comment" . ,_))
     (with-silent-modifications
       (remove-list-of-text-properties beg end '(sweeprolog-fullstop))))
    (`("syntax_error" ,_ ,eb ,ee)
     (with-silent-modifications
       (remove-list-of-text-properties (min beg eb) (max end ee)
                                       '(sweeprolog-fullstop))))
    ("fullstop"
     (with-silent-modifications
       (add-text-properties beg end
                            '(sweeprolog-fullstop
                              t
                              rear-nonsticky
                              (sweeprolog-fullstop)))))))

(defun sweeprolog-analyze-start-flymake (&rest _)
  (flymake-start))

(defun sweeprolog-analyze-fragment-flymake (beg end arg)
  (when-let ((type-text
              (pcase arg
                (`("head" "unreferenced" ,f ,a)
                 (cons :note
                       (format "Unreferenced definition for %s/%s"
                               f a)))
                (`("goal" "undefined" ,f ,a)
                 (cons :warning
                       (substitute-command-keys
                        (format "Undefined predicate %s/%s, use \\[sweeprolog-insert-term-dwim] to define it"
                                f a))))
                (`("goal" ("autoload" . ,file) . ,_)
                 (when sweeprolog-note-implicit-autoloads
                   (cons :note
                         (substitute-command-keys
                          (format "Implicit autoload from %s, use \\[sweeprolog-update-dependencies] to add dependency directive"
                                  file)))))
                ("instantiation_error"
                 (cons :warning "Instantiation error"))
                (`("type_error" . ,error-type)
                 (cons :warning (format "Type error (expected %s)"
                                        error-type)))
                (`("syntax_error" ,message . ,_)
                 (and (or (and sweeprolog--analyze-point
                               (<= (save-excursion
                                     (goto-char sweeprolog--analyze-point)
                                     (sweeprolog-beginning-of-top-term)
                                     (1- (point)))
                                   (1+ end) sweeprolog--analyze-point))
                          (< (save-excursion
                               (goto-char sweeprolog--analyze-point)
                               (sweeprolog-end-of-top-term)
                               (point))
                             (save-excursion
                               (goto-char sweeprolog--analyze-point)
                               (sweeprolog-beginning-of-next-top-term)
                               (point))
                             (point-max)))
                      (cons :error message)))
                ("unused_import"
                 (cons :note "Unused import"))
                ("undefined_import"
                 (cons :warning "Undefined import"))
                ("error"
                 (cons :warning "Unspecified error"))
                ("no_flag_name"
                 (cons :warning "No such flag"))
                ("singleton"
                 (cons :note "Singleton variable"))
                ("no_option_name"
                 (cons :warning "No such option"))
                (`("file_no_depend" . ,file)
                 (cons :note (format "Unused dependency on %s"
                                     file)))
                ("nofile"
                 (cons :warning "No such file"))
                ("no_function"
                 (cons :warning "No such arithmetic function"))))
             (diag (flymake-make-diagnostic (current-buffer)
                                            beg end
                                            (car type-text)
                                            (cdr type-text))))
    (push diag sweeprolog--diagnostics)))

(defun sweeprolog-analyze-end-flymake (beg end)
  (when sweeprolog--diagnostics-report-fn
    (funcall sweeprolog--diagnostics-report-fn
             sweeprolog--diagnostics
             :region (cons beg end))
    (setq sweeprolog--diagnostics-report-fn nil)))

(defun sweeprolog-analyze-fragment-variable (beg end arg)
  (pcase arg
    ((or "var"
         `("goal_term" "meta" variable 0))
     (let ((var (buffer-substring-no-properties beg end)))
       (unless (string= var "_")
         (with-silent-modifications
           (put-text-property beg end 'cursor-sensor-functions
                              (sweeprolog-cursor-sensor-functions
                               var))))))))

(defvar sweeprolog-analyze-region-start-hook
  '(sweeprolog-analyze-start-font-lock))

(defvar sweeprolog-analyze-region-fragment-hook
  '(sweeprolog-analyze-fragment-font-lock
    sweeprolog-analyze-fragment-fullstop))

(defvar sweeprolog-analyze-region-end-hook
  '(sweeprolog-analyze-end-font-lock))

(defun sweeprolog-xref-buffer ()
  (when-let ((fn (buffer-file-name)))
    (sweeprolog--query-once "sweep" "sweep_xref_source" fn)))

(defun sweeprolog-analyze-fragment (frag)
  (let* ((beg (max (point-min) (car frag)))
         (end (min (point-max) (+ beg (cadr frag))))
         (arg (cddr frag)))
    (run-hook-with-args 'sweeprolog-analyze-region-fragment-hook
                        beg end arg)))

(defun sweeprolog-analyze-region (beg end &optional one-term)
  "Analyze the current buffer contents from BEG to END.
If ONE-TERM is non-nil, region is assumed to include one Prolog
top term."
  (run-hook-with-args 'sweeprolog-analyze-region-start-hook beg end)
  (sweeprolog--query-once "sweep" "sweep_analyze_region"
                          (list one-term
                                beg
                                (buffer-substring-no-properties beg end)
                                (buffer-file-name)))
  (run-hook-with-args 'sweeprolog-analyze-region-end-hook beg end))

(defun sweeprolog-analyze-buffer (&optional force)
  "Analyze the current buffer, if it has been modified.

When FORCE is non-nil, analyze the buffer even if it has not been
modified."
  (interactive (list t))
  (when (or force sweeprolog--buffer-modified)
    (sweeprolog-xref-buffer)
    (save-restriction
      (widen)
      (let  ((sweeprolog--analyze-point (point)))
        (sweeprolog-analyze-region (point-min) (point-max))))
    (setq sweeprolog--buffer-modified nil)))

(defun sweeprolog--buffer-string (filename)
  (when-let ((buf (find-buffer-visiting filename)))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (buffer-substring-no-properties
         (point-min)
         (point-max))))))

(defun sweeprolog--buffer-last-modified-time (filename)
  (when-let ((buf (find-buffer-visiting filename)))
    (with-current-buffer buf
      sweeprolog--buffer-last-modified-time)))

(defun sweeprolog-analyze-term (beg &optional end)
  (if end
      (sweeprolog-analyze-region beg end "true")
    (save-mark-and-excursion
      (goto-char beg)
      (unless (sweeprolog-at-beginning-of-top-term-p)
        (sweeprolog-beginning-of-top-term))
      (unless (bobp)
        (forward-char -1))
      (let ((start (point)))
        (sweeprolog-end-of-top-term)
        (sweeprolog-analyze-region start (point) "true")))))

(defun sweeprolog-analyze-some-terms (beg end &optional _verbose)
  (let ((sweeprolog--analyze-point (point)))
   (save-match-data
     (save-mark-and-excursion
       (goto-char beg)
       (sweeprolog-beginning-of-top-term)
       (unless (bobp)
         (forward-char -1)
         (sweeprolog-beginning-of-top-term)
         (unless (bobp) (forward-char -1)))
       (let ((start (point))
             (cur (point)))
         (while (and (not (eobp))
                     (< (point) end))
           (setq cur (point))
           (sweeprolog-end-of-top-term)
           (sweeprolog-analyze-term cur (point)))
         (setq cur (point))
         (sweeprolog-end-of-top-term)
         (skip-chars-forward " \t\n")
         (sweeprolog-analyze-term cur (point))
         `(jit-lock-bounds ,start . ,(point)))))))

(defun sweeprolog-syntax-propertize (start end)
  (goto-char start)
  (let ((case-fold-search nil))
    (funcall
     (syntax-propertize-rules
      ((rx (group-n 1 "\\") anychar)
       (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
            (string-to-syntax "."))))
      ((rx bow (group-n 1 "0'" anychar))
       (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
            (string-to-syntax "w")))))
     start end)))

(defun sweeprolog-highlight-variable (point &optional var)
  "Highlight occurrences of the variable VAR in the clause at POINT.

If VAR is nil, clear variable highlighting in the current clause
instead.

Interactively, operate on the clause at point.  If a prefix
argument is specified, clear variable highlighting in the
current clause.  Otherwise prompt for VAR, defaulting to the
variable at point, if any."
  (interactive (list (point)
                     (unless current-prefix-arg
                       (let ((v (symbol-at-point)))
                         (read-string "Highlight variable: "
                                      nil nil
                                      (and v
                                           (save-match-data
                                             (let ((case-fold-search nil))
                                               (string-match
                                                (rx bos upper)
                                                (symbol-name v))))
                                           (symbol-name v))))))
               sweeprolog-mode sweeprolog-top-level-mode)
  (save-excursion
    (goto-char point)
    (sweeprolog-analyze-term-at-point
     (lambda (beg end arg)
       (pcase arg
         ((or "var"
              `("goal_term" "meta" variable 0))
          (let ((cur (buffer-substring-no-properties beg end)))
            (when (and var (string= cur var))
              (with-silent-modifications
               (font-lock--add-text-property beg
                                             end
                                             'font-lock-face
                                             (sweeprolog-variable-at-point-face)
                                             (current-buffer) nil))))))))))

(defun sweeprolog-cursor-sensor-functions (var)
  (list
   (lambda (_win old dir)
     (if (eq dir 'entered)
         (sweeprolog-highlight-variable (point) var)
       (sweeprolog-highlight-variable old)))))


;;;; Flymake

(defun sweeprolog-diagnostic-function (report-fn &rest rest)
  (setq sweeprolog--diagnostics nil
        sweeprolog--diagnostics-report-fn report-fn
        sweeprolog--diagnostics-changes-beg (plist-get rest :changes-start)
        sweeprolog--diagnostics-changes-end (plist-get rest :changes-end)))

(defun sweeprolog-show-diagnostics (&optional proj)
  "Show diagnostics for the current project, or buffer if PROJ is nil.

Interactively, PROJ is the prefix argument."
  (interactive "P" sweeprolog-mode)
  (if (and sweeprolog-enable-flymake
           flymake-mode)
      (if proj
          (flymake-show-project-diagnostics)
        (flymake-show-buffer-diagnostics))
    (user-error "Flymake is not active in the current buffer")))


;;;; Top-level

(defun sweeprolog-colourise-query (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((beg (cdr comint-last-prompt))
                 (end (point-max))
                 (query (buffer-substring-no-properties beg end)))
        (with-silent-modifications
          (font-lock-unfontify-region beg end))
        (sweeprolog--query-once "sweep" "sweep_colourise_query"
                                (cons query (marker-position beg)))))))

(defun sweeprolog-top-level-buffer (&optional name)
  "Return a Prolog top-level buffer named NAME.

If NAME is nil, use the default name \"*sweeprolog-top-level*\".

If the buffer already exists, ensure it is associated with a live
top-level."
  (unless sweeprolog-prolog-server-port
    (sweeprolog-start-prolog-server))
  (let ((buf (get-buffer-create (or name "*sweeprolog-top-level*"))))
    (unless (process-live-p (get-buffer-process buf))
      (with-current-buffer buf
        (unless (eq major-mode 'sweeprolog-top-level-mode)
          (sweeprolog-top-level-mode)))
      (unless (sweeprolog--query-once "sweep" "sweep_accept_top_level_client"
                                      (buffer-name buf))
        (error "Failed to create new top-level!"))
      (make-comint-in-buffer "sweeprolog-top-level"
                             buf
                             (cons "localhost"
                                   sweeprolog-prolog-server-port))
      (unless comint-last-prompt
        (accept-process-output (get-buffer-process buf) 1))
      (sweeprolog-top-level--populate-thread-id))
    buf))

;;;###autoload
(defun sweeprolog-top-level (&optional buffer-name)
  "Run a Prolog top-level in a buffer.

BUFFER-NAME is passed to `sweeprolog-top-level-buffer' to obtain
an appropriate buffer.

Interactively, a prefix argument means to prompt for BUFFER-NAME."
  (interactive
   (list (and current-prefix-arg
              (read-buffer "Top-level buffer: "
                           (if (and (eq major-mode 'sweeprolog-top-level-mode)
                                    (null (get-buffer-process
                                           (current-buffer))))
                               (buffer-name)
                             (generate-new-buffer-name "*sweeprolog-top-level*"))))))
  (pop-to-buffer (sweeprolog-top-level-buffer buffer-name)
                 sweeprolog-top-level-display-action))

(defun sweeprolog-top-level-send-string (string &optional buffer)
  "Send STRING to the top-level associated with BUFFER.

If BUFFER is nil, use `sweeprolog-top-level-buffer' to obtain an
appropriate buffer."
  (comint-send-string (get-buffer-process
                       (or buffer (sweeprolog-top-level-buffer)))
                      string))

;;;###autoload
(defun sweeprolog-top-level-send-goal (goal)
  "Send GOAL to a top-level buffer and display that buffer."
  (interactive (list (sweeprolog-read-goal)))
  (let ((goal (cond
               ((string-match (rx "." (or white "\n") eos) goal)
                goal)
               ((string-match (rx "." eos) goal)
                (concat goal "\n"))
               (t (concat goal ".\n")))))
    (let ((buffer (sweeprolog-top-level-buffer)))
      (sweeprolog-top-level-send-string goal buffer)
      (display-buffer buffer sweeprolog-top-level-display-action))))

(defun sweeprolog-top-level--post-self-insert-function ()
  (when-let ((pend (cdr comint-last-prompt)))
    (let* ((pstart (car comint-last-prompt))
           (prompt (buffer-substring-no-properties pstart pend)))
      (when (and (= (point) (1+ pend))
                 (not (string-empty-p  prompt))
                 (not (string= "?- "   (substring prompt
                                                  (- pend pstart 3)
                                                  (- pend pstart))))
                 (not (string= "|: "   prompt))
                 (not (string= "|    " prompt)))
        (comint-send-input)))))

(defun sweeprolog-top-level--populate-thread-id ()
  (setq sweeprolog-top-level-thread-id
        (sweeprolog--query-once "sweep" "sweep_top_level_thread_buffer"
                                (buffer-name) t)))

(defun sweeprolog-signal-thread (tid goal)
  (sweeprolog--query-once "sweep" "sweep_thread_signal"
                          (cons tid goal)))

(defun sweeprolog-top-level-signal (buffer goal)
  "Signal the top-level thread corresponding to BUFFER to run GOAL."
  (interactive
   (list (read-buffer "Top-level buffer: "
                      nil t
                      (lambda (b)
                        (let ((bb (or (and (consp b) (car b)) b)))
                          (with-current-buffer bb
                            (and (derived-mode-p 'sweeprolog-top-level-mode)
                                 sweeprolog-top-level-thread-id)))))
         (read-string "Signal goal: ?- ")))
  (with-current-buffer (get-buffer buffer)
    (sweeprolog-top-level-signal-current goal)))

(defun sweeprolog-top-level-signal-current (goal)
  "Signal the current top-level thread to run GOAL.

Interactively, when called with a prefix argument, prompt for
GOAL.  Otherwise, GOAL is set to a default value specified by
`sweeprolog-top-level-signal-default-goal'."
  (interactive (list (if current-prefix-arg
                         (read-string "Signal goal: ?- " nil
                                      'sweeprolog-top-level-signal-goal-history)
                       sweeprolog-top-level-signal-default-goal)))
  (unless sweeprolog-top-level-thread-id
    (sweeprolog-top-level--populate-thread-id))
  (sweeprolog-signal-thread sweeprolog-top-level-thread-id goal))

;;;###autoload
(define-derived-mode sweeprolog-top-level-mode comint-mode "Sweep Top-level"
  "Major mode for interacting with an inferior Prolog interpreter."
  :group 'sweeprolog-top-level
  (setq-local comint-prompt-regexp           (rx line-start "?- ")
              comint-input-ignoredups        t
              comint-prompt-read-only        t
              comint-input-filter            (lambda (s)
                                               (< sweeprolog-top-level-min-history-length
                                                  (length s)))
              comint-delimiter-argument-list '(?,)
              comment-start "%")
  (add-hook 'post-self-insert-hook #'sweeprolog-top-level--post-self-insert-function nil t)
  (dolist (capf sweeprolog-completion-at-point-functions)
    (add-hook 'completion-at-point-functions capf nil t))
  (setq sweeprolog-top-level-timer (run-with-idle-timer 0.2 t #'sweeprolog-colourise-query (current-buffer)))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (condition-case _
                  (sweeprolog-top-level-signal (current-buffer)
                                               "thread_exit(0)")
                (prolog-exception nil)))
            nil t)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (timerp sweeprolog-top-level-timer)
                (cancel-timer sweeprolog-top-level-timer)))
            nil t))

(defun sweeprolog-buffer-load-time (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (sweeprolog--query-once "sweep" "sweep_source_file_load_time"
                          (with-current-buffer buffer
                           (or (buffer-file-name)
                               (expand-file-name (buffer-name))))))

(defun sweeprolog-buffer-loaded-since-last-modification-p ()
  (when-let ((mtime (or sweeprolog--buffer-last-modified-time
                        (and (buffer-file-name)
                             (float-time
                              (file-attribute-modification-time
                               (file-attributes (buffer-file-name)))))))
             (ltime (sweeprolog-buffer-load-time)))
    (<= mtime ltime)))

(defun sweeprolog-load-buffer (buffer)
  "Load the Prolog buffer BUFFER into the embedded SWI-Prolog runtime.

Interactively, if the major mode of the current buffer is
`sweeprolog-mode' and the command is called without a prefix argument,
load the current buffer.  Otherwise, prompt for a `sweeprolog-mode'
buffer to load."
  (interactive (list
                (if (and (not current-prefix-arg)
                         (eq major-mode 'sweeprolog-mode))
                    (current-buffer)
                  (read-buffer "Load buffer: "
                               (when (eq major-mode 'sweeprolog-mode)
                                 (buffer-name))
                               t
                               (lambda (b)
                                 (let ((n (or (and (consp b) (car b)) b)))
                                   (with-current-buffer n
                                     (eq major-mode 'sweeprolog-mode))))))))
  (with-current-buffer buffer
    (if (sweeprolog-buffer-loaded-since-last-modification-p)
        (message "Buffer %s already loaded." (buffer-name))
      (let* ((beg (point-min))
             (end (point-max))
             (contents (buffer-substring-no-properties beg end)))
        (if (sweeprolog--query-once "sweep" "sweep_load_buffer"
                                    (list contents
                                          (or sweeprolog--buffer-last-modified-time
                                              (float-time))
                                          (or (buffer-file-name)
                                              (expand-file-name (buffer-name)))))
            (progn
              (message "Loaded %s." (buffer-name))
              (force-mode-line-update))
          (user-error "Loading %s failed!" (buffer-name)))))))


;;;; Prolog file specifications

;;;###autoload
(defun sweeprolog-file-name-handler (operation &rest args)
  (cond ((eq operation 'expand-file-name)
         (let ((fn (car  args))
               (dn (cadr args)))
           (or (sweeprolog--query-once "sweep" "sweep_expand_file_name"
                                       (cons fn dn))
               (let ((inhibit-file-name-handlers
                      (cons 'sweeprolog-file-name-handler
                            (and (eq inhibit-file-name-operation operation)
                                 inhibit-file-name-handlers)))
                     (inhibit-file-name-operation operation))
                 (apply operation args)))))
        (t (let ((inhibit-file-name-handlers
                  (cons 'sweeprolog-file-name-handler
                        (and (eq inhibit-file-name-operation operation)
                             inhibit-file-name-handlers)))
                 (inhibit-file-name-operation operation))
             (apply operation args)))))

(add-to-list 'file-name-handler-alist
             (cons (rx bol (one-or-more lower) "(")
                   #'sweeprolog-file-name-handler))

(defun sweeprolog-file-at-point (&optional point)
  "Return the file name specified by the Prolog file spec at POINT."
  (setq point (or point (point)))
  (let ((fap nil))
    (sweeprolog-analyze-term-at-point
     (lambda (beg end arg)
       (when (<= beg point end)
         (pcase arg
           ((or `("file"           . ,file)
                `("file_no_depend" . ,file))
            (setq fap file))))))
    fap))

(defun sweeprolog-find-file-at-point (point &optional other-window)
  "Find file specified by the Prolog file spec at POINT.

If OTHER-WINDOW is non-nil, find it in another window.

Interactively, POINT is set to the current point and OTHER-WINDOW
is the prefix argument."
  (interactive "d\nP" sweeprolog-mode)
  (if-let ((file (sweeprolog-file-at-point point)))
      (if other-window
          (find-file-other-window file)
        (find-file file))
    (user-error "No file specification found at point!")))


;;;; Moving and editing

(defun sweeprolog-beginning-of-top-term (&optional arg)
  (let ((times (or arg 1)))
    (if (< 0 times)
        (let ((p (point)))
          (while (and (< 0 times) (not (bobp)))
            (setq times (1- times))
            (when-let ((safe-start (nth 8 (syntax-ppss))))
              (goto-char safe-start))
            (unless (and (sweeprolog-at-beginning-of-top-term-p)
                         (not (= p (point))))
              (re-search-backward (rx bol graph) nil t)
              (let ((safe-start (or (nth 8 (syntax-ppss))
                                    (nth 8 (syntax-ppss (1+ (point)))))))
                (while (and safe-start (not (bobp)))
                  (goto-char safe-start)
                  (if (or (bobp)
                          (sweeprolog-at-beginning-of-top-term-p))
                      (setq safe-start nil)
                    (backward-char)
                    (re-search-backward (rx bol graph) nil t)
                    (setq safe-start (or (nth 8 (syntax-ppss))
                                         (nth 8 (syntax-ppss (1+ (point)))))))))))
          (not (= p (point))))
      (sweeprolog-beginning-of-next-top-term (- times)))))

(defun sweeprolog-beginning-of-next-top-term (&optional times)
  (setq times (or times 1))
  (let ((target (save-mark-and-excursion
                  (while (< 0 times)
                    (setq times (1- times))
                    (when (sweeprolog-at-beginning-of-top-term-p)
                      (forward-char))
                    (while (and (re-search-forward (rx bol graph) nil t)
                                (or (nth 8 (syntax-ppss))
                                    (and (= (char-before) ?/)
                                         (= (char-after) ?*))
                                    (eobp)))))
                  (beginning-of-line)
                  (point))))
    (unless (= target (point))
      (goto-char target))))

(defun sweeprolog-at-fullstop-p (&optional point)
  (setq point (or point (point)))
  (get-text-property point 'sweeprolog-fullstop))

(defun sweeprolog-end-of-last-fullstop (&optional point)
  (setq point (or point (point)))
  (if (sweeprolog-at-fullstop-p point)
      (when-let ((beg (previous-single-property-change
                       point
                       'sweeprolog-fullstop)))
        (previous-single-property-change beg 'sweeprolog-fullstop))
    (previous-single-property-change point 'sweeprolog-fullstop)))

(defun sweeprolog-end-of-next-fullstop (&optional point)
  (setq point (or point (point)))
  (when-let
      ((end
        (if (sweeprolog-at-fullstop-p point)
            (next-single-property-change point 'sweeprolog-fullstop)
          (when-let ((beg (next-single-property-change
                           point
                           'sweeprolog-fullstop)))
            (next-single-property-change beg 'sweeprolog-fullstop)))))
    (min (1+ end) (point-max))))

(defun sweeprolog-end-of-top-term ()
  (save-restriction
    (narrow-to-region (or (sweeprolog-end-of-last-fullstop)
                          (point-min))
                      (or (sweeprolog-end-of-next-fullstop)
                          (point-max)))
    (unless (eobp)
      (while (and (nth 8 (syntax-ppss)) (not (eobp)))
        (forward-char))
      (or (re-search-forward (rx "." (or white "\n")) nil t)
          (goto-char (point-max)))
      (while (and (or (nth 8 (syntax-ppss))
                      (save-excursion
                        (nth 8 (syntax-ppss (max (point-min)
                                                 (1- (point))))))
                      (save-match-data
                        (looking-back (rx (or "#" "$" "&" "*" "+" "-"
                                              "." "/" ":" "<" "=" ">"
                                              "?" "@" "\\" "^" "~")
                                          "." (or white "\n"))
                                      (line-beginning-position))))
                  (not (eobp)))
        (while (and (nth 8 (syntax-ppss)) (not (eobp)))
          (forward-char))
        (or (re-search-forward (rx "." (or white "\n")) nil t)
            (goto-char (point-max)))))))

(defun sweeprolog-at-hole-p (&optional point)
  (setq point (or point (point)))
  (get-text-property point 'sweeprolog-hole))

(defun sweeprolog-beginning-of-hole (&optional point)
  (let ((beg (or point (point))))
    (when (sweeprolog-at-hole-p beg)
      (while (and (< (point-min) beg)
                  (sweeprolog-at-hole-p (1- beg)))
        (setq beg (1- beg)))
      beg)))

(defun sweeprolog-end-of-hole (&optional point)
  (let ((end (or point (point))))
    (when (sweeprolog-at-hole-p end)
      (while (and (< end (point-max))
                  (sweeprolog-at-hole-p (1+ end)))
        (setq end (1+ end)))
      (1+ end))))


(defun sweeprolog-count-holes (&optional interactive)
  "Count holes in the current buffer and return the total number.
If INTERACTIVE is non-nil, as it is when called interactively,
also print a message with the result."
  (interactive (list t) sweeprolog-mode)
  (save-excursion
    (goto-char (point-min))
    (let ((hole nil) (count 0))
      (while (setq hole (sweeprolog--next-hole))
        (goto-char (cdr hole))
        (setq count (1+ count)))
      (when interactive
        (message "%s %s left in buffer %s."
                 (if (zerop count) "No" count)
                 (ngettext "hole" "holes" count)
                 (buffer-name)))
      count)))

(defun sweeprolog--next-hole (&optional wrap)
  "Return the bounds of the next hole in the current buffer.

When WRAP in non-nil, wrap around if no holes are found between
point and the end of the buffer."
  (if-let ((current-hole-beg (sweeprolog-beginning-of-hole)))
      (cons current-hole-beg
            (sweeprolog-end-of-hole))
    (let ((point (point)))
      (while (not (or (sweeprolog-at-hole-p) (eobp)))
        (forward-char))
      (if (eobp)
          (when wrap
            (save-restriction
              (goto-char (point-min))
              (narrow-to-region (point) point)
              (sweeprolog--next-hole)))
        (cons (point)
              (sweeprolog-end-of-hole))))))

(defun sweeprolog--backward-wrap (&optional wrap)
  (if (bobp)
      (when wrap
        (goto-char (point-max))
        t)
    (forward-char -1)
    t))

(defun sweeprolog--previous-hole (&optional wrap)
  "Return the bounds of the previous hole in the current buffer.

When WRAP in non-nil, wrap around if no holes are found between
point and the beginning of the buffer."
  (when (and (< (point-min) (point))
             (sweeprolog-at-hole-p (1- (point))))
    (forward-char -1)
    (while (and (sweeprolog-at-hole-p) (not (bobp)))
      (forward-char -1)))
  (let ((start (point)))
    (when (sweeprolog--backward-wrap wrap)
      (if-let ((current-hole-beg (sweeprolog-beginning-of-hole)))
          (cons current-hole-beg
                (sweeprolog-end-of-hole))
        (let ((point (point)))
          (while (not (or (sweeprolog-at-hole-p) (bobp)))
            (forward-char -1))
          (if (and (bobp) (not (sweeprolog-at-hole-p)))
              (or (and wrap
                       (save-restriction
                         (goto-char (point-max))
                         (narrow-to-region point (point))
                         (sweeprolog--previous-hole)))
                  (and (goto-char start) nil))
            (cons (sweeprolog-beginning-of-hole)
                  (sweeprolog-end-of-hole))))))))

(defun sweeprolog-backward-hole (&optional n)
  "Move point to the previous Nth hole in the current buffer.
If N is 0, display the number of holes in the current buffer by
calling `sweeprolog-count-holes' instead.

See also `sweeprolog-forward-hole'."
  (interactive "p" sweeprolog-mode)
  (sweeprolog-forward-hole (- (or n 1))))

(defun sweeprolog-forward-hole (&optional n)
  "Move point to the next Nth hole in the current buffer.
If N is 0, display the number of holes in the current buffer by
calling `sweeprolog-count-holes' instead.

See also `sweeprolog-backward-hole'."
  (interactive "p" sweeprolog-mode)
  (setq n (or n 1))
  (if (zerop n)
      (sweeprolog-count-holes t)
    (let* ((func (if (< 0 n)
                     #'sweeprolog--next-hole
                   #'sweeprolog--previous-hole))
           (hole (funcall func t)))
      (unless hole
        (user-error "No holes in buffer %s" (buffer-name)))
      (goto-char (cdr hole))
      (dotimes (_ (1- (abs n)))
        (setq hole (funcall func t))
        (goto-char (cdr hole)))
      (setq deactivate-mark nil)
      (push-mark (car hole) t t))))

(put 'sweeprolog-backward-hole
     'repeat-map
     'sweeprolog-forward-hole-repeat-map)

(put 'sweeprolog-forward-hole
     'repeat-map
     'sweeprolog-forward-hole-repeat-map)

(put 'sweeprolog-insert-term-with-holes
     'repeat-map
     'sweeprolog-forward-hole-repeat-map)

(put 'sweeprolog-insert-term-dwim 'undo-inhibit-region t)

(defun sweeprolog--hole (&optional string)
  (propertize (or string "_")
              'sweeprolog-hole t
              'rear-nonsticky '(sweeprolog-hole
                                cursor-sensor-functions
                                font-lock-face)))

(defun sweeprolog--precedence-at-point (&optional point)
  (setq point (or point (point)))
  (pcase (sweeprolog-last-token-boundaries point)
    ((or `(operator ,obeg ,oend)
         `(symbol   ,obeg ,oend))
     (let ((op (buffer-substring-no-properties obeg oend)))
       (or (and (string= "." op)
                (or (not (char-after (1+ obeg)))
                    (member (char-syntax (char-after (1+ obeg)))
                            '(?> ? )))
                1200)
           (sweeprolog-op-infix-precedence op)
           (sweeprolog-op-prefix-precedence op)
           1200)))
    (_ 1200)))

(defun sweeprolog-insert-term-with-holes (functor arity)
  "Insert a term with functor FUNCTOR and arity ARITY.

If ARITY is negative, just insert a single hole at point.
Otherwise, insert FUNCTOR along with ARITY holes, one for each of
the term's arguments.

Interactively, prompt for FUNCTOR.  Without a prefix argument,
prompt for ARITY as well.  Otherwise, ARITY is the numeric value
of the prefix argument."
  (interactive
   (let* ((arity (and current-prefix-arg
                      (prefix-numeric-value current-prefix-arg))))
     (if (and (numberp arity)
              (< arity 0))
         (list "_" arity)
       (let ((functor-arity (sweeprolog-read-functor arity)))
         (list (car functor-arity) (cdr functor-arity))))))
  (combine-after-change-calls
    (when (use-region-p)
      (delete-region (region-beginning)
                     (region-end)))
    (let* ((beg (point)))
      (when
          (pcase (sweeprolog-last-token-boundaries beg)
            (`(symbol ,obeg ,oend)
             (let ((op (buffer-substring-no-properties obeg oend)))
               (not (or (sweeprolog-op-infix-precedence op)
                        (sweeprolog-op-prefix-precedence op)))))
            (`(close . ,_) t))
        (insert ", "))
      (if (<= 0 arity)
          (let ((term-format
                 (sweeprolog--query-once
                  "sweep" "sweep_format_term"
                  (list functor arity
                        (sweeprolog--precedence-at-point beg)))))
            (insert (car term-format))
            (pcase  (cdr term-format)
              ((or `(compound
                     "term_position"
                     0 ,length
                     ,_ ,_
                     ,holes)
                   `(compound
                     "parentheses_term_position"
                     0 ,length
                     (compound
                      "term_position"
                      ,_ ,_ ,_ ,_
                      ,holes)))
               (with-silent-modifications
                 (dolist (hole holes)
                   (pcase hole
                     (`(compound "-" ,hbeg ,hend)
                      (add-text-properties
                       (- (point) length (- hbeg))
                       (- (point) length (- hend))
                       (list
                        'sweeprolog-hole t
                        'font-lock-face (list (sweeprolog-hole-face))
                        'rear-nonsticky '(sweeprolog-hole
                                          cursor-sensor-functions
                                          font-lock-face))))))))))
        (insert (sweeprolog--hole functor)))
      (pcase (sweeprolog-next-token-boundaries)
        ('nil (insert "."))
        (`(,kind ,obeg ,oend)
         (if (save-excursion
               (goto-char obeg)
               (sweeprolog-at-beginning-of-top-term-p))
             (insert ".")
           (when (or (and (member kind '(symbol operator))
                          (let ((op (buffer-substring-no-properties obeg oend)))
                            (not (or (sweeprolog-op-infix-precedence op)
                                     (sweeprolog-op-suffix-precedence op)))))
                     (member kind '(open functor)))
             (insert ", ")))))
      (unless (= 0 arity)
        (goto-char beg))
      (deactivate-mark)
      (sweeprolog-forward-hole))))

(defun sweeprolog-insert-clause (functor arity &optional neck module)
  (let ((point (point))
        (neck (or neck ":-"))
        (head-format (sweeprolog--query-once "sweep" "sweep_format_head"
                                             (cons functor arity))))
    (combine-after-change-calls
      (insert "\n"
              (if module
                  (concat module ":")
                "")
              (car head-format))
      (pcase (cdr head-format)
        (`(compound
           "term_position"
           0 ,length
           ,_fbeg ,_fend
           ,holes)
         (with-silent-modifications
           (dolist (hole holes)
             (pcase hole
               (`(compound "-" ,hbeg ,hend)
                (add-text-properties
                 (- (point) length (- hbeg))
                 (- (point) length (- hend))
                 (list
                  'sweeprolog-hole t
                  'font-lock-face (list (sweeprolog-hole-face))
                  'rear-nonsticky '(sweeprolog-hole
                                    cursor-sensor-functions
                                    font-lock-face)))))))))
      (insert " " neck " " (sweeprolog--hole "Body") ".\n"))
    (goto-char point)
    (sweeprolog-forward-hole)))

(defun sweeprolog-maybe-insert-next-clause (point kind beg end)
  (when-let ((current-predicate (and (eq kind 'operator)
                                     (string= "." (buffer-substring-no-properties beg end))
                                     (sweeprolog-definition-at-point point))))
    (let ((functor (nth 1 current-predicate))
          (arity   (nth 2 current-predicate))
          (neck    (nth 4 current-predicate))
          (module  (nth 5 current-predicate)))
      (goto-char end)
      (end-of-line)
      (sweeprolog-insert-clause functor
                                (- arity (if (string= neck "-->") 2 0))
                                neck
                                module)
      t)))

(defun sweeprolog-default-new-predicate-location (&rest _)
  (sweeprolog-end-of-predicate-at-point))

(defun sweeprolog-new-predicate-location-above-current (&rest _)
  (sweeprolog-beginning-of-predicate-at-point)
  (let ((last (or (caddr (sweeprolog-last-token-boundaries))
                  (point-min))))
    (while (re-search-backward (rx bol "%" (or "%" "!")) last t))))

(defun sweeprolog-maybe-define-predicate (point _kind _beg _end)
  (let ((functor nil)
        (arity nil)
        (neck ":-"))
    (sweeprolog-analyze-term-at-point
     (lambda (beg end arg)
       (pcase arg
         (`("goal_term" "undefined" ,f ,a)
          (when (<= beg point end)
            (setq functor f
                  arity   a)))
         ("neck"
          (setq neck
                (buffer-substring-no-properties beg end)))
         (`("goal_term" "built_in" "phrase" ,a)
          (when (and (<= beg point end)
                     (member a '(2 3))
                     (string= neck ":-"))
            (setq neck "-->")))
         (`("dcg" . "plain")
          (when (and (<= beg point end)
                     (string= neck "-->"))
            (setq neck ":-"))))))
    (when functor
      (funcall sweeprolog-new-predicate-location-function
               functor arity neck)
      (sweeprolog-insert-clause functor
                                (- arity (if (string= neck "-->") 2 0))
                                neck)
      t)))

(defun sweeprolog-insert-term-dwim (&optional point)
  "Insert an appropriate Prolog term at POINT.

This command calls the functions in
`sweeprolog-insert-term-functions' one after the other until one
of them signal success by returning non-nil."
  (interactive "d" sweeprolog-mode)
  (setq point (or point (point)))
  (let* ((bounds (sweeprolog-last-token-boundaries))
         (kind (car bounds))
         (beg  (cadr bounds))
         (end  (caddr bounds)))
    (unless (run-hook-with-args-until-success
             'sweeprolog-insert-term-functions
             point kind beg end)
      (user-error "No term insertion function applies here"))))

(defun sweeprolog-at-beginning-of-top-term-p ()
  (and (looking-at-p (rx bol graph))
       (not (nth 8 (syntax-ppss)))
       (not (looking-at-p (rx bol (or "%" "/*"))))))

(defun sweeprolog-analyze-buffer-with (cb)
  (add-hook 'sweeprolog-analyze-region-fragment-hook cb nil t)
  (sweeprolog-analyze-buffer t)
  (remove-hook 'sweeprolog-analyze-region-fragment-hook cb t))

(defun sweeprolog-analyze-term-at-point (cb &optional point)
  (setq point (or point (point)))
  (let ((sweeprolog--analyze-point point))
    (add-hook 'sweeprolog-analyze-region-fragment-hook cb nil t)
    (sweeprolog-analyze-term point)
    (remove-hook 'sweeprolog-analyze-region-fragment-hook cb t)))

(defun sweeprolog-definition-at-point (&optional point)
  (save-excursion
    (when point (goto-char point))
    (let ((functor nil)
          (arity nil)
          (neck ":-")
          (module nil)
          (start nil)
          (stop nil))
      (sweeprolog-analyze-term-at-point (lambda (beg end arg)
                                          (pcase arg
                                            ("range"
                                             (setq start beg))
                                            (`("head" "meta" ":" 2)
                                             (setq module t))
                                            ("expanded"
                                             (setq module "prolog"))
                                            (`("hook" . "message")
                                             (when (string= module "prolog")
                                               (setq functor (buffer-substring-no-properties beg end)
                                                     arity 3)))
                                            (`("module" . ,mod)
                                             (when (eq module t)
                                               (setq module mod)))
                                            (`("head" ,_ ,f ,a)
                                             (setq functor f
                                                   arity a))
                                            ("neck"
                                             (setq neck
                                                   (buffer-substring-no-properties beg end)))
                                            ("fullstop"
                                             (setq stop beg)))))
      (when functor
        (list start functor arity stop neck module)))))

(defun sweeprolog-end-of-predicate-at-point ()
  "Move to the end of the predicate definition at point."
  (when-let* ((def (sweeprolog-definition-at-point)))
    (let ((point (point))
          (fun (cadr def))
          (ari (caddr def)))
      (while (and point (not (eobp)))
        (sweeprolog-end-of-top-term)
        (if-let* ((ndef (sweeprolog-definition-at-point (point)))
                  (nfun (cadr ndef))
                  (nari (caddr ndef))
                  (same (and (string= fun nfun)
                             (=       ari nari))))
            (setq point (point))
          (goto-char point)
          (setq point nil))))))

(defun sweeprolog-forward-predicate (&optional arg)
  "Move forward over the ARGth next predicate definition from point."
  (interactive "p" sweeprolog-mode)
  (setq arg (or arg 1))
  (while (< 0 arg)
    (setq arg (1- arg))
    (if-let ((line
              (sweeprolog--query-once "sweep" "sweep_beginning_of_next_predicate"
                                      (line-number-at-pos))))
        (progn
          (goto-char (point-min))
          (forward-line (1- line)))
      (setq arg 0)
      (user-error "No next predicate"))))

(defun sweeprolog-backward-predicate (&optional arg)
  "Move backward over the ARGth next predicate definition from point."
  (interactive "p" sweeprolog-mode)
  (setq arg (or arg 1))
  (while (< 0 arg)
    (setq arg (1- arg))
    (if-let ((line
              (sweeprolog--query-once "sweep" "sweep_beginning_of_last_predicate"
                                      (line-number-at-pos))))
        (progn
          (goto-char (point-min))
          (forward-line (1- line)))
      (setq arg 0)
      (user-error "No previous predicate"))))

(defun sweeprolog-end-of-next-predicate ()
  (let ((def-at-point (sweeprolog-definition-at-point)))
    (when (or (and def-at-point (<= (point) (nth 3 def-at-point)))
              (condition-case _
                  (progn (sweeprolog-forward-predicate)
                         t)))
      (sweeprolog-end-of-predicate-at-point)
      (point))))

(defun sweeprolog-mark-predicate (&optional allow-extend)
  "Put point at beginning of this predicate, mark at end.

Interactively (or if ALLOW-EXTEND is non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next predicate after the ones already marked."
  (interactive "p" sweeprolog-mode)
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (and transient-mark-mode mark-active)))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (sweeprolog-end-of-next-predicate)))
    (when (sweeprolog-end-of-next-predicate)
      (push-mark nil t t)
      (sweeprolog-backward-predicate)
      (let ((last (or (caddr (sweeprolog-last-token-boundaries))
                      (point-min))))
        (while (re-search-backward (rx bol "%" (or "%" "!")) last t))))))

(defun sweeprolog-beginning-of-predicate-at-point (&optional point)
  "Find the beginning of the predicate definition at or above POINT.

Return a cons cell (FUN . ARI) where FUN is the functor name of
the defined predicate and ARI is its arity, or nil if there is no
predicate definition at or directly above POINT."
  (when-let* ((def (sweeprolog-definition-at-point point)))
    (unless (sweeprolog-at-beginning-of-top-term-p)
      (sweeprolog-beginning-of-top-term)
      (backward-char 1))
    (let ((point (point))
          (fun (cadr def))
          (ari (caddr def)))
      (while (and point (not (bobp)))
        (sweeprolog-beginning-of-top-term)
        (backward-char 1)
        (if-let* ((moved (< (point) point))
                  (ndef (sweeprolog-definition-at-point (point)))
                  (nfun (cadr ndef))
                  (nari (caddr ndef))
                  (same (and (string= fun nfun)
                             (=       ari nari))))
            (setq point (point))
          (goto-char point)
          (setq point nil)))
      (cons fun ari))))

(defun sweeprolog-format-term-with-holes (functor arity &optional pre)
  (let ((term-format
         (sweeprolog--query-once
          "sweep" "sweep_format_term"
          (list functor arity (or pre 1200)))))
    (let ((term (car term-format)))
      (pcase  (cdr term-format)
        ((or `(compound
               "term_position"
               ,_ ,_ ,_ ,_
               ,holes)
             `(compound
               "parentheses_term_position"
               ,_ ,_
               (compound
                "term_position"
                ,_ ,_ ,_ ,_
                ,holes)))
         (dolist (hole holes)
           (pcase hole
             (`(compound "-" ,hbeg ,hend)
              (add-text-properties
               hbeg hend
               (list
                'sweeprolog-hole t
                'font-lock-face (list (sweeprolog-hole-face))
                'rear-nonsticky '(sweeprolog-hole
                                  cursor-sensor-functions
                                  font-lock-face))
               term))))))
      term)))

(defun sweeprolog-read-predicate-documentation-with-holes
    (functor arity)
  "Use holes for initial documentation for predicate FUNCTOR/ARITY."
  (list (sweeprolog-format-term-with-holes functor arity)
        (sweeprolog--hole "Det")
        nil))

(defun sweeprolog-read-predicate-documentation-default-function
    (functor arity)
  "Prompt for initial documentation for predicate FUNCTOR/ARITY."
  (let ((cur 1)
        (arguments nil))
    (while (<= cur arity)
      (let ((num (pcase cur
                   (1 "First")
                   (2 "Second")
                   (3 "Third")
                   (_ (concat (number-to-string cur) "th")))))
        (push (read-string (concat num " argument: ")) arguments))
      (setq cur (1+ cur)))
    (setq arguments (reverse arguments))
    (let ((det (cadr (read-multiple-choice
                      "Determinism: "
                      '((?d "det"       "Succeeds exactly once")
                        (?s "semidet"   "Succeeds at most once")
                        (?f "failure"   "Always fails")
                        (?n "nondet"    "Succeeds any number of times")
                        (?m "multi"     "Succeeds at least once")
                        (?u "undefined" "Undefined")))))
          (summary (read-string "Summary: ")))
      (list (concat (sweeprolog-format-string-as-atom functor)
                    (if arguments
                        (concat "("
                                (mapconcat #'identity arguments ", ")
                                ")")
                      ""))
            det
            (and (not (string-empty-p summary))
                 summary)))))

(defun sweeprolog-insert-predicate-documentation (head det sum)
  (combine-after-change-calls
    (insert "%!  " head " is " det ".\n"
            (if sum (concat "%\n%   " sum "\n") "")
            "\n")
    (forward-char -2)
    (fill-paragraph t)))

(defun sweeprolog-read-predicate-documentation (fun ari)
  "Return information for initial predicate documentation of FUN/ARI.

Calls the function specified by
`sweeprolog-read-predicate-documentation-function' to do the
work."
  (funcall sweeprolog-read-predicate-documentation-function fun ari))

(defun sweeprolog-document-predicate-at-point (point)
  "Insert documentation comment for the predicate at or above POINT."
  (interactive "d" sweeprolog-mode)
  (when-let* ((pred (sweeprolog-beginning-of-predicate-at-point point))
              (fun (car pred))
              (ari (cdr pred))
              (doc (sweeprolog-read-predicate-documentation fun ari)))
    (sweeprolog-insert-predicate-documentation (car doc)
                                               (cadr doc)
                                               (caddr doc))))

(defun sweeprolog-token-boundaries (&optional pos)
  (let ((point (or pos (point))))
    (save-excursion
      (goto-char point)
      (unless (eobp)
       (let ((beg (point))
             (syn (char-syntax (char-after))))
         (cond
          ((or (= syn ?w) (= syn ?_))
           (skip-syntax-forward "w_")
           (if (= (char-syntax (char-after)) ?\()
               (progn
                 (forward-char)
                 (list 'functor beg (point)))
             (list 'symbol beg (point))))
          ((= syn ?\")
           (forward-char)
           (while (and (not (eobp)) (nth 3 (syntax-ppss)))
             (forward-char))
           (list 'string beg (point)))
          ((= syn ?.)
           (skip-syntax-forward ".")
           (list 'operator beg (point)))
          ((= syn ?\()
           (list 'open beg (point)))
          ((= syn ?\))
           (list 'close beg (point)))
          ((= syn ?>) nil)
          (t (list 'else beg (point)))))))))

(defun sweeprolog-next-token-boundaries (&optional pos)
  (let ((point (or pos (point))))
    (save-excursion
      (goto-char point)
      (while (forward-comment 1))
      (unless (eobp)
        (let ((beg (point))
              (syn (char-syntax (char-after))))
          (cond
           ((or (= syn ?w) (= syn ?_))
            (skip-syntax-forward "w_")
            (if (= (char-syntax (char-after)) ?\()
                (progn
                  (forward-char)
                  (list 'functor beg (point)))
              (list 'symbol beg (point))))
           ((= syn ?\")
            (forward-char)
            (while (and (not (eobp)) (nth 3 (syntax-ppss)))
              (forward-char))
            (list 'string beg (point)))
           ((or (= syn ?.)
                (= syn ?\\))
            (skip-syntax-forward ".")
            (list 'operator beg (point)))
           ((= syn ?\()
            (list 'open beg (point)))
           ((= syn ?\))
            (list 'close beg (point)))
           ((= syn ?>) nil)
           (t (list 'else beg (point)))))))))

(defun sweeprolog-last-token-boundaries (&optional pos)
  (let ((point (or pos (point)))
        (go t))
    (save-excursion
      (goto-char point)
      (while (and (not (bobp)) go)
        (skip-chars-backward " \t\n")
        (unless (bobp)
          (unless (nth 4 (syntax-ppss))
            (forward-char -1))
          (if (nth 4 (syntax-ppss))
              (goto-char (nth 8 (syntax-ppss)))
            (setq go nil))))
      (unless (bobp)
        (let ((end (1+ (point)))
              (syn (char-syntax (char-after))))
          (cond
           ((or (= syn ?w) (= syn ?_))
            (skip-syntax-backward "w_")
            (list 'symbol (point) end))
           ((= syn ?\")
            (list 'string (nth 8 (syntax-ppss)) end))
           ((and (= syn ?\()
                 (or (= (char-syntax (char-before)) ?w)
                     (= (char-syntax (char-before)) ?_)))
            (skip-syntax-backward "w_")
            (list 'functor (point) end))
           ((or (= syn ?.)
                (= syn ?\\))  ; specifically, the backslash character
            (skip-syntax-backward ".")
            (list 'operator (point) end))
           ((= syn ?\()
            (list 'open (1- end) end))
           ((= syn ?\))
            (list 'close (1- end) end))
           (t (list 'else (1- end) end))))))))

(defun sweeprolog--forward-term (pre)
  (pcase (sweeprolog-next-token-boundaries)
    ('nil
     (signal 'scan-error
             (list "Cannot scan beyond end of buffer."
                   (point-max)
                   (point-max))))
    (`(close ,lbeg ,lend)
     (signal 'scan-error
             (list "Cannot scan beyond closing parenthesis or bracket."
                   lbeg
                   lend)))
    (`(open ,obeg ,_)
     (goto-char obeg)
     (goto-char (scan-lists (point) 1 0))
     (sweeprolog--forward-term pre))
    (`(functor ,_ ,oend)
     (goto-char (1- oend))
     (goto-char (scan-lists (point) 1 0))
     (sweeprolog--forward-term pre))
    (`(operator ,obeg ,oend)
     (if (and (string= "." (buffer-substring-no-properties obeg oend))
              (member (char-syntax (char-after (1+ obeg))) '(?> ? )))
         (signal 'scan-error
                 (list "Cannot scan beyond fullstop."
                       obeg
                       (1+ obeg)))
       (if-let ((opre (sweeprolog-op-infix-precedence
                       (buffer-substring-no-properties obeg oend))))
           (if (> opre pre)
               (signal 'scan-error
                       (list (format "Cannot scan beyond infix operator of higher precedence %s." opre)
                             obeg
                             oend))
             (goto-char oend)
             (sweeprolog--forward-term pre))
         (if-let ((ppre (sweeprolog-op-suffix-precedence
                         (buffer-substring-no-properties obeg oend))))
             (if (> ppre pre)
                 (signal 'scan-error
                         (list (format "Cannot scan beyond suffix operator of higher precedence %s." opre)
                               obeg
                               oend))
               (goto-char oend)
               (sweeprolog--forward-term pre))
           (goto-char oend)
           (sweeprolog--forward-term pre)))))
    (`(symbol ,obeg ,oend)
     (if-let ((opre (sweeprolog-op-infix-precedence
                     (buffer-substring-no-properties obeg oend))))
         (if (> opre pre)
             (signal 'scan-error
                     (list (format "Cannot scan backwards infix operator of higher precedence %s." opre)
                           obeg
                           oend))
           (goto-char oend)
           (sweeprolog--forward-term pre))
       (if-let ((ppre (sweeprolog-op-prefix-precedence
                       (buffer-substring-no-properties obeg oend))))
           (if (> ppre pre)
               (signal 'scan-error
                       (list (format "Cannot scan backwards beyond prefix operator of higher precedence %s." opre)
                             obeg
                             oend))
             (goto-char oend)
             (sweeprolog--forward-term pre))
         (goto-char oend)
         (sweeprolog--forward-term pre))))
    (`(,_ ,_ ,oend)
     (goto-char oend)
     (sweeprolog--forward-term pre))))

(defun sweeprolog-forward-term (pre)
  (condition-case _
      (sweeprolog--forward-term pre)
    (scan-error nil)))

(defun sweeprolog--backward-term (pre)
  (let ((infix-flag t))
    (while t
      (pcase (sweeprolog-last-token-boundaries)
        ('nil
         (signal 'scan-error
                 (list "Cannot scan backwards beyond beginning of buffer."
                       (point-min)
                       (point-min))))
        (`(open ,obeg ,oend)
         (signal 'scan-error
                 (list "Cannot scan backwards beyond opening parenthesis or bracket."
                       obeg
                       oend)))
        (`(functor ,obeg ,oend)
         (signal 'scan-error
                 (list "Cannot scan backwards beyond functor."
                       obeg
                       oend)))
        (`(operator ,obeg ,oend)
         (if (and (string= "." (buffer-substring-no-properties obeg oend))
                  (or (not (char-after (1+ obeg)))
                      (member (char-syntax (char-after (1+ obeg)))
                              '(?> ? ))))
             (signal 'scan-error
                     (list "Cannot scan backwards beyond fullstop."
                           obeg
                           (1+ obeg)))
           (if-let ((opre (sweeprolog-op-infix-precedence
                           (buffer-substring-no-properties obeg oend))))
               (if (> opre pre)
                   (signal 'scan-error
                           (list (format "Cannot scan backwards beyond infix operator of higher precedence %s." opre)
                                 obeg
                                 oend))
                 (goto-char obeg)
                 (setq infix-flag t))
             (if-let ((ppre (sweeprolog-op-prefix-precedence
                             (buffer-substring-no-properties obeg oend))))
                 (if (> ppre pre)
                     (signal 'scan-error
                             (list (format "Cannot scan backwards beyond prefix operator of higher precedence %s." opre)
                                   obeg
                                   oend))
                   (goto-char obeg)
                   (setq infix-flag nil))
               (goto-char obeg)
               (setq infix-flag nil)))))
        (`(symbol ,obeg ,oend)
         (if-let ((opre (sweeprolog-op-infix-precedence (buffer-substring-no-properties obeg oend))))
             (if (> opre pre)
                 (signal 'scan-error
                         (list (format "Cannot scan backwards beyond infix operator of higher precedence %s." opre)
                               obeg
                               oend))
               (goto-char obeg)
               (setq infix-flag t))
           (if-let ((ppre (and (not infix-flag)
                               (sweeprolog-op-prefix-precedence (buffer-substring-no-properties obeg oend)))))
               (if (> ppre pre)
                   (signal 'scan-error
                           (list (format "Cannot scan backwards beyond prefix operator of higher precedence %s." opre)
                                 obeg
                                 oend))
                 (goto-char obeg)
                 (setq infix-flag nil))
             (goto-char obeg)
             (setq infix-flag nil))))
        (`(close ,lbeg ,_lend)
         (goto-char (nth 1 (syntax-ppss lbeg)))
         (when (or (= (char-syntax (char-before)) ?w)
                   (= (char-syntax (char-before)) ?_))
           (skip-syntax-backward "w_"))
         (setq infix-flag nil))
        (`(,_ ,lbeg ,_)
         (goto-char lbeg)
         (setq infix-flag nil))))))

(defun sweeprolog-backward-term (pre)
  (condition-case _
      (sweeprolog--backward-term pre)
    (scan-error nil)))

(defun sweeprolog--backward-sexp ()
  (let ((point (point))
        (prec (pcase (sweeprolog-last-token-boundaries)
                (`(operator ,obeg ,oend)
                 (unless (and nil
                              (string= "." (buffer-substring-no-properties obeg oend))
                              (member (char-syntax (char-after (1+ obeg))) '(?> ? )))
                   (if-let ((pprec
                             (sweeprolog-op-infix-precedence
                              (buffer-substring-no-properties obeg oend))))
                       (progn (goto-char obeg) (1- pprec))
                     0)))
                (_ 0))))
    (condition-case error
        (sweeprolog--backward-term prec)
      (scan-error (when (= point (point))
                    (signal 'scan-error (cdr error)))))))

(defun sweeprolog--forward-sexp ()
  (let ((point (point))
        (prec (pcase (sweeprolog-next-token-boundaries)
                (`(operator ,obeg ,oend)
                 (unless (and nil
                              (string= "." (buffer-substring-no-properties obeg oend))
                              (member (char-syntax (char-after (1+ obeg))) '(?> ? )))
                   (if-let ((pprec
                             (sweeprolog-op-infix-precedence
                              (buffer-substring-no-properties obeg oend))))
                       (progn (goto-char oend) (1- pprec))
                     0)))
                (_ 0))))
    (condition-case error
        (sweeprolog--forward-term prec)
      (scan-error (when (= point (point))
                    (signal 'scan-error (cdr error)))))))

(defun sweeprolog-forward-sexp-function (arg)
  (let* ((times (abs arg))
         (func  (or (and (not (= arg 0))
                         (< 0 (/ times arg))
                         #'sweeprolog--forward-sexp)
                    #'sweeprolog--backward-sexp)))
    (while (< 0 times)
      (funcall func)
      (setq times (1- times)))))

(defun sweeprolog-op-suffix-precedence (token)
  (sweeprolog--open-query "user" "sweep" "sweep_op_info" (cons token (buffer-file-name)))
  (let ((res nil) (go t))
    (while go
      (if-let ((sol (sweeprolog-next-solution))
               (det (car sol))
               (fix (cadr sol))
               (pre (cddr sol)))
          (if (member fix '("xf" "yf"))
              (setq res pre go nil)
            (when (eq '! det)
              (setq go nil)))
        (setq go nil)))
    (sweeprolog-close-query)
    res))

(defun sweeprolog-op-prefix-precedence (token)
  (sweeprolog--open-query "user" "sweep" "sweep_op_info" (cons token (buffer-file-name)))
  (let ((res nil) (go t))
    (while go
      (if-let ((sol (sweeprolog-next-solution))
               (det (car sol))
               (fix (cadr sol))
               (pre (cddr sol)))
          (if (member fix '("fx" "fy"))
              (setq res pre go nil)
            (when (eq '! det)
              (setq go nil)))
        (setq go nil)))
    (sweeprolog-close-query)
    res))

(defun sweeprolog-op-infix-precedence (token)
  (sweeprolog--open-query "user" "sweep" "sweep_op_info" (cons token (buffer-file-name)))
  (let ((res nil) (go t))
    (while go
      (if-let ((sol (sweeprolog-next-solution))
               (det (car sol))
               (fix (cadr sol))
               (pre (cddr sol)))
          (if (member fix '("xfx" "xfy" "yfx"))
              (setq res pre go nil)
            (when (eq '! det)
              (setq go nil)))
        (setq go nil)))
    (sweeprolog-close-query)
    res))

(defun sweeprolog-local-predicate-export-comment (fun ari)
  (sweeprolog--query-once "sweep" "sweep_local_predicate_export_comment"
                          (list (buffer-file-name) fun ari)))

(defun sweeprolog-exportable-predicates ()
  "Return a list of exportable predicates from the current buffer."
  (sweeprolog-xref-buffer)
  (sweeprolog--query-once "sweep" "sweep_exportable_predicates"
                          (buffer-file-name)))

(defun sweeprolog-read-exportable-predicate ()
  "Read a predicate name that can be exported in the current buffer."
  (completing-read sweeprolog-read-exportable-predicate-prompt
                   (sweeprolog-exportable-predicates)))

(defun sweeprolog-export-predicate (pred &optional comm)
  "Add PRED to the export list of the current module.
Optional argument COMM is a comment to insert after the PRED in
the export list.

Interactively, if called without a prefix argument and point is
near a predicate definiton, export that predicate and derive the
export comment from its PlDoc comment.  Otherwise, prompt for a
predicate to export providing completion candidates based on the
non-exported predicates defined in the current buffer."
  (interactive (or (and (not current-prefix-arg)
                        (when-let ((def (sweeprolog-definition-at-point))
                                   (fun (cadr def))
                                   (ari (caddr def)))
                          (list (concat fun "/" (number-to-string ari))
                                (sweeprolog-local-predicate-export-comment fun ari))))
                   (list
                    (sweeprolog-read-exportable-predicate)
                    (read-string "Export comment: ")))
               sweeprolog-mode)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (unless (or (sweeprolog-at-beginning-of-top-term-p)
                  (sweeprolog-beginning-of-next-top-term))
        (user-error "No module declaration found"))
      (let* ((target-position nil)
             (module-term-beg nil)
             (module-term-end nil)
             (exported-operator nil)
             (func (lambda (beg end arg)
                     (pcase arg
                       (`("goal_term" "built_in" "module" 2)
                        (setq module-term-beg beg
                              module-term-end end))
                       ((or "list"
                            "empty_list")
                        (when (and (not target-position)
                                   (< module-term-beg beg)
                                   (< end module-term-end))
                          (setq target-position (1- end))))
                       ("exported_operator"
                        (unless exported-operator
                          (setq exported-operator t
                                target-position beg)))))))
        (sweeprolog-analyze-term-at-point func)
        (unless (member pred (sweeprolog-exportable-predicates))
          (user-error "Cannot add %s to export list" pred))
        (if target-position
            (save-excursion
              (goto-char target-position)
              (let ((pos (- (point-max) (line-end-position))))
                (combine-after-change-calls
                  (if exported-operator
                      (progn
                        (insert pred ",\n")
                        (backward-char)
                        (when (and comm (not (string-empty-p comm)))
                          (insert "  % " comm))
                        (indent-region module-term-beg (- (point-max) pos))
                        (align-regexp module-term-beg (- (point-max) pos)
                                      (rx (group (zero-or-more blank)) "%")))
                    (pcase (sweeprolog-last-token-boundaries)
                      (`(open ,_ ,_)
                       (insert pred)
                       (when (and comm (not (string-empty-p comm)))
                         (insert "  % " comm))
                       (insert "\n")
                       (indent-region module-term-beg (1+ (point))))
                      (`(symbol ,_ ,oend)
                       (let ((point (point)))
                         (goto-char oend)
                         (insert ",")
                         (goto-char (1+ point))
                         (insert "\n")
                         (backward-char)
                         (insert pred)
                         (when (and comm (not (string-empty-p comm)))
                           (insert "  % " comm))
                         (indent-region module-term-beg (- (point-max) pos))
                         (align-regexp module-term-beg (- (point-max) pos)
                                       (rx (group (zero-or-more blank)) "%"))))
                      (tok (user-error "Unexpected token %s while looking for export list" tok))))))
              (sweeprolog-analyze-buffer t)
              (message "Exported %s" pred))
          (user-error "No export list found"))))))

(defun sweeprolog-align-spaces (&optional _)
  "Adjust in-line spaces between the previous and next Prolog tokens.

This command ensures that the next token starts at a column
distanced from the beginning of the previous by a multiple of
four columns, which accommodates the convetional alignment for
if-then-else constructs and other common layouts in SWI-Prolog."
  (interactive "" sweeprolog-mode)
  (let ((bol (line-beginning-position)))
    (if (nth 4 (syntax-ppss))
        (combine-after-change-calls
          (delete-horizontal-space)
          (let* ((lend (point))
                 (lbeg (save-excursion
                        (while (and (< bol (point))
                                    (not (= (char-syntax (char-before))
                                            ? )))
                          (forward-char -1))
                        (point)))
                 (num (- 4 (% (- lend lbeg) 4))))
            (insert (make-string (if (< 0 num)
                                     num
                                   4) ? )))))
    (pcase (sweeprolog-last-token-boundaries)
      (`(,_ ,lbeg ,lend)
       (when (<= bol lend)
         (let ((num (- 4 (% (- lend lbeg) 4))))
           (when (< 0 num)
             (goto-char lend)
             (combine-after-change-calls
               (delete-horizontal-space)
               (insert (make-string num ? ))))))))))

(defun sweeprolog-electric-layout-post-self-insert-function ()
  "Adjust whitespace around point according to Prolog conventions.

This function is added to ‘post-self-insert-hook’ by
`sweeprolog-electric-layout-mode'."
  (if (nth 8 (syntax-ppss))
      (when (member (buffer-substring-no-properties (line-beginning-position)
                                                    (point))
                    '("%%" "%!"))
        (insert "  "))
    (let ((inserted (char-before)))
      (cond
       ((member inserted (string-to-list "(;>"))
        (pcase (sweeprolog-last-token-boundaries)
          ((or `(open     ,beg ,end)
               `(operator ,beg ,end))
           (when (and (member (buffer-substring-no-properties beg end)
                              '("(" ";" "->" "*->"))
                      (sweeprolog-context-callable-p))
             (insert (make-string (+ 4 beg (- end)) ? ))))))
       ((= (char-syntax inserted) ?\))
        (sweeprolog-indent-line))))))

;;;###autoload
(define-minor-mode sweeprolog-electric-layout-mode
  "Automatically adjust whitespace in `sweeprolog-mode' buffers.

When enabled, spaces are automatically inserted as you type in
certain contexts to maintain conventional Prolog layout."
  :group 'sweeprolog
  (if sweeprolog-electric-layout-mode
      (add-hook 'post-self-insert-hook
                #'sweeprolog-electric-layout-post-self-insert-function
                nil t)
    (remove-hook 'post-self-insert-hook
                 #'sweeprolog-electric-layout-post-self-insert-function
                 t)))

(defun sweeprolog--update-buffer-last-modified-time (&rest _)
  (setq sweeprolog--buffer-last-modified-time (float-time)
        sweeprolog--buffer-modified t))

;;;###autoload
(define-derived-mode sweeprolog-mode prog-mode "Sweep"
  "Major mode for reading and editing Prolog code."
  :group 'sweeprolog
  (setq-local comment-start "%")
  (setq-local comment-start-skip "\\(?:/\\*+ *\\|%+ *\\)")
  (setq-local parens-require-spaces nil)
  (setq-local imenu-create-index-function #'sweeprolog-create-index-function)
  (setq-local beginning-of-defun-function #'sweeprolog-beginning-of-top-term)
  (setq-local end-of-defun-function #'sweeprolog-end-of-top-term)
  (setq-local forward-sexp-function #'sweeprolog-forward-sexp-function)
  (setq-local syntax-propertize-function #'sweeprolog-syntax-propertize)
  (setq-local indent-line-function #'sweeprolog-indent-line)
  (setq-local adaptive-fill-regexp "[ \t]*")
  (setq-local fill-indent-according-to-mode t)
  (setq-local comment-multi-line t)
  (setq-local mode-line-process
              '(:eval
                (when (sweeprolog-buffer-loaded-since-last-modification-p)
                  "/Loaded")))
  (setq-local font-lock-defaults
              '(nil
                nil
                nil
                nil
                nil
                (font-lock-fontify-region-function . sweeprolog-analyze-some-terms)))
  (add-hook 'after-change-functions
            #'sweeprolog--update-buffer-last-modified-time nil t)
  (add-hook 'after-change-functions
            #'sweeprolog-analyze-some-terms nil t)
  (when sweeprolog-enable-eldoc
    (when (fboundp 'eldoc-documentation-default)
      (setq-local eldoc-documentation-strategy #'eldoc-documentation-default))
    (add-hook 'eldoc-documentation-functions #'sweeprolog-predicate-modes-doc nil t))
  (when sweeprolog-enable-help-echo
    (add-hook 'sweeprolog-analyze-region-start-hook #'sweeprolog-analyze-start-help-echo nil t)
    (add-hook 'sweeprolog-analyze-region-fragment-hook #'sweeprolog-analyze-fragment-help-echo nil t))
  (when sweeprolog-enable-flymake
    (add-hook 'flymake-diagnostic-functions #'sweeprolog-diagnostic-function nil t)
    (flymake-mode)
    (add-hook 'sweeprolog-analyze-region-start-hook #'sweeprolog-analyze-start-flymake nil t)
    (add-hook 'sweeprolog-analyze-region-fragment-hook #'sweeprolog-analyze-fragment-flymake nil t)
    (add-hook 'sweeprolog-analyze-region-end-hook #'sweeprolog-analyze-end-flymake nil t)
    (setq-local next-error-function #'flymake-goto-next-error))
  (when (and (boundp 'cycle-spacing-actions)
             (consp cycle-spacing-actions)
             sweeprolog-enable-cycle-spacing)
    (setq-local cycle-spacing-actions
                (cons #'sweeprolog-align-spaces
                      cycle-spacing-actions)))
  (sweeprolog-ensure-initialized)
  (sweeprolog--update-buffer-last-modified-time)
  (let ((time (current-time)))
    (sweeprolog-analyze-buffer t)
    (setq sweeprolog--analyze-buffer-duration (float-time (time-since time))))
  (add-hook 'xref-backend-functions #'sweeprolog--xref-backend nil t)
  (add-hook 'file-name-at-point-functions #'sweeprolog-file-at-point nil t)
  (dolist (capf sweeprolog-completion-at-point-functions)
    (add-hook 'completion-at-point-functions capf nil t))
  (when sweeprolog-analyze-buffer-on-idle
    (setq sweeprolog--timer
          (run-with-idle-timer
           (max sweeprolog-analyze-buffer-min-interval
                (* 10 sweeprolog--analyze-buffer-duration))
           t
           (let ((buffer (current-buffer)))
             (lambda ()
               (when (and (buffer-live-p buffer)
                          (not (< sweeprolog-analyze-buffer-max-size
                                  (buffer-size buffer)))
                          (get-buffer-window buffer))
                 (with-current-buffer buffer
                   (sweeprolog-analyze-buffer)))))))
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (timerp sweeprolog--timer)
                  (cancel-timer sweeprolog--timer)))))
  (when sweeprolog-enable-cursor-sensor
    (add-hook 'sweeprolog-analyze-region-fragment-hook
              #'sweeprolog-analyze-fragment-variable nil t)
    (cursor-sensor-mode 1))
  (when (boundp 'context-menu-functions)
    (add-hook 'context-menu-functions
              #'sweeprolog-context-menu-function))
  (unless (member 'sweeprolog-hole yank-excluded-properties)
   (setq-local yank-excluded-properties
               (cons 'sweeprolog-hole yank-excluded-properties))))


;;;; Skeletons and auto-insert

(defun sweeprolog-format-string-as-atom (string)
  "Return STRING formatted as a Prolog atom.

Notably, the returned string is quoted if required to make it a
valid Prolog atom."
  (sweeprolog--query-once "sweep" "sweep_string_to_atom" string))

(define-skeleton sweeprolog-plunit-testset-skeleton
  "Insert PlUnit test-set skeleton."
  (let* ((fn (buffer-file-name))
         (def (when fn (file-name-base fn))))
    (sweeprolog-format-string-as-atom
     (read-string (concat "Test set name"
                          (when def (concat " (default " def ")"))
                          ": ")
                  nil nil def)))
  ":- begin_tests(" str ").\n\n"
  "test(" _ ") :- " (sweeprolog--hole "TestBody") ".\n\n"
  ":- end_tests(" str ").\n")

(define-skeleton sweeprolog-module-header-skeleton
  "Insert SWI-Prolog module header skeleton."
  (sweeprolog-format-string-as-atom
   (or (and (buffer-file-name)
            (file-name-sans-extension
             (file-name-base (buffer-file-name))))
       (read-string "Module name: ")))
  "/*"
  "\n    Author:        " (progn user-full-name)
  "\n    Email:         " (progn user-mail-address)
  (progn sweeprolog-module-header-comment-skeleton)
  "\n*/\n\n:- module(" str ", []).\n\n/** <module> "
  -
  "\n\n*/\n\n")

(define-auto-insert
  '(sweeprolog-mode . "SWI-Prolog module header")
  #'sweeprolog-module-header-skeleton)


;;;; Indentation

(defun sweeprolog-infer-indent-style ()
  "Infer indentation style for the current buffer from its contents.

Examine the indentation of the first clause body starting on a
line of its own and update the buffer-local values of
`sweeprolog-indent-offset' and `indent-tabs-mode' are updated
accordingly."
  (interactive "" sweeprolog-mode)
  (let ((offset nil)
        (usetab nil))
    (sweeprolog-analyze-buffer-with
     (lambda (beg _end arg)
       (pcase arg
         ("body"
          (unless offset
            (save-excursion
              (goto-char beg)
              (let ((prefix (buffer-substring (line-beginning-position)
                                              (point))))
                (save-match-data
                  (cond
                   ((string-match (rx bos (+ " ") eos)
                                  prefix)
                    (setq offset (current-column)))
                   ((string-match (rx bos (+ (or " " "\t")) eos)
                                  prefix)
                    (setq offset (current-column)
                          usetab t)))))))))))
    (if (not offset)
        (message (concat "No indented body term found in"
                         " current buffer.  Keeping current"
                         " indentation style: offset=%s tabs=%s")
                 sweeprolog-indent-offset
                 indent-tabs-mode)
      (message "Inferred indentation style: offset=%s tabs=%s"
               offset usetab)
      (setq-local sweeprolog-indent-offset offset
                  indent-tabs-mode usetab))))

(defun sweeprolog-indent-line-after-functor (fbeg _fend)
  (save-excursion
    (goto-char fbeg)
    (+ (current-column) sweeprolog-indent-offset)))

(defun sweeprolog-indent-line-after-open (fbeg _fend)
  (save-excursion
    (goto-char fbeg)
    (+ (current-column) sweeprolog-indent-offset)))

(defun sweeprolog-indent-line-after-prefix (fbeg _fend _pre)
  (save-excursion
    (goto-char fbeg)
    (+ (current-column) sweeprolog-indent-offset)))

(defun sweeprolog-indent-line-after-term ()
  (if-let ((open (nth 1 (syntax-ppss))))
      (save-excursion
        (goto-char open)
        (current-column))
    'noindent))

(defun sweeprolog-indent-line-after-neck (fbeg _fend)
  (save-excursion
    (goto-char fbeg)
    (sweeprolog-backward-term 1200)
    (+ (current-column) sweeprolog-indent-offset)))

(defun sweeprolog-indent-line-after-infix (fbeg _fend pre)
  (save-excursion
    (goto-char fbeg)
    (sweeprolog-backward-term (1- pre))
    (let ((go t)
          (line-beg (line-beginning-position)))
      (while go
        (pcase (sweeprolog-last-token-boundaries)
          ((or `(operator ,lbeg ,lend)
               `(symbol ,lbeg ,lend))
           (if-let ((sym  (buffer-substring-no-properties lbeg lend))
                    (ppre (sweeprolog-op-infix-precedence sym))
                    (res (= ppre pre)))
               (if (< lbeg line-beg)
                   (setq go nil)
                 (goto-char lbeg)
                 (sweeprolog-backward-term (1- pre)))
             (setq go nil)))
          (_ (setq go nil))))
      (let ((col (current-column)))
        (if (= col 0)
             (/ sweeprolog-indent-offset 2)
          col)))))

(defun sweeprolog-indent-line ()
  "Indent the current line in a `sweeprolog-mode' buffer."
  (interactive)
  (let ((pos (- (point-max) (point))))
    (back-to-indentation)
    (let ((column (if (nth 8 (syntax-ppss))
                      'noindent
                    (if-let ((open (and (not (eobp))
                                        (= (char-syntax (char-after)) ?\))
                                        (nth 1 (syntax-ppss)))))
                        (save-excursion
                          (goto-char open)
                          (when (and (char-before)
                                     (or (= (char-syntax (char-before)) ?w)
                                         (= (char-syntax (char-before)) ?_)))
                            (when (save-excursion
                                    (forward-char)
                                    (skip-syntax-forward " " (line-end-position))
                                    (eolp))
                              (skip-syntax-backward "w_")))
                          (current-column))
                      (pcase (sweeprolog-last-token-boundaries)
                        ('nil 'noindent)
                        (`(functor ,lbeg ,lend)
                         (sweeprolog-indent-line-after-functor lbeg lend))
                        (`(open ,lbeg ,lend)
                         (sweeprolog-indent-line-after-open lbeg lend))
                        (`(symbol ,lbeg ,lend)
                         (let ((sym (buffer-substring-no-properties lbeg lend)))
                           (cond
                            ((pcase (sweeprolog-op-prefix-precedence sym)
                               ('nil (sweeprolog-indent-line-after-term))
                               (pre  (sweeprolog-indent-line-after-prefix lbeg lend pre)))))))
                        (`(operator ,lbeg ,lend)
                         (let ((op (buffer-substring-no-properties lbeg lend)))
                           (cond
                            ((string= op ".") 'noindent)
                            ((pcase (sweeprolog-op-infix-precedence op)
                               ('nil nil)
                               (1200 (sweeprolog-indent-line-after-neck lbeg lend))
                               (pre  (sweeprolog-indent-line-after-infix lbeg lend pre))))
                            ((pcase (sweeprolog-op-prefix-precedence op)
                               ('nil nil)
                               (pre  (sweeprolog-indent-line-after-prefix lbeg lend pre)))))))
                        (`(,_ltyp ,_lbeg ,_lend)
                         (sweeprolog-indent-line-after-term)))))))
      (when (numberp column)
        (unless (= column (current-column))
          (combine-after-change-calls
            (delete-horizontal-space)
            (indent-to column))))
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))
      column)))


;;;; Xref

(defun sweeprolog--xref-backend ()
  "Hook for `xref-backend-functions'."
  'sweeprolog)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql sweeprolog)))
  (sweeprolog-identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql sweeprolog)))
  (completion-table-with-cache #'sweeprolog-predicates-collection))

(cl-defmethod xref-backend-identifier-completion-ignore-case ((_backend (eql sweeprolog)))
  "Case is always significant for Prolog identifiers, so return nil."
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql sweeprolog)) mfn)
  (when-let ((loc (sweeprolog-predicate-location mfn))
             (path (car loc))
             (line (or (cdr loc) 1)))
    (list (xref-make (concat path ":" (number-to-string line)) (xref-make-file-location path line 0)))))

(cl-defmethod xref-backend-references ((_backend (eql sweeprolog)) mfn)
  (let ((xref-items nil)
        (refs (sweeprolog-predicate-references mfn)))
    (dolist-with-progress-reporter (loc refs)
        "Formatting cross references... "
      (push (let* ((by   (nth 0 loc))
                   (file (nth 1 loc))
                   (beg  (nth 2 loc))
                   (buf (find-file-noselect file t)))
              (xref-make (format "Call from %s at line %s" by
                                 (with-current-buffer buf
                                   (line-number-at-pos beg t)))
                         (xref-make-buffer-location buf beg)))
            xref-items))
    (reverse xref-items)))

(cl-defmethod xref-backend-apropos ((_backend (eql sweeprolog)) pattern)
  (let ((matches (sweeprolog-predicate-apropos pattern)))
    (seq-map (lambda (match)
               (let ((mfn (car match))
                     (path (cadr match))
                     (line (or (cddr match) 1)))
                 (xref-make mfn
                            (xref-make-file-location path line 0))))
             matches)))


;;;; Imenu

(defun sweeprolog-create-index-function ()
  (seq-map (lambda (entry)
             (let ((car (car entry))
                   (line (cdr entry)))
               (goto-char (point-min))
               (forward-line (1- line))
               (cons car (line-beginning-position))))
           (sweeprolog--query-once "sweep" "sweep_imenu_index"
                                   (buffer-file-name))))


;;;; ElDoc

(defun sweeprolog-predicate-modes-doc (cb)
  (when-let ((pi (sweeprolog-identifier-at-point))
             (docs (sweeprolog--query-once "sweep" "sweep_documentation"
                                           pi)))
    (funcall cb (car docs) :thing pi :face font-lock-function-name-face)))


;;;; Top-level Menu

(defun sweeprolog-top-level-menu--entries ()
  (mapcar (lambda (th)
            (let ((id (nth 0 th))
                  (bn (nth 1 th))
                  (st (nth 2 th))
                  (sz (number-to-string (nth 3 th)))
                  (ct (number-to-string (nth 4 th))))
              (list id (vector bn st sz ct))))
          (sweeprolog--query-once "sweep" "sweep_top_level_threads" nil)))

(defun sweeprolog-top-level-menu--refresh ()
  (tabulated-list-init-header)
  (setq tabulated-list-entries (sweeprolog-top-level-menu--entries)))

(defun sweeprolog-top-level-menu-new (name)
  "Create and switch to a new Prolog top-level buffer named NAME."
  (interactive (list
                (read-string
                 "New top-level buffer name: "
                 nil nil
                 (generate-new-buffer-name "*sweeprolog-top-level*")))
               sweeprolog-top-level-menu-mode)
  (sweeprolog-top-level name))

(defun sweeprolog-top-level-menu-signal (goal)
  "Signal the thread of to Top-level Menu entry at point to run GOAL."
  (interactive (list (read-string "Signal goal: ?- "))
               sweeprolog-top-level-menu-mode)
  (if-let ((tid (tabulated-list-get-id)))
      (sweeprolog-signal-thread tid goal)
    (user-error "No top-level menu entry here")))

(defun sweeprolog-top-level-menu-kill ()
  "Kill the buffer of to the `sweep' Top-level Menu entry at point."
  (interactive "" sweeprolog-top-level-menu-mode)
  (if-let ((vec (tabulated-list-get-entry)))
      (let ((bn (seq-elt vec 0)))
        (kill-buffer bn))
    (user-error "No top-level menu entry here")))

(defun sweeprolog-top-level-menu-go-to ()
  "Go to the buffer of to the `sweep' Top-level Menu entry at point."
  (interactive "" sweeprolog-top-level-menu-mode)
  (if-let ((vec (tabulated-list-get-entry)))
      (let* ((bn (seq-elt vec 0))
             (buf (get-buffer bn)))
        (if (buffer-live-p buf)
            (pop-to-buffer buf sweeprolog-top-level-display-action)
          (message "Buffer %s is no longer availabe." bn)))
    (user-error "No top-level menu entry here")))

(define-derived-mode sweeprolog-top-level-menu-mode
  tabulated-list-mode "Sweep Top-level Menu"
  "Major mode for browsing a list of active `sweep' top-levels."
  (setq tabulated-list-format [("Buffer"      32 t)
                               ("Status"      32 t)
                               ("Stacks Size" 20 t)
                               ("CPU Time"    20 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Buffer" nil))
  (add-hook 'tabulated-list-revert-hook
            #'sweeprolog-top-level-menu--refresh nil t)
  (tabulated-list-init-header))

(defun sweeprolog-list-top-levels ()
  "Display a list of Prolog top-level threads."
  (interactive)
  (let ((buf (get-buffer-create "*sweep Top-levels*")))
    (with-current-buffer buf
      (sweeprolog-top-level-menu-mode)
      (sweeprolog-top-level-menu--refresh)
      (tabulated-list-print))
    (pop-to-buffer-same-window buf)))


;;;; Help

;;;###autoload
(defun sweeprolog-info-manual ()
  "Display the Sweep manual in Info mode."
  (interactive)
  (info "sweep"))

(info-lookup-maybe-add-help
 :mode     (cons 'emacs-lisp-mode "sweeprolog")
 :regexp   (concat "\\bsweeprolog-[^][()`'‘’,\" \t\n]+")
 :doc-spec '(("(sweep)Function Index" nil "^ -+ .*: " "\\( \\|$\\)")
             ("(sweep)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)")))

(add-to-list 'Info-file-list-for-emacs (cons "sweeprolog" "sweep"))

;;;###autoload
(defun sweeprolog-view-news ()
  "View the Sweep News file, which lists recent changes to Sweep."
  (interactive)
  (view-file (expand-file-name "NEWS.org" sweeprolog--directory)))

(defun sweeprolog--buttonize (string callback data)
  (if (fboundp 'buttonize)
      (buttonize string callback data)
    (if (fboundp 'button-buttonize)
        (button-buttonize string callback data)
      (propertize string
                  'face 'button
                  'button t
                  'follow-link t
                  'category t
                  'button-data data
                  'keymap button-map
                  'action callback))))

(defun sweeprolog--buttonize-region (start end callback data)
  (if (fboundp 'buttonize-region)
      (buttonize-region start end callback data)
    (add-text-properties start end
                         (list 'font-lock-face 'button
                               'mouse-face 'highlight
                               'help-echo nil
                               'button t
                               'follow-link t
                               'category t
                               'button-data data
                               'keymap button-map
                               'action callback))
    (add-face-text-property start end 'button t)))

(defun sweeprolog-render-html-span (dom)
  (if (string= "fn-text" (dom-attr dom 'class))
      (progn (insert " ")
             (push dom sweeprolog--html-footnotes))
    (shr-tag-span dom)))

(defun sweeprolog-render-html-a (dom)
  (let* ((url (dom-attr dom 'href))
         (class (dom-attr dom 'class))
         (parsed (url-generic-parse-url url))
         (target (url-target parsed))
         (start (point)))
    (shr-generic dom)
    (cond
     ((url-host parsed))
     (target
      (when (string-match (rx (one-or-more anychar)
                              "/"
                              (one-or-more digit) eos)
                          target)
        (sweeprolog--buttonize-region start
                                      (point)
                                      #'sweeprolog-describe-predicate
                                      target)))
     (t (let* ((path-and-query (url-path-and-query parsed))
               (path (car path-and-query))
               (query (cdr path-and-query)))
          (cond
           ((string= class "file")
            (let ((dir (file-name-directory path))
                  (base (file-name-base path)))
              (when (string= dir "/pldoc/doc/_SWI_/library/")
                (sweeprolog--buttonize-region start
                                              (point)
                                              #'find-file
                                              (concat "library(" base ")")))))
           ((string= path "/pldoc/man")
            (pcase (url-parse-query-string query)
              (`(("predicate" ,pred))
               (sweeprolog--buttonize-region start
                                             (point)
                                             #'sweeprolog-describe-predicate
                                             pred))))))))))

(defun sweeprolog-render-html (html)
  (with-temp-buffer
    (insert html)
    (setq sweeprolog--html-footnotes nil)
    (let ((shr-external-rendering-functions
           '((a . sweeprolog-render-html-a)
             (var . shr-tag-i)
             (span . sweeprolog-render-html-span))))
      (shr-render-region (point-min) (point-max))
      (goto-char (point-max))
      (when sweeprolog--html-footnotes
        (insert "\n\nFootnotes:")
       (dolist (footnote sweeprolog--html-footnotes)
         (insert "\n\n")
         (shr-tag-span footnote))))
    (buffer-string)))

(defun sweeprolog--describe-module (mod)
  (let ((page
         (when-let ((html (sweeprolog--query-once "sweep"
                                                  "sweep_module_html_documentation"
                                                  mod)))
           (sweeprolog-render-html html))))
    (help-setup-xref (list #'sweeprolog--describe-module mod)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (if-let ((path (sweeprolog-module-path mod)))
            (progn
              (setq help-mode--current-data
                    (list :symbol (intern mod)
                          :type   'swi-prolog-module
                          :file   path))
              (if page
                  (insert (sweeprolog--buttonize mod #'sweeprolog-find-module mod)
                          " is a SWI-Prolog module.\n\n"
                          page)
                (insert (sweeprolog--buttonize mod #'sweeprolog-find-module mod)
                        " is an undocumented SWI-Prolog module.")))
          (insert mod " is not documented as a SWI-Prolog module."))))))

;;;###autoload
(defun sweeprolog-describe-module (mod)
  "Display the full documentation for MOD (a Prolog module)."
  (interactive (list (sweeprolog-read-module-name)))
  (sweeprolog--describe-module mod))

(defun sweeprolog--render-predicate-properties (props)
  (concat
   "\n\nPredicate properties: "
   (string-join
    (delq nil
          (mapcar
           (lambda (prop)
             (pcase prop
               (`(atom . "built_in") "built-in")
               (`(atom . "dynamic") "dynamic")
               (`(atom . "foreign") "foreign")
               (`(atom . "quasi_quotation_syntax")
                "defines a quasi-quotation syntax")
               (`(atom . "ssu")
                "defined using single sided unification rules")
               (`(atom . "tabled") "tabled")
               (`(atom . "thread_local") "thread-local")
               (`(atom . "transparent") "module-transparent")
               (`(atom . "volatile") "volatile.")
               (`(atom . "iso") "specified in the ISO standard")
               (`(atom . "multifile") "multifile")
               (`(compound "meta_predicate" . ,_) "meta-predicate")
               (`(atom . "non_terminal") "DCG non-terminal")
               (`(compound "size" ,s) (concat (number-to-string s)
                                              " bytes"))
               (`(compound "number_of_clauses" ,n)
                (concat "has "
                        (pcase n
                          (1 "one clause")
                          (_ (concat (number-to-string n)
                                     " clauses")))))
               (`(atom . "det") "deterministic")
               (`(atom . "discontiguous") "discontiguous")
               (`(atom . "exported") "exported")))
           props))
    ", ")
   "."))

(defun sweeprolog-predicate-properties (pred)
  (sweeprolog--query-once "sweep" "sweep_predicate_properties" pred))

(defun sweeprolog--describe-predicate (pred)
  (let ((page
         (when-let
             ((html
               (sweeprolog--query-once "sweep" "sweep_predicate_html_documentation"
                                       pred)))
           (sweeprolog-render-html html)))
        (props (sweeprolog-predicate-properties pred))
        (path (car (sweeprolog-predicate-location pred))))
    (help-setup-xref (list #'sweeprolog--describe-predicate pred)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (if path
            (progn (setq help-mode--current-data
                         (list :symbol (intern pred)
                               :type   'swi-prolog-predicate
                               :file   path))
                   (insert (sweeprolog--buttonize pred #'sweeprolog-find-predicate pred)
                           (if page
                               (concat " is a SWI-Prolog predicate.\n\n"
                                       page)
                             " is an undocumented SWI-Prolog predicate.")
                           (if props
                               (sweeprolog--render-predicate-properties props)
                             "")))
          (if page
              (insert pred " is a SWI-Prolog predicate.\n\n" page
                      (if props
                          (sweeprolog--render-predicate-properties props)
                        ""))
            (insert pred " is not documented as a SWI-Prolog predicate.")))))))

;;;###autoload
(defun sweeprolog-describe-predicate (pred)
  "Display the full documentation for PRED (a Prolog predicate)."
  (interactive (list (sweeprolog-read-predicate
                      (concat "Describe predicate"
                              (when-let ((def (sweeprolog-identifier-at-point)))
                                (concat " (default " def ")"))
                              ": "))))
  (sweeprolog--describe-predicate pred))

(defun sweeprolog--find-predicate-from-symbol (sym)
  (sweeprolog-find-predicate (symbol-name sym)))

(add-to-list 'find-function-regexp-alist
             (cons 'swi-prolog-module
                   'sweeprolog-module-documentation-regexp))

(add-to-list 'find-function-regexp-alist
             (cons 'swi-prolog-predicate
                   'sweeprolog--find-predicate-from-symbol))


;;;; Dependency Management

(defun sweeprolog-update-dependencies ()
  "Add explicit dependencies for implicitly autoaloaded predicates."
  (interactive "" sweeprolog-mode)
  (let ((styles   nil)
        (existing nil)
        (current-directive-beg nil)
        (current-directive-end nil)
        (current-directive-file nil)
        (last-directive-end nil)
        (in-directive nil)
        (seen-non-directive nil)
        (first-structured-comment nil))
    (sweeprolog-analyze-buffer-with
     (lambda (beg end arg)
       (pcase arg
         ("directive"
          (setq in-directive t))
         (`("goal_term" "built_in" "autoload" 1)
          (when in-directive
            (add-to-list 'styles 'autoload)
            (unless seen-non-directive
              (setq last-directive-end end))))
         (`("goal_term" "built_in" "autoload" 2)
          (when in-directive
            (setq current-directive-beg beg
                  current-directive-end end)
            (unless seen-non-directive
              (setq last-directive-end end))
            (add-to-list 'styles 'autoload)))
         (`("goal_term" "built_in" "use_module" 1)
          (when in-directive
            (add-to-list 'styles 'use-module)
            (unless seen-non-directive
              (setq last-directive-end end))))
         (`("goal_term" "built_in" "use_module" 2)
          (when in-directive
            (setq current-directive-beg beg
                  current-directive-end end)
            (unless seen-non-directive
              (setq last-directive-end end))
            (add-to-list 'styles 'use-module)))
         (`("goal_term" "built_in" "module" 2)
          (when in-directive
            (unless seen-non-directive
              (setq last-directive-end end))))
         (`("goal_term" . ,_)
          (setq in-directive nil))
         ((or "term"
              "clause"
              "grammar_rule"
              "method")
          (setq in-directive nil
                seen-non-directive beg))
         (`("comment" . "structured")
          (unless (or first-structured-comment
                      (< seen-non-directive beg))
            (setq first-structured-comment beg)))
         ((or `("file"           . ,file)
              `("file_no_depend" . ,file))
          (when (and current-directive-beg
                     (<= current-directive-beg
                         beg end
                         current-directive-end))
            (setq current-directive-file file)))
         ((or "list" "empty_list")
          (when (and current-directive-beg
                     (<= current-directive-beg
                         beg end
                         current-directive-end))
            (push
             (cons current-directive-file (copy-marker (1- end) t))
             existing))))))
    (setq last-directive-end
          (copy-marker (or (and last-directive-end
                                (save-excursion
                                  (goto-char last-directive-end)
                                  (sweeprolog-end-of-top-term)
                                  (point)))
                           first-structured-comment
                           (point-min))))
    (if-let ((missing
              (sweeprolog--query-once "sweep" "sweep_file_missing_dependencies"
                                      (buffer-file-name))))
        (progn
          (dolist (autoloaded missing)
            (let* ((file    (nth 0 autoloaded))
                   (pred    (nth 1 autoloaded))
                   (kind    (pcase sweeprolog-dependency-directive
                              ('use-module "use_module")
                              ('infer (if (equal styles '(use-module))
                                          "use_module"
                                        (nth 2 autoloaded)))
                              (_ (nth 2 autoloaded)))))
              (if (not pred)
                  (save-mark-and-excursion
                    (goto-char last-directive-end)
                    (insert-before-markers
                     ":- " kind "("
                     (sweeprolog--query-once "sweep"
                                             "sweep_file_path_in_library"
                                             file)
                     ").\n")
                    (indent-region-line-by-line (save-excursion
                                                  (sweeprolog-beginning-of-top-term)
                                                  (point))
                                                (point)))
                (if-let ((marker (cdr (assoc-string file existing))))
                    (save-mark-and-excursion
                      (goto-char marker)
                      (let ((close-line (line-number-at-pos (point)))
                            (previous-line nil))
                        (pcase (sweeprolog-last-token-boundaries)
                          (`(open ,_ ,oend)
                           (goto-char oend)
                           (insert pred))
                          (`(symbol ,_ ,oend)
                           (let ((import-list-end-marker
                                  (copy-marker (point))))
                             (goto-char oend)
                             (setq previous-line (line-number-at-pos (point)))
                             (delete-horizontal-space)
                             (insert-before-markers (if (eolp)
                                                        ","
                                                      ", "))
                             (goto-char import-list-end-marker)
                             (insert pred)
                             (unless (= previous-line close-line)
                               (insert "\n"))
                             (when (< fill-column (current-column))
                               (goto-char import-list-end-marker)
                               (insert "\n"))))
                          (tok
                           (user-error "Unexpected token %s while looking for import list"
                                       tok))))
                      (indent-region-line-by-line (save-excursion
                                                    (sweeprolog-beginning-of-top-term)
                                                    (point))
                                                  (save-excursion
                                                    (sweeprolog-end-of-top-term)
                                                    (point))))
                  (save-mark-and-excursion
                    (goto-char last-directive-end)
                    (insert-before-markers
                     ":- " kind "("
                     (sweeprolog--query-once "sweep"
                                             "sweep_file_path_in_library"
                                             file)
                     ", [" pred "]).\n")
                    (indent-region-line-by-line (save-excursion
                                                  (sweeprolog-beginning-of-top-term)
                                                  (point))
                                                (point))
                    (push (cons file (copy-marker (- (point) 4) t)) existing))))))
          (sweeprolog-analyze-buffer t))
      (message "No implicit autoloads found."))))


;;;; Minor mode for moving to the next hole with TAB

(defun sweeprolog-indent-or-forward-hole (&optional arg)
  "Indent the current line or region, or go to the next hole.

If the region is active, indent it by calling `indent-region'.
Otherwise, indent the current line or, if already indented, move
to the ARGth next hole in the buffer."
  (interactive "p" sweeprolog-mode)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (let ((point (point)))
      (sweeprolog-indent-line)
      (when (= point (point))
        (sweeprolog-forward-hole arg)))))

;;;###autoload
(define-minor-mode sweeprolog-forward-hole-on-tab-mode
  "Make TAB do the Right Thing in `sweeprolog-mode'.

When enabled, this minor mode binds TAB to the command
`sweeprolog-indent-or-forward-hole', which moves to the next hole
in the buffer when the called in a line that's already indented
properly."
  :group 'sweeprolog)


;;;; Command line argument handling

(defun sweeprolog-command-line-function ()
  (when (string= argi "--swipl-args")
    (let ((current-arg nil)
          (swipl-args nil)
          (go t))
      (while (and go command-line-args-left)
        (setq current-arg (car command-line-args-left))
        (setq command-line-args-left (cdr command-line-args-left))
        (if (string= current-arg ";")
            (setq go nil)
          (push current-arg swipl-args)))
      (setq sweeprolog--extra-init-args (reverse swipl-args)))))

;;;###autoload
(defun sweeprolog-handle-command-line-args ()
  "Add flag `--swipl-args' to Emacs's command line handling."
  (add-to-list 'command-line-functions
               #'sweeprolog-command-line-function))


;;;; Term Search

(defvar sweeprolog-term-search-last-search nil
  "Last term searched with `sweeprolog-term-search'.")

(defvar-local sweeprolog-term-search-overlays nil
  "List of `sweeprolog-term-search' overlays in the current buffer.")

(defvar-local sweeprolog-term-search-repeat-count 0)

(defun sweeprolog-term-search-delete-overlays ()
  "Delete overlays created by `sweeprolog-term-search'."
  (interactive "" sweeprolog-mode)
  (mapc #'delete-overlay sweeprolog-term-search-overlays)
  (setq sweeprolog-term-search-overlays nil))

(defun sweeprolog-term-search-repeat-forward ()
  "Repeat last `sweeprolog-term-search' searching forward from point."
  (interactive "" sweeprolog-mode)
  (setq sweeprolog-term-search-repeat-count
        (mod (1+ sweeprolog-term-search-repeat-count)
             (length sweeprolog-term-search-overlays)))
  (sweeprolog-term-search (car sweeprolog-term-search-last-search)
                          (cdr sweeprolog-term-search-last-search)))

(defun sweeprolog-term-search-repeat-backward ()
  "Repeat last `sweeprolog-term-search' searching backward from point."
  (interactive "" sweeprolog-mode)
  (setq sweeprolog-term-search-repeat-count
        (mod (1- sweeprolog-term-search-repeat-count)
             (length sweeprolog-term-search-overlays)))
  (sweeprolog-term-search (car sweeprolog-term-search-last-search)
                          (cdr sweeprolog-term-search-last-search) t))

(defun sweeprolog-term-search-abort ()
  "Abort term search and restore point to its original position."
  (interactive "" sweeprolog-mode)
  (goto-char (mark t))
  (pop-mark)
  (sweeprolog-term-search-delete-overlays)
  (signal 'quit nil))

(defvar sweeprolog-term-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'sweeprolog-term-search-abort)
    (define-key map (kbd "C-m") #'sweeprolog-term-search-delete-overlays)
    (define-key map (kbd "C-r") #'sweeprolog-term-search-repeat-backward)
    (define-key map (kbd "C-s") #'sweeprolog-term-search-repeat-forward)
    map)
  "Transient keymap activated after `sweeprolog-term-search'.")

(defun sweeprolog-term-search-in-buffer (term &optional goal buffer)
  "Search for Prolog term TERM satisfying GOAL in buffer BUFFER.

Return a list of (BEG . END) cons cells where BEG is the buffer
position of the beginning of a matching term and END is its
corresponding end position."
  (setq goal   (or goal "true"))
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((offset (point-min)))
      (mapcar (lambda (match)
                (cons (+ offset (car match))
                      (+ offset (cdr match))))
              (sweeprolog--query-once "sweep" "sweep_term_search"
                                      (list buffer-file-name
                                            term goal))))))

(defun sweeprolog-read-term-try ()
  "Try to read a Prolog term in the minibuffer.

Exit the minibuffer if successful, else report the error to the
user and move point to the location of the error.  If point is
not already at the location of the error, push a mark before
moving point."
  (interactive)
  (unless (> (minibuffer-depth) 0)
    (error "Minibuffer must be active"))
  (if-let* ((contents (minibuffer-contents))
            (error-point
             (condition-case error
                 (progn
                   (sweeprolog--query-once "system" "term_string"
                                           contents t)
                   nil)
               (prolog-exception (pcase error
                                   (`(prolog-exception
                                      compound "error"
                                      (compound "syntax_error" ,_)
                                      (compound ,_ ,_ ,point))
                                    (+ (length (minibuffer-prompt))
                                       point 1)))))))
      (progn
        (unless (= (point) error-point)
          (push-mark))
        (goto-char error-point)
        (minibuffer-message "Invalid Prolog term"))
    (exit-minibuffer)))

(defvar sweeprolog-read-term-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-m") #'sweeprolog-read-term-try)
    (define-key map (kbd "C-j") #'sweeprolog-read-term-try)
    map)
  "Keymap used by `sweeprolog-read-term'.")

(defvar sweeprolog-read-goal-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map sweeprolog-read-term-map)
    (define-key map (kbd "C-i") #'completion-at-point)
    map)
  "Keymap used by `sweeprolog-goal-term'.")

(defun sweeprolog-terms-at-point (&optional point)
  "Return boundarines of Prolog terms at POINT, innermost first."
  (setq point (or point (point)))
  (save-excursion
    (goto-char point)
    (unless (sweeprolog-at-beginning-of-top-term-p)
      (sweeprolog-beginning-of-top-term))
    (unless (bobp)
      (forward-char -1))
    (let ((start (point)))
      (sweeprolog-end-of-top-term)
      (mapcar (lambda (beg-end)
                (buffer-substring-no-properties (car beg-end)
                                                (cdr beg-end)))
              (reverse
               (sweeprolog--query-once "sweep" "sweep_terms_at_point"
                                       (list
                                        (buffer-substring-no-properties
                                         start (point))
                                        start
                                        (- point start))))))))

(defun sweeprolog-goals-at-point (&optional point)
  (when (derived-mode-p 'sweeprolog-mode 'sweeprolog-top-level-mode)
    (setq point (or point (point)))
    (save-excursion
      (goto-char point)
      (mapcar (lambda (beg-end)
                (buffer-substring-no-properties (car beg-end)
                                                (cdr beg-end)))
              (let ((goals-at-point nil))
                (sweeprolog-analyze-term-at-point
                 (lambda (beg end arg)
                   (when (<= beg point end)
                     (pcase arg
                       (`("goal_term" ,_ ,_ ,_)
                        (push (cons beg end) goals-at-point))))))
                goals-at-point)))))

(defvar sweeprolog-read-term-history nil
  "History list for `sweeprolog-read-term'.")

(defvar sweeprolog-read-goal-history nil
  "History list for `sweeprolog-read-goal'.")

(defun sweeprolog-read-term (&optional prompt)
  "Read a Prolog term prompting with PROMPT (default \"?- \")."
  (setq prompt (or prompt "?- "))
  (read-from-minibuffer prompt nil
                        sweeprolog-read-term-map nil
                        'sweeprolog-read-term-history
                        (when (derived-mode-p 'sweeprolog-mode)
                          (sweeprolog-terms-at-point))))

(defun sweeprolog-read-goal (&optional prompt)
  "Read a Prolog goal prompting with PROMPT (default \"?- \")."
  (setq prompt (or prompt "?- "))
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table sweeprolog-mode-syntax-table)
        (dolist (capf sweeprolog-completion-at-point-functions)
          (add-hook 'completion-at-point-functions capf nil t)))
    (read-from-minibuffer prompt nil
                          sweeprolog-read-goal-map nil
                          'sweeprolog-read-goal-history
                          (when (derived-mode-p 'sweeprolog-mode)
                            (sweeprolog-goals-at-point)))))

(defun sweeprolog-term-search-next (point overlays backward)
  "Return first overlay in OVERLAYS starting after POINT.
If no overlay starts after POINT, return the first overlay.

If BACKWARD is non-nil, return last overlay ending before POINT
instead, or the last overlay if no overlay ends before POINT."
  (if backward
      (let* ((match nil)
             (reversed (reverse overlays))
             (first (car reversed)))
        (while (and reversed (not match))
          (let ((head (car reversed))
                (tail (cdr reversed)))
            (if (< (overlay-start head) point)
                (setq match head)
              (setq reversed tail))))
        (or match first))
    (let ((match nil)
          (first (car overlays)))
      (while (and overlays (not match))
        (let ((head (car overlays))
              (tail (cdr overlays)))
          (if (< point (overlay-start head))
              (setq match head)
            (setq overlays tail))))
      (or match first))))

(defun sweeprolog-term-search (term &optional goal backward interactive)
  "Search forward for Prolog term TERM in the current buffer.

Optional argument GOAL is a goal that matching terms must
satisfy, it may refer to variables occuring in TERM.

If BACKWARD is non-nil, search backward instead.

If INTERACTIVE is non-nil, as it is when called interactively,
push the current position to the mark ring before moving point.

When called interactively with a prefix argument, prompt for
GOAL."
  (interactive (let* ((term (sweeprolog-read-term "[Term-search] ?- "))
                      (goal (if current-prefix-arg
                                (sweeprolog-read-goal
                                 (concat "[Term-search goal for "
                                         term
                                         "] ?- "))
                              "true")))
                 (list term goal nil t))
               sweeprolog-mode)
  (when interactive
    (setq sweeprolog-term-search-repeat-count 0))
  (sweeprolog-term-search-delete-overlays)
  (setq sweeprolog-term-search-last-search (cons term goal))
  (let ((matches (sweeprolog-term-search-in-buffer term goal)))
    (if (not matches)
        (message "No matching term found.")
      (setq sweeprolog-term-search-overlays
            (mapcar (lambda (match)
                      (let* ((beg (car match))
                             (end (cdr match))
                             (overlay (make-overlay beg end)))
                        (overlay-put overlay 'face 'lazy-highlight)
                        (overlay-put overlay 'evaporate t)
                        overlay))
                    matches))
      (let ((next
             (sweeprolog-term-search-next
              (point) sweeprolog-term-search-overlays backward)))
        (overlay-put next 'face 'isearch)
        (when interactive
          (push-mark (point) t))
        (goto-char (overlay-start next)))
      (set-transient-map sweeprolog-term-search-map t
                         #'sweeprolog-term-search-delete-overlays)
      (message
       (substitute-command-keys
        (concat
         "Match "
         (number-to-string (1+ sweeprolog-term-search-repeat-count))
         "/"
         (number-to-string (length matches)) ".  "
         "\\<sweeprolog-term-search-map>"
         "\\[sweeprolog-term-search-repeat-forward] for next match, "
         "\\[sweeprolog-term-search-repeat-backward] for previous match."))))))


;;;; Right-Click Context Menu

(defvar sweeprolog-context-menu-point-at-click nil
  "Buffer position at mouse click.")

(defvar sweeprolog-context-menu-file-at-click nil
  "Prolog file specification at mouse click.")

(defvar sweeprolog-context-menu-module-at-click nil
  "Prolog module name at mouse click.")

(defvar sweeprolog-context-menu-predicate-at-click nil
  "Prolog predicate indicator at mouse click.")

(defvar sweeprolog-context-menu-variable-at-click nil
  "Prolog variable at mouse click.")

(defvar sweeprolog-context-menu-breakpoints-at-click nil
  "Prolog breakpoints at mouse click.")

(defun sweeprolog-context-menu-find-module ()
  "Find Prolog module at mouse click."
  (interactive)
  (sweeprolog-find-module sweeprolog-context-menu-module-at-click))

(defun sweeprolog-context-menu-find-module-other-window ()
  "Find Prolog module at mouse click in another window."
  (interactive)
  (sweeprolog-find-module sweeprolog-context-menu-module-at-click t))

(defun sweeprolog-context-menu-describe-module ()
  "Describe Prolog module at mouse click."
  (interactive)
  (sweeprolog-describe-module sweeprolog-context-menu-module-at-click))

(defun sweeprolog-context-menu-find-file ()
  "Find Prolog file at mouse click."
  (interactive)
  (find-file sweeprolog-context-menu-file-at-click))

(defun sweeprolog-context-menu-find-file-other-window ()
  "Find Prolog file at mouse click in another window."
  (interactive)
  (find-file-other-window sweeprolog-context-menu-file-at-click))

(defun sweeprolog-context-menu-describe-predicate ()
  "Describe Prolog predicate at mouse click."
  (interactive)
  (sweeprolog-describe-predicate sweeprolog-context-menu-predicate-at-click))

(defun sweeprolog-context-menu-rename-variable ()
  "Rename Prolog variable at mouse click."
  (interactive)
  (sweeprolog-rename-variable sweeprolog-context-menu-variable-at-click
                              nil
                              sweeprolog-context-menu-point-at-click
                              t))

(defun sweeprolog-context-menu-increment-numbered-variables ()
  "Increment numbered variables starting with the variable at click."
  (interactive)
  (sweeprolog-increment-numbered-variables
   1
   sweeprolog-context-menu-point-at-click
   sweeprolog-context-menu-variable-at-click))

(defun sweeprolog-context-menu-decrement-numbered-variables ()
  "Decrement numbered variables starting with the variable at click."
  (interactive)
  (sweeprolog-decrement-numbered-variables
   1
   sweeprolog-context-menu-point-at-click
   sweeprolog-context-menu-variable-at-click))

(defun sweeprolog-breakpoint-context-menu-set ()
  "Set breakpoint at click."
  (interactive)
  (sweeprolog-set-breakpoint sweeprolog-context-menu-point-at-click))

(defun sweeprolog-breakpoint-context-menu-delete ()
  "Delete breakpoints at click."
  (interactive)
  (dolist (id sweeprolog-context-menu-breakpoints-at-click)
    (sweeprolog-delete-breakpoint id))
  (let ((n (length sweeprolog-context-menu-breakpoints-at-click)))
   (message "Deleted %d %s" n
            (ngettext "breakpoint" "breakpoints" n))))

(defun sweeprolog-breakpoint-context-menu-set-condition ()
  "Set condition goal for the breakpoint at click."
  (interactive)
  (let ((id (car sweeprolog-context-menu-breakpoints-at-click))
        (cond (sweeprolog-read-breakpoint-condition)))
    (sweeprolog-set-breakpoint-condition id cond)))

(defun sweeprolog-context-menu-for-predicate (menu tok _beg _end _point)
  "Extend MENU with predicate-related commands if TOK describes one."
  (pcase tok
    ((or `("head" ,_ ,f ,a)
         `("goal" ,_ ,f ,a))
     (let ((pred (sweeprolog--query-once "sweep" "sweep_functor_arity_pi"
                                         (append (list f a)
                                                 (buffer-file-name)))))
       (setq sweeprolog-context-menu-predicate-at-click pred)
       (define-key menu [sweeprolog-describe-predicate]
                   `(menu-item "Describe This Predicate"
                               sweeprolog-context-menu-describe-predicate
                               :help ,(format "Describe predicate %s" pred)
                               :keys "\\[sweeprolog-describe-predicate]"))))))

(defun sweeprolog-context-menu-for-module (menu tok _beg _end _point)
  "Extend MENU with module-related commands if TOK describes one."
  (pcase tok
    (`("module" . ,module)
     (setq sweeprolog-context-menu-module-at-click module)
     (define-key menu [sweeprolog-predicate-module]
                 `(menu-item "Describe This Module"
                             sweeprolog-context-menu-describe-module
                             :help ,(format "Describe module %s" module)
                             :keys "\\[sweeprolog-describe-module]"))
     (define-key menu [sweeprolog-find-module-other-window]
                 `(menu-item "Find in Other Window"
                             sweeprolog-context-menu-find-module-other-window
                             :help ,(format "Find module %s in other window" module)
                             :keys "\\[universal-argument] \\[sweeprolog-find-module]"))
     (define-key menu [sweeprolog-find-module]
                 `(menu-item "Find This Module"
                             sweeprolog-context-menu-find-module
                             :help ,(format "Find module %s" module)
                             :keys "\\[sweeprolog-find-module]")))))

(defun sweeprolog-context-menu-for-file (menu tok _beg _end _point)
  "Extend MENU with file-related commands if TOK specifies one."
  (pcase tok
    ((or `("file"           . ,file)
         `("file_no_depend" . ,file))
     (setq sweeprolog-context-menu-file-at-click file)
     (define-key menu [sweeprolog-find-file-other-window]
                 `(menu-item "Find in Other Window"
                             sweeprolog-context-menu-find-file-other-window
                             :help ,(format "Find %s in other window" file)
                             :keys "\\[universal-argument] \\[sweeprolog-find-file-at-point]"))
     (define-key menu [sweeprolog-find-file]
                 `(menu-item "Find This File"
                             sweeprolog-context-menu-find-file
                             :help ,(format "Find %s" file)
                             :keys "\\[sweeprolog-find-file-at-point]")))))

(defun sweeprolog-context-menu-for-variable (menu tok beg end point)
  "Extend MENU with variable-related commands if TOK specifies one.
BEG and END are the variable's beginning and end positions, and
POINT is the buffer position of the mouse click."
  (pcase tok
    ((or "var"
         "singleton"
         `("goal_term" "meta" variable 0))
     (setq sweeprolog-context-menu-point-at-click point
           sweeprolog-context-menu-variable-at-click
           (buffer-substring-no-properties beg end))
     (when-let ((var-num
                 (sweeprolog--decode-numbered-variable-name
                  sweeprolog-context-menu-variable-at-click)))
       (define-key menu [sweeprolog-decrement-numbered-variables]
                   `(menu-item "Decrement Variable Numbers"
                               sweeprolog-context-menu-decrement-numbered-variables
                               :help ,(concat "Decrement variable numbers starting from "
                                              (sweeprolog--format-variable
                                               sweeprolog-context-menu-variable-at-click))
                               :keys "\\[sweeprolog-decrement-numbered-variables]"))
       (define-key menu [sweeprolog-increment-numbered-variables]
                   `(menu-item "Increment Variable Numbers"
                               sweeprolog-context-menu-increment-numbered-variables
                               :help ,(concat "Increment variable numbers starting from "
                                              (sweeprolog--format-variable
                                               sweeprolog-context-menu-variable-at-click))
                               :keys "\\[sweeprolog-increment-numbered-variables]")))
     (define-key menu [sweeprolog-rename-variable]
                 `(menu-item "Rename Variable"
                             sweeprolog-context-menu-rename-variable
                             :help ,(concat "Rename variable "
                                            (sweeprolog--format-variable
                                             sweeprolog-context-menu-variable-at-click))
                             :keys "\\[sweeprolog-rename-variable]")))))

(defun sweeprolog-context-menu-for-clause (menu tok _beg _end point)
  "Extend MENU with clause-related commands if TOK specifies one.
POINT is the buffer position of the mouse click."
  (pcase tok
    ((or "clause"
         "grammar_rule")
     (when-let ((file (buffer-file-name))
                (submenu (make-sparse-keymap (propertize "Breakpoint"))))
       (if-let ((bps-at-point
                 (sweeprolog-breakpoints-at-point file point))
                (ids (mapcar (lambda (bp)
                               (alist-get "id" bp nil nil #'string=))
                             bps-at-point)))
           (progn
             (setq sweeprolog-context-menu-breakpoints-at-click ids)
             (define-key-after submenu [sweeprolog-delete-breakpoint]
               `(menu-item "Delete"
                           sweeprolog-breakpoint-context-menu-delete
                           :help "Delete this breakpoint"
                           :keys "\\[negative-argument] \\[sweeprolog-set-breakpoint]"))
             (define-key-after submenu [sweeprolog-set-breakpoint-condition]
               `(menu-item "Set condition"
                           sweeprolog-breakpoint-context-menu-set-condition
                           :help "Set condition goal for this breakpoint"
                           :keys "\\[universal-argument] \\[sweeprolog-set-breakpoint]")))
         (setq sweeprolog-context-menu-point-at-click point)
         (define-key-after submenu [sweeprolog-set-breakpoint]
           `(menu-item "Set"
                       sweeprolog-breakpoint-context-menu-set
                       :help "Set breakpoint"
                       :keys "\\[sweeprolog-set-breakpoint]")))
       (define-key-after menu [sweeprolog-breakpoint]
         `(menu-item "Breakpoint" ,submenu))))))

(defvar sweeprolog-context-menu-functions
  '(sweeprolog-context-menu-for-clause
    sweeprolog-context-menu-for-file
    sweeprolog-context-menu-for-module
    sweeprolog-context-menu-for-predicate
    sweeprolog-context-menu-for-variable)
  "Functions that create context menu entries for Prolog tokens.
Each function receives as its arguments the menu, the Prolog
token's description, its start position, its end position, and
the position for which the menu is created.")

(defun sweeprolog-context-menu-function (menu click)
  "Populate MENU with Prolog commands at CLICK."
  (let ((point (posn-point (event-start click))))
    (save-mark-and-excursion
      (goto-char point)
      (sweeprolog-analyze-term-at-point
       (lambda (beg end tok)
         (when (<= beg point end)
           (run-hook-with-args 'sweeprolog-context-menu-functions
                               menu tok beg end point))))))
  menu)



;;;; Async Prolog Queries

(defvar-local sweeprolog-async-goal-thread-id nil
  "Prolog thread running the async goal of the current buffer.")

(defvar-local sweeprolog-async-goal-current-goal nil
  "Prolog async goal of the current buffer.")

(defun sweeprolog-async-goal-interrupt (proc &optional _group)
  "Interrupt async Prolog goal associated with process PROC."
  (with-current-buffer (process-buffer proc)
    (sweeprolog--query-once "sweep" "sweep_interrupt_async_goal"
                            sweeprolog-async-goal-thread-id)))

(defun sweeprolog-async-goal-filter (proc string)
  "Process filter function for async Prolog queries.

Deletes PROC if STRING contains an end of output marker string."
  (internal-default-process-filter proc string)
  (when (string-match (rx "Sweep async goal finished")
                      string)
    (sit-for 1)
    (delete-process proc)))

(defun sweeprolog-async-goal-start (goal &optional buffer)
  "Start async Prolog goal GOAL and direct its output to BUFFER."
  (setq buffer (or buffer (current-buffer)))
  (sweeprolog-ensure-initialized)
  (if (fboundp 'sweeprolog-open-channel)
      (let* ((proc (make-pipe-process
                    :name (concat "?- " goal)
                    :buffer buffer
                    :filter #'sweeprolog-async-goal-filter))
             (fd (sweeprolog-open-channel proc)))
        (sweeprolog--query-once "sweep" "sweep_async_goal"
                                (cons goal fd)))
    (error "Async queries require Emacs 28 and SWI-Prolog 9.1.4 or later")))

(defun sweeprolog-async-goal-restart ()
  "Restart async Prolog goal in the current buffer."
  (interactive "" sweeprolog-async-goal-output-mode)
  (when-let ((proc (get-buffer-process (current-buffer))))
    (if (process-live-p proc)
        (if (yes-or-no-p "A goal is running; kill it? ")
            (condition-case ()
                (progn
                  (interrupt-process proc)
                  (sit-for 1)
                  (delete-process proc))
              (error nil))
          (error "Cannot have two processes in `%s' at once"
                 (buffer-name)))))
  (setq sweeprolog-async-goal-thread-id
        (sweeprolog-async-goal-start
         sweeprolog-async-goal-current-goal)))

(defvar sweeprolog-async-goal-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    (define-key map (kbd "g") #'sweeprolog-async-goal-restart)
    map)
  "Keymap used by `sweeprolog-async-goal-output-mode'.")

(define-compilation-mode sweeprolog-async-goal-output-mode
  "Sweep Async Output"
  "Major mode for viewing the output of async Prolog queries."
  (add-hook 'interrupt-process-functions
            #'sweeprolog-async-goal-interrupt nil t)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when-let ((proc (get-buffer-process (current-buffer))))
                (when (and (process-live-p proc)
                           sweeprolog-async-goal-thread-id)
                  (condition-case _
                      (sweeprolog--query-once "sweep" "sweep_interrupt_async_goal"
                                              sweeprolog-async-goal-thread-id)
                    (prolog-exception nil)))))
            nil t))

;;;###autoload
(defun sweeprolog-async-goal (goal)
  "Execute GOAL and display its output in a buffer asynchronously."
  (interactive (list (sweeprolog-read-goal "[async] ?- ")))
  (let* ((buffer-name (generate-new-buffer-name
                       (format "*Async Output for %s*" goal)))
         (buffer (get-buffer-create buffer-name))
         (tid (sweeprolog-async-goal-start goal buffer)))
    (with-current-buffer buffer
      (sweeprolog-async-goal-output-mode)
      (setq sweeprolog-async-goal-thread-id     tid
            sweeprolog-async-goal-current-goal goal))
    (display-buffer buffer)))


;;;; Refactoring

(defun sweeprolog--variables-at-point (point)
  "Return information about variables in the Prolog term at POINT.

Returns a cons cell (VAR-OCCURRENCES . VAR-AT-POINT).
VAR-OCCURRENCES is an alist of elements (VAR . OCCURRENCES) where
VAR is a variable name and OCCURRENCES is itself an alist of
elements (BEG . END) describing the beginning and end of
occurrences of this variable in buffer positions.  VAR-AT-POINT
is the name of the variable at point, if any."
  (let ((vars nil)
        (var-at-point nil))
    (sweeprolog-analyze-term-at-point
     (lambda (beg end arg)
       (pcase arg
         ((or "var"
              "singleton"
              `("goal_term" "meta" variable 0))
          (let ((var (buffer-substring-no-properties beg end)))
            (push (cons beg end)
                  (alist-get var vars nil nil #'string=))
            (when (<= beg point end)
              (setq var-at-point var))))))
     point)
    (cons vars var-at-point)))

(defun sweeprolog--format-variable (var)
  (propertize var 'face (sweeprolog-variable-face)))

(defvar sweeprolog-read-new-variable--existing-vars nil)
(defvar-local sweeprolog-read-new-variable--warned nil)

(defun sweeprolog--variable-name-p (string)
  "Return t if STRING is valid Prolog variable name."
  (save-match-data
    (let ((case-fold-search nil))
      (not
       (not
        (string-match (rx bos (or "_" upper) (* alnum) eos) string))))))

(defun sweeprolog--decode-numbered-variable-name (string)
  "Return t if STRING is valid number variable name."
  (save-match-data
    (let ((case-fold-search nil))
      (when (string-match (rx bos (group-n 1 (or "_" upper) (or (seq (* alnum) letter)
                                                                ""))
                              (group-n 2 (or "0" (seq (any (?1 . ?9)) (* digit)))) eos)
                          string)
        (cons (match-string 1 string)
              (string-to-number (match-string 2 string)))))))

(defun sweeprolog-read-new-variable-try ()
  "Try to exit the minibuffer with a new Prolog variable name.

If the minibuffer contains a variable that already occurs in the
current clause, warn and ask for confirmation before exiting.  If
the minibuffer does not contain a valid variable name, report it
and refuse to exit."
  (interactive)
  (unless (> (minibuffer-depth) 0)
    (error "Minibuffer must be active"))
  (let ((choice (minibuffer-contents)))
    (if (sweeprolog--variable-name-p choice)
        (if (member choice sweeprolog-read-new-variable--existing-vars)
            (pcase sweeprolog-rename-variable-allow-existing
              ('confirm (if (string= sweeprolog-read-new-variable--warned choice)
                            (exit-minibuffer)
                          (minibuffer-message
                           (substitute-command-keys
                            "%s already exists, type \\[sweeprolog-read-new-variable-try] again to confirm")
                           (sweeprolog--format-variable choice))
                          (setq sweeprolog-read-new-variable--warned choice)))
              ('nil (minibuffer-message "%s already exists" (sweeprolog--format-variable choice)))
              (_ (exit-minibuffer)))
          (exit-minibuffer))
      (minibuffer-message "Invalid Prolog variable name"))))

(put 'sweeprolog-read-new-variable-try :advertised-binding [?\C-m])

(defvar sweeprolog-read-new-variable-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-m") #'sweeprolog-read-new-variable-try)
    (define-key map (kbd "C-j") #'sweeprolog-read-new-variable-try)
    map)
  "Keymap used by `sweeprolog-read-new-variable'.")

(defun sweeprolog-read-new-variable (prompt existing-vars)
  "Prompt with PROMPT for a new Prolog variable.
EXISTING-VARS is a list of existing variable names (strings)."
  (let ((sweeprolog-read-new-variable--existing-vars existing-vars))
    (read-from-minibuffer prompt nil sweeprolog-read-new-variable-map)))

(defun sweeprolog-read-existing-variable (occurrences &optional default prompt)
  (let* ((max-var-len (apply #'max
                             (mapcar #'length
                                     (mapcar #'car
                                             occurrences))))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (let ((n (length (alist-get key
                                              occurrences
                                              nil nil
                                              #'string=))))
                    (concat (make-string (- max-var-len
                                            (length key))
                                         ? )
                            (format " %d %s" n
                                    (ngettext "occurrence"
                                              "occurrences"
                                              n))))))))
    (completing-read
     (concat
      (or prompt "Rename variable")
      (when default
        (concat " (default " (sweeprolog--format-variable default) ")"))
      ": ")
     occurrences nil t nil nil default)))

(defun sweeprolog-rename-variable (&optional old new point verbose)
  "Rename the variable OLD to NEW in the Prolog term at POINT.

If OLD is nil, prompt for it in the minibuffer with completion.
If NEW is nil, prompt for it as well.  If POINT is nil, it
defaults to the current point.  If VERBOSE is non-nil, also print
a message with the number of replaced occurrences of OLD.

Interactively, OLD, NEW and POINT are nil, and VERBOSE is t."
  (interactive (list nil nil nil t) sweeprolog-mode)
  (setq point (or point (point)))
  (let* ((term-var-occurrences (sweeprolog--variables-at-point point))
         (var-occurrences (car term-var-occurrences))
         (var-at-point (cdr term-var-occurrences)))
    (unless var-occurrences
      (user-error "Term does not contain variables!"))
    (let* ((old-name
            (or old
                (sweeprolog-read-existing-variable var-occurrences
                                                   var-at-point)))
           (old-formatted (sweeprolog--format-variable old-name))
           (existing-vars (mapcar #'car var-occurrences))
           (new-name
            (or new
                (sweeprolog-read-new-variable
                 (concat "Rename "
                         old-formatted
                         " to: ")
                 existing-vars)))
           (old-occurrences
            (alist-get old-name var-occurrences nil nil #'string=))
           (num (length old-occurrences)))
      (save-excursion
        (combine-after-change-calls
          (dolist (old-occurrence old-occurrences)
            (let ((occurrence-beg (car old-occurrence))
                  (occurrence-end (cdr old-occurrence)))
              (delete-region occurrence-beg occurrence-end)
              (goto-char occurrence-beg)
              (insert new-name)))))
      (when verbose
        (message (if (member new-name existing-vars)
                     "Merged %d %s of %s to %s."
                   "Replaced %d %s of %s to %s.")
                 num
                 (ngettext "occurrence" "occurrences" num)
                 old-formatted
                 (sweeprolog--format-variable new-name))))))

(defun sweeprolog--decode-numbered-variable (string)
  (when (string-match (rx bos
                          (group-n 1
                            (or "_" upper)
                            (* (or "_" alnum))
                            (or "_" letter))
                          (group-n 2
                            (+ digit))
                          eos)
                      string)
    (cons (match-string 1 string)
          (string-to-number (match-string 2 string)))))

(defvar sweeprolog-increment-numbered-variables-last-result nil)

(defvar sweeprolog-increment-numbered-variables-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'sweeprolog-increment-numbered-variables-more)
    (define-key map (kbd "-") #'sweeprolog-decrement-numbered-variables-more)
    map)
  "Transient keymap activated after `sweeprolog-increment-numbered-variables'.")

(defun sweeprolog-increment-numbered-variables (increment point &optional from)
  "Increment numbered variables from at POINT starting with FROM.

FROM is either nil or a numbered varialbe (a string) that occurs
in clause at POINT.  If it is nil, prompt for a numbered variable
in the clause at POINT and use that as FROM.

INCREMENT is an integer that is added to the number in each
occurrence of FROM and any numbered variable that has the same
base name and a greater number in the clause at POINT.

Interactively, POINT is point, FROM is nil, and INCREMENT is the
numeric prefix argument (1 without prefix argument)."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (point) nil)
               sweeprolog-mode)
  (let* ((term-var-occurrences (sweeprolog--variables-at-point point))
         (numbered-vars (seq-filter
                         (lambda (v)
                           (sweeprolog--decode-numbered-variable-name
                            (car v)))
                         (car term-var-occurrences)))
         (var-at-point (cdr term-var-occurrences)))
    (unless numbered-vars
      (user-error "Term does not contain numbered variables!"))
    (let* ((old-name
            (or from
                (sweeprolog-read-existing-variable
                 numbered-vars var-at-point
                 (if (< 0 increment)
                     "Increment variable"
                   "Decrement variable"))))
           (old-decoded (sweeprolog--decode-numbered-variable-name
                         old-name))
           (base (car old-decoded))
           (first (cdr old-decoded))
           (target (+ increment first)))
      (when (< increment 0)
        (let ((old-formatted (sweeprolog--format-variable old-name))
              (decrement (- increment))
              (blocker nil))
          (cond
           ((< target 0)
            (user-error "Cannot decrement %s by %d (negative variable number)"
                        old-formatted decrement))
           ((setq blocker
                  (caar (seq-filter (lambda (numbered-var)
                                      (pcase (sweeprolog--decode-numbered-variable-name
                                              (car numbered-var))
                                        (`(,nb . ,nn) (and (string= nb base)
                                                           (< nn first)
                                                           (<= target nn)))))
                                    numbered-vars)))
            (user-error "Cannot decrement %s by %d (blocked on %s)"
                        old-formatted decrement blocker)))))
      (let* ((rest
              (seq-filter
               (lambda (v)
                 (pcase (sweeprolog--decode-numbered-variable-name
                         (car v))
                   (`(,b . ,n) (and (string= b base)
                                    (<= first n)))))
               numbered-vars))
             (to-replace
              (sort
               (apply #'append
                      (mapcar
                       (lambda (v)
                         (pcase (sweeprolog--decode-numbered-variable-name
                                 (car v))
                           (`(,b . ,n)
                            (let* ((ni (+ n increment))
                                   (bn (concat b (number-to-string ni))))
                              (mapcar
                               (lambda (p)
                                 (list (car p)
                                       (cdr p)
                                       bn))
                               (cdr v))))))
                       rest))
               (lambda (l r)
                 (> (car l) (car r))))))
        (save-excursion
          (combine-after-change-calls
            (dolist (replace to-replace)
              (let ((beg (nth 0 replace)))
                (delete-region beg (nth 1 replace))
                (goto-char beg)
                (insert (nth 2 replace))))))
        (setq sweeprolog-increment-numbered-variables-last-result
              (concat base (number-to-string target)))
        (set-transient-map sweeprolog-increment-numbered-variables-map t)
        (message
         (substitute-command-keys
          (concat
           "Repeat with "
           "\\<sweeprolog-increment-numbered-variables-map>"
           "\\[sweeprolog-increment-numbered-variables-more], "
           "\\[sweeprolog-decrement-numbered-variables-more]")))))))

(defun sweeprolog-decrement-numbered-variables (decrement point &optional old)
  "Decrement numbered variables from at POINT starting with FROM.

FROM is either nil or a numbered varialbe (a string) that occurs
in clause at POINT.  If it is nil, prompt for a numbered variable
in the clause at POINT and use that as FROM.

DECREMENT is an integer that is substracted from the number in
each occurrence of FROM and any numbered variable that has the
same base name and a greater number in the clause at POINT.

Interactively, POINT is point, FROM is nil, and DECREMENT is the
numeric prefix argument (1 without prefix argument)."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (point) nil)
               sweeprolog-mode)
  (sweeprolog-increment-numbered-variables (- decrement) point old))

(defun sweeprolog-increment-numbered-variables-more ()
  "Increment the last incremented/decremented numbered variable."
  (interactive)
  (sweeprolog-increment-numbered-variables
   1 (point) sweeprolog-increment-numbered-variables-last-result))

(defun sweeprolog-decrement-numbered-variables-more ()
  "Decrement the last incremented/decremented numbered variable."
  (interactive)
  (sweeprolog-decrement-numbered-variables
   1 (point) sweeprolog-increment-numbered-variables-last-result))


;;;; Breakpoints

(defun sweeprolog-current-breakpoints ()
  "Return the list for current Prolog breakpoints.
Each breakpoint is represented as an alist with string keys."
  (sweeprolog--query-once "sweep" "sweep_current_breakpoints" nil))

(defun sweeprolog-current-breakpoints-in-region (beg end &optional buf)
  "Return breakpoints that start between BEG an END in BUF.
If BUF is nil, it defaults to the current buffer.

The return value is an alist of elements (BPBEG . BPEND) where
BPBEG is the start position of the breakpoint and BPEND is its
end.  list for current Prolog breakpoints."
  (setq buf (or buf (current-buffer)))
  (with-current-buffer buf
    (sweeprolog--query-once "sweep" "sweep_current_breakpoints_in_region"
                            (list (or (buffer-file-name)
                                      (expand-file-name (buffer-name)))
                                  beg end))))

(defun sweeprolog-read-breakpoint (&optional prompt)
  "Read a Prolog breakpoint id in the minibuffer, with completion.
If PROMPT is non-nil, use it as the minibuffer prompt, otherwise
prompt with \"Breakpoint: \"."
  (let* ((bps (sweeprolog-current-breakpoints))
         (col (mapcar (lambda (bp)
                        (let ((id (alist-get "id" bp nil nil #'string=))
                              (file (alist-get "file" bp nil nil #'string=))
                              (line (alist-get "line" bp nil nil #'string=)))
                          (cons (number-to-string id)
                                (format "%s%s:%d"
                                        (make-string (- 4 (floor (log id 10))) ? )
                                        file line))))
                      bps))
         (current-file (buffer-file-name))
         (current-line (line-number-at-pos (point)))
         (def
          (mapcar (lambda (bp)
                    (number-to-string (alist-get "id" bp nil nil #'string=)))
                  (seq-filter (lambda (bp)
                                (and (string= (expand-file-name (alist-get "file" bp nil nil #'string=))
                                              current-file)
                                     (= (alist-get "line" bp nil nil #'string=)
                                        current-line)))
                              bps)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (alist-get key col nil nil #'string=)))))
    (string-to-number
     (completing-read (concat (or prompt "Breakpoint")
                              (when def (concat " (default " (car def) ")"))
                              ": ")
                      col nil t nil nil def))))

(defun sweeprolog-read-breakpoint-condition ()
  "Read a Prolog breakpoint condition in the minibuffer."
  (sweeprolog-read-goal "[breakpoint condition] ?- "))

(defun sweeprolog-set-breakpoint-condition (id cond)
  "Attach condition goal COND to the breakpoint with id ID."
  (interactive (list (sweeprolog-read-breakpoint "Set condition for breakpoint")
                     (sweeprolog-read-breakpoint-condition)))
  (sweeprolog--query-once "sweep" "sweep_set_breakpoint_condition"
                          (cons id cond)))

(defun sweeprolog-delete-breakpoint (id)
  "Delete the breakpoint with id ID."
  (interactive (list (sweeprolog-read-breakpoint "Delete breakpoint")))
  (let* ((file (sweeprolog--query-once "sweep" "sweep_breakpoint_file" id))
         (buf (find-buffer-visiting file (lambda (b)
                                           (with-current-buffer b
                                             (and (derived-mode-p 'sweeprolog-mode)
                                                  sweeprolog-highlight-breakpoints)))))
         (range (sweeprolog--query-once "sweep" "sweep_breakpoint_range" id)))
    (sweeprolog--query-once "sweep" "sweep_delete_breakpoint" id)
    (message "Deleted breakpoint (id %d)" id)
    (when (and buf range)
      (with-current-buffer buf
        (sweeprolog-analyze-term (car range))))))

(defface sweeprolog-breakpoint-face
  '((((background light)) :background "lightgreen")
    (t                    :background "darkgreen"))
  "Face used to highlight Prolog breakpoints."
  :group 'sweeprolog-faces)

(defvar sweeprolog-breakpoint-face 'sweeprolog-breakpoint-face
  "Face to use for highlighting Prolog breakpoints.")

(defun sweeprolog--highlight-breakpoint (beg end)
  (font-lock--add-text-property beg end
                                'font-lock-face
                                sweeprolog-breakpoint-face
                                (current-buffer)
                                nil))

(defun sweeprolog-highlight-breakpoint (id)
  (when sweeprolog-highlight-breakpoints
    (when-let
        ((range
          (sweeprolog--query-once "sweep" "sweep_breakpoint_range"
                                  id)))
      (with-silent-modifications
        (sweeprolog--highlight-breakpoint (car range)
                                          (cdr range))))))

(defun sweeprolog-breakpoints-at-point (file point)
  (seq-filter (lambda (bp)
                (and (string= (expand-file-name (alist-get "file" bp nil nil #'string=))
                              file)
                     (when-let ((range (alist-get "range" bp nil nil #'string=)))
                       (<= (car range) point (cadr range)))))
              (sweeprolog-current-breakpoints)))

(defun sweeprolog-read-breakpoint-at-point (point &optional prompt)
  "Prompt with PROMPT for a breakpoint at POINT, with completion.
If there only one breakpoint at POINT, return it without prompting."
  (let* ((bps (sweeprolog-breakpoints-at-point (buffer-file-name)
                                               point))
         (col (mapcar (lambda (bp)
                        (let ((id (alist-get "id" bp nil nil #'string=))
                              (file (alist-get "file" bp nil nil #'string=))
                              (line (alist-get "line" bp nil nil #'string=)))
                          (cons (number-to-string id)
                                (format "%s%s:%d"
                                        (make-string (- 4 (floor (log id 10))) ? )
                                        file line))))
                      bps)))
    (when col
      (string-to-number
       (if (= (length col) 1)
           (caar col)
         (let ((completion-extra-properties
                (list :annotation-function
                      (lambda (key)
                        (alist-get key col nil nil #'string=)))))
           (completing-read (or prompt "Breakpoint at point: ") col nil t)))))))

(defun sweeprolog-delete-breakpoint-at-point (point)
  "Delete breakpoint at POINT."
  (interactive "d" sweeprolog-mode)
  (if-let ((id (sweeprolog-read-breakpoint-at-point
                point "Delete breakpoint at point: ")))
      (sweeprolog-delete-breakpoint id)
    (user-error "No breakpoints here!")))

(defun sweeprolog-set-breakpoint (point &optional cond delete)
  "Set breakpoint at POINT with condition COND.
If DELETE is non-nil, delete the breakpoint at POINT instead.

Interactively, POINT is point.  If called without a prefix
argument, COND and DELETE are nil.  If called with a positive
prefix argument, prompt for COND.  Otherwise, if called with a
zero or negative prefix argument, delete the breakpoint at POINT
instead."
  (interactive
   (cons (point)
         (cond ((< (prefix-numeric-value current-prefix-arg) 1)
                (list nil t))
               (current-prefix-arg
                (list (sweeprolog-read-breakpoint-condition)))))
   sweeprolog-mode)
  (if delete
      (sweeprolog-delete-breakpoint-at-point point)
    (if (or (sweeprolog-buffer-loaded-since-last-modification-p)
            (and (y-or-n-p (concat (if (sweeprolog-buffer-load-time)
                                       "Buffer modified since it was last loaded, re"
                                     "Buffer isn't loaded, ")
                                   "load before setting breakpoint?"))
                 (sweeprolog-load-buffer (current-buffer))))
        (if-let ((bp (sweeprolog--query-once "sweep" "sweep_set_breakpoint"
                                             (list (buffer-file-name)
                                                   (line-number-at-pos point)
                                                   (1- point)))))
            (progn
              (if cond
                  (if (sweeprolog-set-breakpoint-condition bp cond)
                      (message "Created conditional breakpoint (id %d)." bp)
                    (sweeprolog-delete-breakpoint bp)
                    (user-error "Failed to set breakpoint condition"))
                (message "Created breakpoint (id %d)." bp))
              (sweeprolog-highlight-breakpoint bp))
          (user-error "Failed to create breakpoint"))
      (user-error "Cannot set breakpoint in buffer without loading it"))))

(defun sweeprolog-breakpoint-menu-mode--entries ()
  (mapcar (lambda (bp)
            (let ((id (alist-get "id" bp nil nil #'string=))
                  (file (alist-get "file" bp nil nil #'string=))
                  (line (alist-get "line" bp nil nil #'string=))
                  (pred (alist-get "predicate" bp nil nil #'string=))
                  (clause (alist-get "clause" bp nil nil #'string=))
                  (cond (alist-get "condition" bp nil nil #'string=)))
              (list id (vector (number-to-string id)
                               (if file file "")
                               (if line (number-to-string line) "")
                               (propertize pred
                                           'face
                                           (sweeprolog-predicate-indicator-face))
                               (number-to-string clause)
                               (or cond "")))))
          (sweeprolog-current-breakpoints)))

(defun sweeprolog-breakpoint-menu-mode--refresh ()
  (tabulated-list-init-header)
  (setq tabulated-list-entries (sweeprolog-breakpoint-menu-mode--entries)))

(defun sweeprolog-breakpoint-menu-find (&optional other-window)
  "Go to the source position of the breakpoint at point.
If OTHER-WINDOW is non-nil, find it in another window."
  (interactive "" sweeprolog-breakpoint-menu-mode)
  (if-let ((vec (tabulated-list-get-entry)))
      (let* ((file (seq-elt vec 1))
             (line (seq-elt vec 2)))
        (if other-window
            (find-file-other-window file)
          (find-file file))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line))))
    (user-error "No breakpoint menu entry here")))

(defun sweeprolog-breakpoint-menu-find-other-window ()
  "Find the position of the breakpoint at point in another window."
  (interactive "" sweeprolog-breakpoint-menu-mode)
  (sweeprolog-breakpoint-menu-find t))

(defun sweeprolog-breakpoint-menu-delete ()
  "Delete the breakpoint at point."
  (interactive "" sweeprolog-breakpoint-menu-mode)
  (if-let ((id (car (tabulated-list-delete-entry))))
      (sweeprolog-delete-breakpoint id)
    (user-error "No breakpoint menu entry here")))

(defun sweeprolog-breakpoint-menu-set-condition (bp cond)
  "Attach condition goal COND to the breakpoint BP at point."
  (interactive (list (tabulated-list-get-id)
                     (sweeprolog-read-breakpoint-condition))
               sweeprolog-breakpoint-menu-mode)
  (if bp
      (if (sweeprolog-set-breakpoint-condition bp cond)
          (tabulated-list-revert)
        (user-error "Failed to set breakpoint condition"))
    (user-error "No breakpoint menu entry here")))

(defvar sweeprolog-breakpoint-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'sweeprolog-breakpoint-menu-find)
    (define-key map (kbd "o")   #'sweeprolog-breakpoint-menu-find-other-window)
    (define-key map (kbd "C")   #'sweeprolog-breakpoint-menu-set-condition)
    (define-key map (kbd "D")   #'sweeprolog-breakpoint-menu-delete)
    map)
  "Local keymap for `sweeprolog-breakpoint-menu-mode' buffers.")

(define-derived-mode sweeprolog-breakpoint-menu-mode
  tabulated-list-mode "Sweep Breakpoint Menu"
  "Major mode for browsing the list of current Prolog breakpoints."
  (setq tabulated-list-format [("ID"   8  t)
                               ("File" 40 t)
                               ("Line" 8  t)
                               ("Predicate" 32  t)
                               ("Clause" 8  t)
                               ("Condition" 20 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "ID" nil))
  (add-hook 'tabulated-list-revert-hook
            #'sweeprolog-breakpoint-menu-mode--refresh nil t)
  (tabulated-list-init-header))

(defun sweeprolog-list-breakpoints ()
  "Display a list of Prolog breakpoints."
  (interactive)
  (let ((buf (get-buffer-create "*Sweep Breakpoints*")))
    (with-current-buffer buf
      (sweeprolog-breakpoint-menu-mode)
      (sweeprolog-breakpoint-menu-mode--refresh)
      (tabulated-list-print))
    (pop-to-buffer buf)))


;;;; Footer

(provide 'sweeprolog)

;;; sweeprolog.el ends here
