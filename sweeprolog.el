;;; sweeprolog.el --- Embedded SWI-Prolog -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <~eshel/dev@lists.sr.ht>
;; Keywords: prolog languages extensions
;; URL: https://git.sr.ht/~eshel/sweep
;; Package-Version: 0.27.6
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Sweep is an embedding of SWI-Prolog in Emacs.  It uses the C
;; interfaces of both SWI-Prolog and Emacs Lisp to let you query
;; Prolog directly from Elisp.  On top of this tight integration,
;; Sweep provides an advanced development environment for SWI-Prolog
;; in Emacs.
;;
;; For more information, see the Sweep manual at
;; <https://eshelyaron.com/sweep.html>.  To read the manual inside
;; Emacs, do M-x sweeprolog-info-manual, or evaluate (info "(sweep)Top").

;;; Code:

(require 'compat)
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
  '(sweeprolog-maybe-extract-region-to-predicate
    sweeprolog-maybe-insert-next-clause
    sweeprolog-maybe-define-predicate)
  "List of functions that insert a Prolog term in a certain context.

See `sweeprolog-insert-term-dwim' for more details.")

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

When non-nil, the function `sweeprolog-predicate-location'
attempts to find the C definitions of SWI-Prolog native built-in
predicates.

The value of this option can be a string, in which case it should
be the name of the SWI-Prolog source code root directory.  Any
other non-nil value says to try and find the SWI-Prolog sources
among the directories that `project-known-project-roots' returns."
  :package-version '((sweeprolog . "0.7.1"))
  :type '(choice (const     :tag "Detect"  t)
                 (directory :tag "Manual")
                 (const     :tag "Disable" nil)))

(defcustom sweeprolog-module-header-comment-skeleton ?\n
  "Additional content for the topmost comment in module headers.

The SWI-Prolog module header inserted by \\[auto-insert] includes
a multiline comment at the very start of the buffer which
contains the name and mail address of the author based on the
user options `user-full-name' and `user-mail-address'
respectively, followed by the value of this variable, which is
interpreted as a skeleton (see `skeleton-insert').  In its
simplest form, this may be a string or a character.

This user option may be useful, for example, to include copyright
notices with the module header."
  :package-version '((sweeprolog . "0.4.6"))
  :type 'sexp)

(defcustom sweeprolog-indent-offset 4
  "Number of columns to indent with in `sweeprolog-mode' buffers."
  :package-version '((sweeprolog . "0.3.1"))
  :type 'natnum)

(defcustom sweeprolog-qq-mode-alist '(("graphql"    . graphql-mode)
                                      ("javascript" . js-mode)
                                      ("html"       . html-mode))
  "Association between Prolog quasi-quotation types and Emacs modes.

This is a list of pairs of the form (TYPE . MODE), where TYPE is
a Prolog quasi-quotation type given as a string, and MODE is a
symbol specifing a major mode."
  :package-version '((sweeprolog . "0.4.3"))
  :type '(alist :key-type string :value-type function))

(defcustom sweeprolog-enable-cycle-spacing t
  "If non-nil and `cycle-spacing-actions' is defined, extend it.

This makes the first invocation of \\[cycle-spacing] in
`sweeprolog-mode' buffers update whitespace around point using
`sweeprolog-align-spaces', which see."
  :package-version '((sweeprolog . "0.5.3"))
  :type 'boolean)

(defcustom sweeprolog-analyze-buffer-on-idle t
  "If non-nil, analyze `sweeprolog-mode' buffers on idle."
  :package-version '((sweeprolog . "0.8.2"))
  :type 'boolean)

(defcustom sweeprolog-analyze-buffer-max-size 100000
  "Maximum buffer size to analyze on idle."
  :package-version '((sweeprolog . "0.8.2"))
  :type 'natnum)

(defcustom sweeprolog-analyze-buffer-min-interval 1.5
  "Minimum idle time to wait before analyzing the buffer."
  :package-version '((sweeprolog . "0.8.2"))
  :type 'float)

(defcustom sweeprolog-swipl-path nil
  "File name of the swipl executable.
When non-nil, this is used by the embedded SWI-Prolog runtime to
locate its \"home\" directory.  Otherwise, Sweep uses
`executable-find' to find the swipl executable."
  :package-version '((sweeprolog . "0.1.1"))
  :type '(choice (file  :tag "File name" :must-match t)
                 (const :tag "Search `executable-path'" nil)))

(defcustom sweeprolog-messages-buffer-name "*Sweep Messages*"
  "The name of the buffer to use for logging Prolog messages."
  :package-version '((sweeprolog . "0.23.1"))
  :type 'string)

(defcustom sweeprolog-read-flag-prompt "Flag: "
  "Prompt used for reading a Prolog flag name from the minibuffer."
  :package-version '((sweeprolog . "0.1.2"))
  :type 'string)

(defcustom sweeprolog-read-module-prompt "Module: "
  "Prompt used for reading a Prolog module name from the minibuffer."
  :package-version '((sweeprolog . "0.1.0"))
  :type 'string)

(defcustom sweeprolog-read-predicate-prompt "Predicate"
  "Prompt used for reading a Prolog predicate name from the minibuffer."
  :package-version '((sweeprolog . "0.19.1"))
  :type 'string)

(defcustom sweeprolog-read-exportable-predicate-prompt "Export predicate: "
  "Prompt used for reading an exportable predicate name."
  :package-version '((sweeprolog . "0.6.2"))
  :type 'string)

(defcustom sweeprolog-read-pack-prompt "Pack: "
  "Prompt used for reading a Prolog pack name from the minibuffer."
  :package-version '((sweeprolog . "0.1.0"))
  :type 'string)

(defcustom sweeprolog-top-level-display-action nil
  "Display action used for displaying the `sweeprolog-top-level' buffer."
  :package-version '((sweeprolog . "0.1.0"))
  :type '(choice (const :tag "Default" nil)
                 (cons  :tag "Buffer display action"
                        (choice (function :tag "Display Function")
                                (repeat :tag "Display Functions" function))
                        (alist :tag "Action alist"))))

(defcustom sweeprolog-top-level-min-history-length 3
  "Minimum input length to record in the `sweeprolog-top-level' history.

Inputs shorther than the value of this variable will not be
inserted to the input history in `sweeprolog-top-level-mode' buffers."
  :package-version '((sweeprolog . "0.2.1"))
  :type 'natnum)

(defcustom sweeprolog-init-args
  (append
   (when (and (featurep 'xwidget-internal)
              (when-let (swipl (or sweeprolog-swipl-path
                                   (executable-find "swipl")))
                (<= 90114               ; first SWI-Prolog version to
                                        ; hide XPCE private symbols
                    (string-to-number
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process swipl
                                       nil '(t nil) nil
                                       "-g"
                                       "current_prolog_flag(version, V), writeln(V)"
                                       "-t" "halt")))))))
     ;; Disable XPCE if Emacs has been built with Xwidgets to
     ;; workaround a potential crash due to symbol collision
     ;; (see https://github.com/SWI-Prolog/swipl-devel/issues/1188).
     '("--pce=false"))
   (list "-q"
         "--no-signals"
         "-g"
         "create_prolog_flag(sweep,true,[access(read_only),type(boolean)])"
         ;; SWI-Prolog does its own locale initialization, but Emacs's
         ;; Elisp reader only works with certain values of LC_NUMERIC,
         ;; so we need to reset it after loading Prolog.  This is
         ;; basically emulating fixup_locale from src/emacs.c:
         "-g" "setlocale(numeric, _, 'C')"
         "-O"
         "-l"
         (expand-file-name
          "sweep.pl"
          sweeprolog--directory)))
  "List of strings used as initialization arguments for Prolog."
  :package-version '((sweeprolog "0.22.2"))
  :type '(repeat string))

(defcustom sweeprolog-enable-flymake t
  "Whether to enable Flymake support in Sweep Prolog mode buffers."
  :package-version '((sweeprolog "0.6.0"))
  :type 'boolean)

(defcustom sweeprolog-note-implicit-autoloads t
  "Whether Flymake should note implicitly autoload predicates."
  :package-version '((sweeprolog "0.9.2"))
  :type 'boolean)

(defcustom sweeprolog-enable-eldoc t
  "Whether to enable ElDoc support in Sweep Prolog mode buffers."
  :package-version '((sweeprolog "0.4.7"))
  :type 'boolean)

(defcustom sweeprolog-enable-cursor-sensor t
  "Whether to enable `cursor-sensor-mode' in Sweep Prolog mode buffers.

When non-nil, Sweep Prolog mode leverages `cursor-sensor-mode' to
highlight all occurrences of the variable at point in the current
clause."
  :package-version '((sweeprolog "0.4.2"))
  :type 'boolean)

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
                 (function :tag "Custom Function")))

(defcustom sweeprolog-top-level-signal-default-goal "sweep_interrupt"
  "Prolog goal used by default for signaling top-level threads."
  :package-version '((sweeprolog "0.8.12"))
  :type 'string)

(defcustom sweeprolog-highlight-holes t
  "Whether to highlight holes in a dedicated face."
  :package-version '((sweeprolog "0.8.12"))
  :type 'boolean)

(defcustom sweeprolog-read-predicate-documentation-function
  #'sweeprolog-read-predicate-documentation-default-function
  "Function used for filling in information for predicate documentation.

The function should take four arguments, MODULE, FUNCTOR, ARITY
and NECK, which have the same meaning as in
`sweeprolog-definition-at-point'.

It should return a list with four elements (MOD HEAD DET SUM),
where HEAD is a string that contains a template head term for
calling the documented predicate (e.g. \"foo(+Bar, -Baz)\"), MOD
is the module name to qualify HEAD with, or nil if the documented
predicate is local to the current module and shouldn't be
module-qualified, DET is the determinism specification of the
predicate, and SUM is the first line of the predicate's
documentation, acting as a short summary."
  :package-version '((sweeprolog "0.10.1"))
  :type '(choice
          (const
           :tag "Prompt in Minibuffer"
           sweeprolog-read-predicate-documentation-default-function)
          (const
           :tag "Use Holes"
           sweeprolog-read-predicate-documentation-with-holes)
          (function
           :tag "Custom Function")))

(defcustom sweeprolog-enable-help-echo t
  "Whether to annotate Prolog tokens with the `help-echo' property.

When non-nil, Sweep Prolog mode adds a short description to each
token via its `help-echo' text property."
  :package-version '((sweeprolog "0.12.0"))
  :type 'boolean)

(defcustom sweeprolog-rename-variable-allow-existing 'confirm
  "Whether to allow renaming variables to existing variable names.
If it is the symbol `confirm', allow but ask for confirmation
first."
  :package-version '((sweeprolog "0.15.1"))
  :type '(choice (const :tag "Allow"   t)
                 (const :tag "Confirm" confirm)
                 (const :tag "Refuse"  nil)))

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
                 (const :tag "Infer" infer)))

(defcustom sweeprolog-highlight-breakpoints t
  "Whether to highlight breakpoints with a dedicated face."
  :package-version '((sweeprolog "0.17.0"))
  :type 'boolean)

(defcustom sweeprolog-predicate-visible-p-function #'sweeprolog-predicate-non-hidden-p
  "Controls how `sweeprolog-read-predicate' filters completion candidates.

This affects commands that read a Prolog predicate indicator in
the minibuffer, such as \\[sweeprolog-find-predicate]
and \\[sweeprolog-describe-predicate].

If non-nil, this is a function that takes a single string
argument, and returns non-nil if that string should be included
as a completion candidate (i.e. it is not hidden).  If this
user option is nil, all known predicates are provided as
completion candidates."
  :package-version '((sweeprolog "0.19.1"))
  :type '(choice (const    :tag "Include all predicates" nil)
                 (const    :tag "Exclude internal hidden predicates"
                           sweeprolog-predicate-non-hidden-p)
                 (function :tag "Custom exclusion predicate")))

(defcustom sweeprolog-top-level-persistent-history nil
  "Controls if and where top-level buffers store persistent history.

If this option is nil, top-level buffers neither read persistent
history on start-up nor write it on exit.  Otherwise, this option
specifies a file name where top-level buffers store their input
history.

If this is a string FILE, top-level buffers use the file FILE for
persistent history.  FILE can be either an absolute file name or
a relative file name, in which case it is expanded relative to
the `default-directory' of the top-level buffer.  If this is a
function, it is called with no arguments to produce a string with
the same meaning.

This option can also be a list of the form (project REL DEF), in
which case the persistent history file that a top-level buffer
uses depends on the project that the buffer belongs to, as
determined by `project-current'.  If the buffer belongs to a
project, its persistent history file is REL relative to the
project's root directory.  Otherwise, the persistent history file
is DEF, which may be nil or omitted to disable persistent history
for top-level buffers that don't belong to any project."
  :package-version '((sweeprolog "0.20.0"))
  :type '(choice (const    :tag "Disable persistent history" nil)
                 (cons     :tag "File name relative to project root"
                           (const project) string)
                 (file     :tag "History file name")
                 (function :tag "Function returning history file name")))

(defcustom sweeprolog-pack-description-max-width 80
  "Maximum pack description width to display during completion.

This is an integer specifying a string width at which
`sweeprolog-pack-install' truncates pack descriptions annotating
pack completion candidates."
  :package-version '((sweeprolog "0.22.2"))
  :type 'natnum)

(defcustom sweeprolog-top-level-use-pty
  (not (memq system-type '(ms-dos windows-nt)))
  "Whether to communicate with top-levels using pseudo-terminal (\"pty\").

By default, this is t on systems where Emacs can use a pty."
  :package-version '((sweeprolog "0.25.0"))
  :type 'boolean)


;;;; Keymaps

(defvar-keymap sweeprolog-mode-map
  :doc "Keymap for `sweeprolog-mode'."
  "C-c C-b" #'sweeprolog-set-breakpoint
  "C-c C-c" #'sweeprolog-analyze-buffer
  "C-c C-d" #'sweeprolog-document-predicate-at-point
  "C-c C-e" #'sweeprolog-export-predicate
  "C-c TAB" #'sweeprolog-forward-hole
  "C-c C-i" #'sweeprolog-forward-hole
  "C-c <backtab>" #'sweeprolog-backward-hole
  "C-c C-S-i" #'sweeprolog-backward-hole
  "C-c C-l" #'sweeprolog-load-buffer
  "C-c C-m" #'sweeprolog-insert-term-with-holes
  "C-c C-o" #'sweeprolog-find-file-at-point
  "C-c C-q" #'sweeprolog-top-level-send-goal
  "C-c C-r" #'sweeprolog-rename-variable
  "C-c C-S-s" #'sweeprolog-query-replace-term
  "C-c C-s" #'sweeprolog-term-search
  "C-c C-t" #'sweeprolog-top-level
  "C-c C-u" #'sweeprolog-update-dependencies
  "C-c C-`" #'sweeprolog-show-diagnostics
  "C-c C-&" #'sweeprolog-async-goal
  "C-c C-%" #'sweeprolog-make-example-usage-comment
  "C-c C--" #'sweeprolog-decrement-numbered-variables
  "C-c C-+" #'sweeprolog-increment-numbered-variables
  "C-c C-_" #'sweeprolog-replace-with-anonymous-variable
  "C-M-^"   #'kill-backward-up-list
  "C-M-m"   #'sweeprolog-insert-term-dwim
  "M-p"     #'sweeprolog-backward-predicate
  "M-n"     #'sweeprolog-forward-predicate
  "M-h"     #'sweeprolog-mark-predicate)

(defvar-keymap sweeprolog-forward-hole-repeat-map
  :doc "Repeat map for \\[sweeprolog-forward-hole]."
  :repeat t
  "TAB"       #'sweeprolog-forward-hole
  "C-i"       #'sweeprolog-forward-hole
  "<backtab>" #'sweeprolog-backward-hole
  "C-S-i"     #'sweeprolog-backward-hole
  "C-m"       #'sweeprolog-insert-term-with-holes)

(defvar-keymap sweeprolog-top-level-mode-map
  :doc "Keymap for `sweeprolog-top-level-mode'."
  "C-c C-c" #'sweeprolog-top-level-signal-current
  "C-c C-i" #'sweeprolog-forward-hole)

(defvar-keymap sweeprolog-top-level-menu-mode-map
  :doc "Local keymap for `sweeprolog-top-level-menu-mode' buffers."
  "RET" #'sweeprolog-top-level-menu-go-to
  "k"   #'sweeprolog-top-level-menu-kill
  "t"   #'sweeprolog-top-level-menu-new
  "s"   #'sweeprolog-top-level-menu-signal)

(defvar-keymap sweeprolog-help-prefix-map
  :doc "Keymap for `sweeprolog' help commands."
  "m" #'sweeprolog-describe-module
  "p" #'sweeprolog-describe-predicate
  "e" #'sweeprolog-view-messages
  "n" #'sweeprolog-view-news)

;;;###autoload (autoload 'sweeprolog-help-prefix-map "sweeprolog" nil t 'keymap)
(defalias 'sweeprolog-help-prefix-map sweeprolog-help-prefix-map)

(defvar-keymap sweeprolog-prefix-map
  :doc "Keymap for `sweeprolog' global commands."
  "B" #'sweeprolog-list-breakpoints
  "F" #'sweeprolog-set-prolog-flag
  "P" #'sweeprolog-pack-install
  "R" #'sweeprolog-restart
  "T" #'sweeprolog-list-top-levels
  "X" #'sweeprolog-xref-project-source-files
  "h" 'sweeprolog-help-prefix-map
  "l" #'sweeprolog-load-buffer
  "m" #'sweeprolog-find-module
  "p" #'sweeprolog-find-predicate
  "q" #'sweeprolog-top-level-send-goal
  "t" #'sweeprolog-top-level
  "&" #'sweeprolog-async-goal)

;;;###autoload (autoload 'sweeprolog-prefix-map "sweeprolog" nil t 'keymap)
(defalias 'sweeprolog-prefix-map sweeprolog-prefix-map)

(defvar-keymap sweeprolog-forward-hole-on-tab-mode-map
  :doc "Keymap for moving to next hole with TAB."
  "TAB" #'sweeprolog-indent-or-forward-hole)

(defvar-keymap sweeprolog-top-level-example-mode-map
  :doc "Keymap for example top-level buffer."
  "C-c C-b" #'sweeprolog-top-level-example-display-source
  "C-c C-q" #'sweeprolog-top-level-example-done)

(defvar-keymap sweeprolog-read-term-map
  :doc "Keymap used by `sweeprolog-read-term'."
  :parent minibuffer-local-map
  "C-m" #'sweeprolog-read-term-try
  "C-j" #'sweeprolog-read-term-try)

(defvar-keymap sweeprolog-read-goal-map
  :doc "Keymap used by `sweeprolog-goal-term'."
  :parent sweeprolog-read-term-map
  "C-i" #'completion-at-point)

;;;; Menu bar

(easy-menu-define sweeprolog-menu (list sweeprolog-mode-map
                                        sweeprolog-top-level-mode-map)
  "Sweep menu."
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
      (derived-mode-p 'sweeprolog-mode) ]
    [ "Insert Module Template"
      auto-insert
      (derived-mode-p 'sweeprolog-mode) ]
    [ "Document Predicate"
      sweeprolog-document-predicate-at-point
      (and (derived-mode-p 'sweeprolog-mode)
           (sweeprolog-definition-at-point)) ]
    [ "Update Autoload Directives" sweeprolog-update-dependencies
      (derived-mode-p 'sweeprolog-mode) ]
    [ "Infer Indentation Style" sweeprolog-infer-indent-style
      (derived-mode-p 'sweeprolog-mode) ]
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
    [ "Read the Sweep Manual" sweeprolog-info-manual t ]
    [ "Sweep News" sweeprolog-view-news t ]
    [ "Report Bug" sweeprolog-submit-bug-report t ]))


;;;; Local variables

(defvar-local sweeprolog--diagnostics nil)

(defvar-local sweeprolog--diagnostics-report-fn nil)

(defvar-local sweeprolog--timer nil)

(defvar-local sweeprolog--analyze-buffer-duration 0.2)

(defvar-local sweeprolog--html-footnotes nil)

(defvar-local sweeprolog-top-level-thread-id nil
  "Prolog top-level thread ID corresponding to this buffer.")

(defvar-local sweeprolog-top-level-example-marker nil)

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
  "Load the dynamic module that LINE describes."
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
      (let* ((success nil)
             (lines (process-lines-handling-status
                     (or sweeprolog-swipl-path "swipl")
                     (lambda (status)
                       (setq success (= status 0)))
                     "-q" "-g" "write_sweep_module_location"
                     "-t" "halt"
                     sweep-pl)))
        (if (and success lines)
            (mapc #'sweeprolog--load-module lines)
          (error (concat "Failed to locate `sweep-module'. "
                         "Make sure SWI-Prolog is installed "
                         "and up to date")))))))

(defun sweeprolog-ensure-initialized ()
  "Initilize Prolog, unless already initilized."
  (sweeprolog--ensure-module)
  (sweeprolog-init))

(defun sweeprolog-init (&rest args)
  "Initialize and setup the embedded Prolog runtime.

If specified, ARGS should be a list of string passed to Prolog as
extra initialization arguments."
  (unless sweeprolog--initialized
    (message "Starting Sweep.")
    (apply #'sweeprolog-initialize
           (cons (or sweeprolog-swipl-path (executable-find "swipl"))
                 (append sweeprolog-init-args
                         (append sweeprolog--extra-init-args
                                 args))))
    (setq sweeprolog--initialized t)
    (add-hook 'kill-emacs-query-functions #'sweeprolog-maybe-kill-top-levels)
    (add-hook 'kill-emacs-hook #'sweeprolog--shutdown)
    (sweeprolog-setup-message-hook)))

(defun sweeprolog-maybe-kill-top-levels ()
  "Ask before killing running Prolog top-levels."
  (let ((top-levels (seq-filter (lambda (buffer)
                                  (with-current-buffer buffer
                                    (and (derived-mode-p 'sweeprolog-top-level-mode)
                                         sweeprolog-top-level-thread-id)))
                                (buffer-list))))
    (or (not top-levels)
        (and (let ((num (length top-levels)))
               (y-or-n-p (if (< 1 num)
                             (format "Stop %d running Sweep top-levels?" num)
                           "Stop running Sweep top-level?")))
             (prog1 t
               (dolist (buffer top-levels)
                 (sweeprolog-top-level-delete-process buffer)))))))

(defun sweeprolog--shutdown ()
  "Shutdown Prolog."
  (message "Stopping Sweep.")
  (sweeprolog--query-once "sweep" "sweep_cleanup_threads" nil)
  (sweeprolog-cleanup)
  (setq sweeprolog--initialized       nil
        sweeprolog-prolog-server-port nil))

(defun sweeprolog-shutdown ()
  "Ask before killing running top-levels and shutdown Prolog."
  (interactive)
  (if (sweeprolog-maybe-kill-top-levels)
      (sweeprolog--shutdown)
    (user-error "Cannot restart Sweep with running top-levels")))

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
  (sweeprolog-shutdown)
  (progn
    (sit-for 1)
    (apply #'sweeprolog-init args)))

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
  "Start the Sweep Prolog top-level embedded server."
  (setq sweeprolog-prolog-server-port
        (sweeprolog--query-once "sweep" "sweep_top_level_server" nil)))

(defun sweeprolog-setup-message-hook ()
  "Setup `thread_message_hook/3' to redirecet Prolog messages."
  (with-current-buffer (get-buffer-create sweeprolog-messages-buffer-name)
    (setq-local window-point-insertion-type t)
    (compilation-minor-mode 1))
  (sweeprolog--query-once "sweep" "sweep_setup_message_hook" nil))


;;;; Prolog messages

(defun sweeprolog-view-messages ()
  "View the log of recent Prolog messages."
  (interactive)
  (with-current-buffer (get-buffer-create sweeprolog-messages-buffer-name)
    (goto-char (point-max))
    (let ((win (display-buffer (current-buffer))))
      (set-window-point win (point))
      win)))

(defun sweeprolog-message (message)
  "Emit the Prolog message MESSAGE to the Sweep messages buffer."
  (with-current-buffer (get-buffer-create sweeprolog-messages-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (let ((kind (car message))
            (content (cdr message)))
        (pcase kind
          (`("debug" . ,topic)
           (insert (propertize "DEBUG" 'face 'sweeprolog-debug-prefix))
           (insert "[")
           (insert (propertize topic 'face 'sweeprolog-debug-topic))
           (insert "]: ")
           (insert content))
          ("informational"
           (insert (propertize "INFO" 'face 'sweeprolog-info-prefix))
           (insert ": ")
           (insert content))
          ("warning"
           (insert (propertize "WARNING" 'face 'sweeprolog-warning-prefix))
           (insert ": ")
           (insert content))
          ("error"
           (insert (propertize "ERROR" 'face 'sweeprolog-error-prefix))
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
  "Return a list of predicate completion candidates matchitng PREFIX."
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

(defun sweeprolog--pi-to-functor-arity (mfn)
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
     (cons functor arity))
    (`(compound ":"
                (atom . ,_)
                (compound "//"
                          (atom . ,functor)
                          ,arity))
     (cons functor (+ arity 2)))
    (`(compound "//"
                (atom . ,functor)
                ,arity)
     (cons functor (+ arity 2)))))

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
  (let ((functor-arity (sweeprolog--pi-to-functor-arity mfn)))
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

(defun sweeprolog-predicate-non-hidden-p (pred)
  "Return nil if PRED is an internal hidden predicate.
These are predicates whose functor begin with $."
  (not (string-match (rx ":'$") pred)))

(defun sweeprolog-read-predicate (&optional prompt)
  "Read a Prolog predicate from the minibuffer with prompt PROMPT.
If PROMPT is nil, `sweeprolog-read-predicate-prompt' is used by
default."
  (let* ((col (sweeprolog-predicates-collection))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (when-let ((val (alist-get key col nil nil #'string=)))
                    (concat (make-string (- 64 (length key)) ? ) val)))))
         (default (sweeprolog-identifier-at-point)))
    (completing-read
     (format-prompt (or prompt sweeprolog-read-predicate-prompt)
                    default)
     col
     (when sweeprolog-predicate-visible-p-function
       (lambda (cand)
         (funcall sweeprolog-predicate-visible-p-function
                  (car cand))))
     'require-match nil 'sweeprolog-read-predicate-history default)))

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
(defun sweeprolog-find-predicate (pi &optional other-window)
  "Jump to the definition of the Prolog predicate PI.

PI should be a string of the form \"M:F/A\" or \"M:F//A\", where
M is a Prolog module name, F is a functor and A is its arity.

If OTHER-WINDOW is non-nil, find it in another window.

Interactively, this command prompts for PI, and OTHER-WINDOW is
the prefix argument."
  (interactive (list (sweeprolog-read-predicate)
                     current-prefix-arg))
  (if-let ((loc (sweeprolog-predicate-location pi)))
      (let ((path (car loc))
            (line (or (cdr loc) 1)))
        (if other-window
            (find-file-other-window path)
          (find-file path))
        (goto-char (point-min))
        (forward-line (1- line)))
    (user-error "Unable to locate predicate %s" pi)))

(defun sweeprolog--fragment-to-mfa (fragment buffer-module)
  (pcase fragment
    ((or `("head_term" ,kind ,functor ,arity)
         `("head"      ,kind ,functor ,arity))
     (pcase kind
       ((or "unreferenced"
            "meta"
            "exported"
            "hook"
            "public"
            "dynamic"
            "multifile"
            "local")
        (list buffer-module functor arity))
       ((or "def_iso"
            "def_swi"
            "iso"
            "built_in")
        (list "system" functor arity))
       (`("imported" . ,file)
        (list (sweeprolog-path-module file) functor arity))
       (`("extern" ,module . ,_)
        (list module functor arity))))
    ((or `("goal_term" ,kind ,functor ,arity)
         `("goal"      ,kind ,functor ,arity))
     (pcase kind
       ((or "meta"
            "hook"
            "dynamic"
            "multifile"
            "local"
            "undefined"
            "thread_local"
            "expanded"
            "recursion")
        (list buffer-module functor arity))
       ((or "def_iso"
            "def_swi"
            "iso"
            "built_in"
            "foreign")
        (list "system" functor arity))
       (`(,(or "imported" "autoload") . ,file)
        (list (sweeprolog-path-module file) functor arity))
       (`("extern" ,module . ,_)
        (list module functor arity))
       ((or "global"
            `("global" . ,_))
        (list "user" functor arity))))))

(defun sweeprolog--mfa-to-pi (module functor arity)
  (unless (eq functor 'variable)
    (sweeprolog--query-once "sweep" "sweep_module_functor_arity_pi"
                            (list module functor arity))))

(defun sweeprolog-path-module (file)
  (sweeprolog--query-once "sweep" "sweep_path_module" file))

(defun sweeprolog-buffer-module (&optional buffer)
  (sweeprolog-path-module (buffer-file-name buffer)))

(defun sweeprolog-identifier-at-point (&optional point)
  (when (or (derived-mode-p 'sweeprolog-mode)
            (derived-mode-p 'sweeprolog-top-level-mode))
    (setq point (or point (point)))
    (save-excursion
      (goto-char point)
      (let ((id-at-point nil)
            (buffer-module (sweeprolog-buffer-module)))
        (sweeprolog-analyze-term-at-point
         (lambda (beg end arg)
           (when (<= beg point end)
             (when-let ((mfa (sweeprolog--fragment-to-mfa arg buffer-module)))
               (setq id-at-point mfa)))))
        (when id-at-point
          (apply #'sweeprolog--mfa-to-pi id-at-point))))))


;;;; Modules

(defvar-local sweeprolog--module-max-width nil)

(defun sweeprolog-modules-collection (&optional before after)
  "Return Prolog modules with names including BEFORE and AFTER in order."
  (when-let ((col (sweeprolog--query-once "sweep" "sweep_modules_collection"
                                          (cons before after))))
    (setq sweeprolog--module-max-width
          (seq-max (mapcar #'string-width col)))
    col))

(defun sweeprolog-module-annotation (module)
  "Return a cons cell (FILE . DESC) for MODULE.

FILE is the file name of MODULE and DESC is its description, or nil."
  (sweeprolog--query-once "sweep" "sweep_module_annotation" module))

(defun sweeprolog-module-minibuffer-annotation-1 (module pad)
  "Return a string used to annotate MODULE while padding to PAD."
  (let* ((width (string-width module))
         (file-desc (sweeprolog-module-annotation module))
         (file (car file-desc))
         (desc (cdr file-desc)))
    (propertize
     (concat
      (make-string
       (+ (- pad width) 2)
       ?\s)
      (when file (concat file
                         (when desc (concat ": "))))
      (replace-regexp-in-string (rx "library(" (+ graph) "): ") ""
                                (or desc "")))
     'face 'sweeprolog-structured-comment)))

(defun sweeprolog-module-minibuffer-annotation (module)
  "Annotation function for module completion candidates.

Return a string used to annotate MODULE."
  (sweeprolog-module-minibuffer-annotation-1 module
                                             (or sweeprolog--module-max-width
                                                 (string-width module))))

(defun sweeprolog-module-minibuffer-affixation (completions)
  "Affixation function for module completion candidates.

Map COMPLETIONS to a list of elements (CAND PRE SUF), where CAND
is a candidate string, PRE is a prefix string to display before
the candidate and SUF is its suffix to display after it."
  (when completions
    (let ((module-max-width (seq-max (mapcar #'string-width
                                             completions))))
      (mapcar (lambda (cand)
                (list cand ""
                      (sweeprolog-module-minibuffer-annotation-1
                       cand module-max-width)))
              completions))))

(defun sweeprolog-module-minibuffer-group (completion transform)
  "Grouping function for module completion candidates.

See (info \"(elisp)Programmed Completion\") for the meaning of
COMPLETION and TRANSFORM."
  (if transform
      completion
    (sweeprolog--query-once "sweep" "sweep_module_class" completion)))

(defun sweeprolog-module-p (mod)
  "Return non-nil if MOD is a known Prolog module."
  (not (null (sweeprolog--query-once "sweep" "sweep_is_module" mod))))

(defun sweeprolog-module-completion-table (string predicate action)
  "Programmed completion function for prolog modules.

See (info \"(elisp)Programmed Completion\") for the meaning of
STRING, PREDICATE and ACTION."
  (cond
   ((eq action 'lambda)
    (and (sweeprolog-module-p string)
         (or (null predicate)
             (funcall predicate string))))
   ((eq action 'metadata)
    '(metadata
      (category            . sweeprolog-module)
      (annotation-function . sweeprolog-module-minibuffer-annotation)
      (affixation-function . sweeprolog-module-minibuffer-affixation)
      (group-function      . sweeprolog-module-minibuffer-group)))
   (t (complete-with-action action
                            (sweeprolog-modules-collection string)
                            string
                            predicate))))

(defun sweeprolog-read-module-name ()
  "Read a Prolog module name from the minibuffer, with completion."
  (completing-read sweeprolog-read-module-prompt
                   #'sweeprolog-module-completion-table
                   nil nil nil
                   'sweeprolog-read-module-history))

(defun sweeprolog-module-path (mod)
  "Return the name of the file that defining the Prolog module MOD."
  (sweeprolog--query-once "sweep" "sweep_module_path" mod))

;;;###autoload
(defun sweeprolog-find-module (mod &optional other-window)
  "Jump to the source file of the Prolog module MOD.

If OTHER-WINDOW is non-nil, find it in another window.

Interactively, OTHER-WINDOW is the prefix argument and this
command prompts for MOD."
  (interactive (list (sweeprolog-read-module-name)
                     current-prefix-arg))
  (if-let ((file (sweeprolog-module-path mod)))
      (if other-window
          (find-file-other-window file)
        (find-file file))
    (user-error "Module %s is not defined in a source file!" mod)))

(defsubst sweeprolog-syntax-class-at (pos)
  "Return the syntax class of the character at POS."
  (syntax-class (syntax-after pos)))

;;;; Completion at point

(defun sweeprolog-completion-at-point ()
  "Completion-at-point function for Prolog code.

Sweep adds this function to `completion-at-point-functions' in
Prolog buffers."
  (let ((ppss (syntax-ppss)))
    (pcase (nth 3 ppss)
      (?\'
       ;; point is inside a quoted atom/functor
       (sweeprolog--quoted-atom-or-functor-completion-at-point (nth 8 ppss)))
      (?\"
       ;; point is inside a string
       (sweeprolog--string-completion-at-point ppss))
      ('nil (if (nth 4 ppss)
                ;; point is inside a comment
                (sweeprolog--comment-completion-at-point ppss)
              (sweeprolog--completion-at-point))))))

(defun sweeprolog-variable-start-char-p (char)
  (sweeprolog--query-once "sweep" "sweep_variable_start_code" char))

(defun sweeprolog-count-arguments-forward (&optional pos)
  (save-excursion
    (if pos
        (goto-char pos)
      (setq pos (point)))
    (if (pcase (sweeprolog-next-token-boundaries)
          (`(close ,beg ,_)
           (= (char-after beg) ?\))))
        0
      (let ((result 1))
        (while (progn
                 (ignore-error scan-error
                   (sweeprolog--forward-term 999))
                 (and (< pos (point))
                      (pcase (sweeprolog-next-token-boundaries)
                        (`(operator ,obeg ,oend)
                         (string=
                          ","
                          (buffer-substring-no-properties obeg
                                                          oend))))))
          (forward-char 1)
          (setq pos (point))
          (cl-incf result))
        result))))

(defun sweeprolog--end-of-quote (bound)
  (save-excursion
    (let ((go t)
          (res nil)
          (pos (point)))
      (while go
        (if (re-search-forward (rx (or "\\" "'")) bound t)
            (if (= (char-before) ?\')
                (setq res (point)
                      go nil)
              (forward-char))
          (goto-char pos)
          (setq go nil)))
      res)))

(defun sweeprolog--qualyfing-module (pos)
  (pcase (sweeprolog-last-token-boundaries pos)
    (`(operator ,beg ,end)
     (when (string= (buffer-substring-no-properties beg end) ":")
       (pcase (sweeprolog-last-token-boundaries beg)
         (`(string ,sbeg ,send)
          (when (= (char-after sbeg) ?\')
            (buffer-substring-no-properties sbeg send)))
         (`(symbol ,sbeg ,send)
          (unless (sweeprolog-variable-start-char-p (char-after sbeg))
            (buffer-substring-no-properties sbeg send))))))))


(defun sweeprolog--parse-context (&optional pos)
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    (sweeprolog-backward-term 0)
    (let ((pos )
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
                      (equal 1 (sweeprolog-syntax-class-at obeg))
                      (let ((sa (sweeprolog-syntax-class-at (1+ obeg))))
                        (or (null sa) (member sa '(0 12)))))
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

(defun sweeprolog-context-callable-p (&optional point)
  "Check if POINT is in a position where a goal should appear."
  (sweeprolog--query-once "sweep" "sweep_context_callable"
                          (sweeprolog--parse-context point)))

(defun sweeprolog-local-variables-collection (&rest exclude)
  "Return a list of variable names that occur in the current clause.

EXCLUDE is a list of variable names to exclude from the resulting
list even when found in the current clause."
  (let* ((case-fold-search nil)
         (beg (save-mark-and-excursion
                (unless (sweeprolog-at-beginning-of-top-term-p)
                  (sweeprolog-beginning-of-top-term))
                (point)))
         (end (save-mark-and-excursion
                (sweeprolog-end-of-top-term)
                (point)))
         (vars nil))
    (save-excursion
      (goto-char beg)
      (save-match-data
        (while (re-search-forward (rx bow (or "_" upper)
                                      (* (or "_" alnum)))
                                  end t)
          (unless (nth 8 (syntax-ppss))
            (let ((match (match-string-no-properties 0)))
              (unless (or (member match exclude)
                          (member match vars))
                (push (match-string-no-properties 0) vars)))))))
    vars))

(defun sweeprolog-predicate-completion-sort (candidates)
  "Sort predicate completion CANDIDATES by functor length."
  (sort candidates
        (lambda (a b)
          (< (or (string-search "(" a) (length a))
             (or (string-search "(" b) (length b))))))

(defun sweeprolog-predicate-completion-candidates (beg end cxt)
  (let* ((col (sweeprolog--query-once
               "sweep" "sweep_heads_collection"
               (list cxt
                     (sweeprolog--qualyfing-module beg)
                     (buffer-substring-no-properties beg (point))
                     (buffer-substring-no-properties (point) end))))
         (table (make-hash-table :test 'equal :size (length col))))
    (dolist (cand col)
      (puthash (car cand) (cdr cand) table))
    (let ((sort-fn
           (lambda (cands)
             (sort cands
                   (lambda (a b)
                     (seq-let (aargs aflen arity) (gethash a table)
                       (seq-let (bargs bflen brity) (gethash b table)
                         (cond
                          ((and aargs (not bargs)))
                          ((and bargs (not aargs)) nil)
                          ((< aflen bflen))
                          ((< bflen aflen) nil)
                          ((< arity brity))
                          ((< brity arity) nil)
                          ((string< a b))))))))))
      (list beg end
            (lambda (s p a)
              (if (eq a 'metadata)
                  (list 'metadata
                        (cons 'display-sort-function sort-fn)
                        (cons 'cycle-sort-function   sort-fn))
                (complete-with-action a table s p)))
            :exclusive 'no
            :annotation-function (lambda (_) " Predicate")
            :exit-function
            (lambda (string status)
              (pcase status
                ('finished
                 (pcase (sweeprolog--query-once
                         "sweep" "sweep_subterm_positions" string)
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
                             'font-lock-face (list 'sweeprolog-hole)
                             'rear-nonsticky '(sweeprolog-hole
                                               cursor-sensor-functions
                                               font-lock-face)))))))
                    (backward-char length)
                    (sweeprolog-forward-hole))))))))))

(defun sweeprolog-compound-completion-candidates (beg end)
  (let ((col (sweeprolog--query-once
              "sweep" "sweep_functors_collection"
              (cons (buffer-substring-no-properties beg (point))
                    (buffer-substring-no-properties (point) end)))))
    (list beg end col
          :exclusive 'no
          :annotation-function (lambda (_) " Compound")
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
                           'font-lock-face (list 'sweeprolog-hole)
                           'rear-nonsticky '(sweeprolog-hole
                                             cursor-sensor-functions
                                             font-lock-face)))))))
                  (backward-char length)
                  (sweeprolog-forward-hole)))))))))

(defun sweeprolog-predicate-functor-completion-candidates (beg end ari cxt)
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_head_functors_collection"
         (list ari cxt
               (sweeprolog--qualyfing-module beg)
               (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)))
        :exclusive 'no
        :annotation-function (lambda (_) " Predicate functor")))

(defun sweeprolog-compound-functor-completion-candidates (beg end ari)
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_compound_functors_collection"
         (list ari
               (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)))
        :exclusive 'no
        :annotation-function (lambda (_) " Functor")))

(defun sweeprolog--flag-completion-at-point (beg end)
  "Return completion candidates for the Prolog flag between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo(Bar, Baz) :- set_prolog_flag(ba-!-"
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_flags_collection"
         (cons (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)))
        :exclusive 'no
        :annotation-function
        (lambda (_) " Flag")))

(defun sweeprolog--atom-or-functor-completion-at-point (beg end)
  "Return completion candidates for the atom or functor between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo :- bar-!-"
  (let* ((cxt (sweeprolog-context-callable-p beg))
         (open-paren (char-after end))
         (fnc (and open-paren (= open-paren ?\()
                   (sweeprolog-count-arguments-forward (1+ end)))))
    (pcase cxt
      ((pred integerp)
       (if fnc
           (sweeprolog-predicate-functor-completion-candidates beg end fnc cxt)
         (sweeprolog-predicate-completion-candidates beg end cxt)))
      ("source"
       (if fnc
           (sweeprolog-source-functor-completion-candidates beg end)
         (sweeprolog-source-completion-candidates beg end)))
      (`("source" . ,source)
       (sweeprolog-alias-source-completion-candidates beg end source))
      (`("option" ,pred ,ari)
       (if fnc
           (sweeprolog-option-functor-completion-candidates beg end pred ari)
         (sweeprolog-option-completion-candidates beg end pred ari)))
      (`("option" ,pred ,ari ,option)
       (sweeprolog-option-arg-completion-candidates beg end pred ari option))
      ("arith"
       (if fnc
           (sweeprolog-arith-functor-completion-candidates beg end)
         (sweeprolog-arith-completion-candidates beg end)))
      ("flag"
       (sweeprolog--flag-completion-at-point beg end))
      (_
       (if fnc
           (sweeprolog-compound-functor-completion-candidates beg end fnc)
         (sweeprolog-compound-completion-candidates beg end))))))

(defun sweeprolog--variable-completion-at-point (beg end)
  "Return completion candidates for the variable between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo(Bar, Baz) :- member(Ba-!-"
  (list beg end
        (sweeprolog-local-variables-collection
         (buffer-substring-no-properties beg end))
        :exclusive 'no
        :annotation-function
        (lambda (_) " Var")))

(defun sweeprolog--quoted-atom-or-functor-completion-at-point (beg)
  "Return completion candidates for the quoted atom starting at BEG.

Used for `completion-at-point' candidates in cases such as:

    foo :- \\='$bar-!-baz\\='("
  (let* ((end (or (sweeprolog--end-of-quote (pos-eol))
                  (point))))
    (sweeprolog--atom-or-functor-completion-at-point beg end)))

(defun sweeprolog--string-completion-at-point (&rest _)
  "Return completion candidates for the Prolog string at point.

Used for `completion-at-point' candidates in cases such as:

    foo :- bar(\"baz-!-"
  nil)

(defun sweeprolog--comment-completion-at-point (&rest _)
  "Return completion candidates for the Prolog comment at point.

Used for `completion-at-point' candidates in cases such as:

    % foo
    % bar-!-"
  nil)

(defun sweeprolog--after-atom-or-variable-completion-at-point (&rest _)
  "Return completion candidates after a Prolog atom or variable.

Used for `completion-at-point' candidates in cases such as:

    foo :- bar -!-"
  nil)

(defun sweeprolog--after-operator-completion-at-point (&rest _)
  "Return completion candidates after a Prolog operator.

Used for `completion-at-point' candidates in cases such as:

    foo(Bar) :- Bar = -!-"
  nil)

(defun sweeprolog--first-term-completion-at-point (&rest _)
  "Return completion candidates at the beginning of a Prolog buffer."
  nil)

(defun sweeprolog--after-term-completion-at-point (&rest _)
  "Return completion candidates after a Prolog term.

Used for `completion-at-point' candidates in cases such as:

    foo(Bar) :- baz(Bar) -!-"
  nil)

(defun sweeprolog--after-curly-brace-completion-at-point (&rest _)
  "Return completion candidates after a curly brace.

Used for `completion-at-point' candidates in cases such as:

    foo(Bar) --> baz, {-!-"
  nil)

(defun sweeprolog--first-dict-argument-completion-at-point (&rest _)
  "Return completion candidates for the first Prolog dictionary key.

Used for `completion-at-point' candidates in cases such as:

    foo(Bar) :- Bar = baz{-!-"
  nil)

(defun sweeprolog--first-list-argument-completion-at-point (&rest _)
  "Return completion candidates for the list argument at point.

Used for `completion-at-point' candidates in cases such as:

    foo :- member(X, [-!-"
  nil)

(defun sweeprolog--term-completion-at-point (&rest _)
  "Return Prolog term at-point completion candidates.

Used for `completion-at-point' candidates in cases such as:

    foo :- bar, (-!-"
  nil)

(defun sweeprolog--after-quoted-functor-completion-at-point (&rest _)
  "Return completion candidates for the compound term at point.

Used for `completion-at-point' candidates in cases such as:

    foo :- \\='$Bar\\='(-!-"
  nil)

(defun sweeprolog--after-functor-completion-at-point (&rest _)
  "Return completion candidates for the compound term at point.

Used for `completion-at-point' candidates in cases such as:

    foo :- bar(-!-"
  nil)

(defun sweeprolog--operator-completion-at-point (&rest _)
  "Return completion candidates for the Prolog operator at point.

Used for `completion-at-point' candidates in cases such as:

    foo :- 123 =-!- 100 + 20 + 3"
  nil)

(defun sweeprolog-arith-functor-completion-candidates (beg end)
  "Return completions for arithmetic function functors between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo(T) :- T is f-!-t("
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_function_functors_collection"
         (list (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)))
        :exclusive 'no
        :annotation-function (lambda (_) " Arithmetic function functor")))

(defun sweeprolog-arith-completion-candidates (beg end)
  "Return completions for arithmetic expression between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo(T) :- T is f-!-"
  (let ((col (sweeprolog--query-once
              "sweep" "sweep_functions_collection"
              (list (buffer-substring-no-properties beg (point))
                    (buffer-substring-no-properties (point) end)))))
    (list beg end col
          :exclusive 'no
          :annotation-function (lambda (_) " Arithmentic function")
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
                           'font-lock-face (list 'sweeprolog-hole)
                           'rear-nonsticky '(sweeprolog-hole
                                             cursor-sensor-functions
                                             font-lock-face)))))))
                  (backward-char length)
                  (sweeprolog-forward-hole)))))))))

(defun sweeprolog-option-functor-completion-candidates (beg end pred ari)
  "Return completions for option functors for PRED/ARI between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo(T) :- read_term(T, [va-!-es("
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_option_functors_collection"
         (list (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)
               pred ari ari))
        :exclusive 'no
        :annotation-function (lambda (_) " Option functor")))

(defun sweeprolog-option-completion-candidates (beg end pred ari)
  "Return completions for options for PRED/ARI between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo(T) :- read_term(T, [va-!-"
  (let ((col (sweeprolog--query-once
              "sweep" "sweep_options_collection"
              (list (buffer-substring-no-properties beg (point))
                    (buffer-substring-no-properties (point) end)
                    pred ari ari))))
    (list beg end col
          :exclusive 'no
          :annotation-function (lambda (_) " Option")
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
                           'font-lock-face (list 'sweeprolog-hole)
                           'rear-nonsticky '(sweeprolog-hole
                                             cursor-sensor-functions
                                             font-lock-face)))))))
                  (backward-char length)
                  (sweeprolog-forward-hole)))))))))

(defun sweeprolog-option-arg-completion-candidates (beg end pred ari option)
  "Return completions for argument of OPTION for PRED/ARI between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    foo(T) :- read_term(T, [syntax_errors(fa-!-"
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_option_arguments_collection"
         (list (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)
               pred ari ari option))
        :exclusive 'no
        :annotation-function (lambda (_) " Option argument")))

(defun sweeprolog-source-functor-completion-candidates (beg end)
  "Return completion candidates for the Prolog path alias between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    :- use_module(lib-!-ry("
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_path_alias_collection"
         (cons (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)))
        :exclusive 'no
        :annotation-function (lambda (_) " Path alias functor")))

(defun sweeprolog-alias-source-completion-candidates (beg end alias)
  "Return completions for the file specification in ALIAS between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    :- use_module(library(prol-!-"
  (list beg end
        (sweeprolog--query-once
         "sweep" "sweep_alias_source_file_name_collection"
         (list (buffer-substring-no-properties beg (point))
               (buffer-substring-no-properties (point) end)
               alias))
        :exclusive 'no
        :annotation-function (lambda (_) " Source")))

(defun sweeprolog-source-completion-candidates (beg end)
  "Return completions for the file specification between BEG and END.

Used for `completion-at-point' candidates in cases such as:

    :- use_module(lib-!-"
  (list beg end
        (append
         (mapcar (lambda (file-name)
                   (file-name-sans-extension
                    (file-relative-name file-name default-directory)))
                 (directory-files-recursively default-directory
                                              (rx ".pl" eos) nil t))
         (sweeprolog--query-once
          "sweep" "sweep_source_file_name_collection"
          (cons (buffer-substring-no-properties beg (point))
                (buffer-substring-no-properties (point) end))))
        :exclusive 'no
        :annotation-function (lambda (_) " Source")))

(defun sweeprolog--completion-at-point ()
  "Return completion candidates for the Prolog code at point.

Used for `completion-at-point' candidates when point is not
inside a comment, string or quoted atom."
  (if (bobp)
      (sweeprolog--first-term-completion-at-point)
    (pcase (sweeprolog-syntax-class-at (1- (point)))
      ((or 2 3)
       (let ((symbol-beg (save-excursion
                           (skip-syntax-backward "w_")
                           (point)))
             (symbol-end (save-excursion
                           (skip-syntax-forward "w_")
                           (point))))
         (if (sweeprolog-variable-start-char-p (char-after symbol-beg))
             (sweeprolog--variable-completion-at-point symbol-beg
                                                       symbol-end)
           (sweeprolog--atom-or-functor-completion-at-point symbol-beg
                                                            symbol-end))))
      (1 (sweeprolog--operator-completion-at-point))
      (4 (pcase (char-before)
           (?\( (when-let ((prev (char-before (1- (point)))))
                  (pcase (sweeprolog-syntax-class-at (- (point) 2))
                    ((or 2 3)
                     (sweeprolog--after-functor-completion-at-point))
                    (7
                     (when (= prev ?\')
                       (sweeprolog--after-quoted-functor-completion-at-point)))
                    (_ (sweeprolog--term-completion-at-point)))))
           (?\[ (sweeprolog--first-list-argument-completion-at-point))
           (?\{ (when-let ((prev (char-before (1- (point)))))
                  (pcase (sweeprolog-syntax-class-at (- (point) 2))
                    ((or 2 3)
                     (sweeprolog--first-dict-argument-completion-at-point))
                    (_ (sweeprolog--after-curly-brace-completion-at-point)))))))
      ((or 5 7) (sweeprolog--after-term-completion-at-point))
      (0 (pcase (sweeprolog-last-token-boundaries)
           ('nil (sweeprolog--first-term-completion-at-point))
           (`(open ,_ ,_) (sweeprolog--term-completion-at-point))
           (`(functor ,_ ,_) (sweeprolog--after-functor-completion-at-point))
           (`(operator ,obeg ,oend) (sweeprolog--after-operator-completion-at-point obeg oend))
           (`(symbol ,obeg ,oend) (sweeprolog--after-atom-or-variable-completion-at-point obeg oend))
           (`(close ,_ ,_) (sweeprolog--after-term-completion-at-point))
           (`(string ,_ ,_) (sweeprolog--after-term-completion-at-point)))))))

;;;; Packages

(defun sweeprolog-packs-collection ()
  (sweeprolog--query-once "sweep" "sweep_packs_collection" ""))

(defun sweeprolog-read-pack-name ()
  "Read a Prolog pack name from the minibuffer, with completion."
  (let* ((col (sweeprolog-packs-collection))
         (max-pack (seq-max (mapcar #'string-width (mapcar #'car col))))
         (max-desc (min sweeprolog-pack-description-max-width
                        (seq-max (mapcar #'string-width
                                         (mapcar #'cadr col)))))
         (completion-extra-properties
          (list :annotation-function
                (lambda (pack)
                  (message pack)
                  (let* ((info (alist-get pack col nil nil #'string=))
                         (des (car info))
                         (ver (cadr info)))
                    (concat
                     (propertize " " 'display
                                 `(space :align-to
                                         ,(+ 2 max-pack)))
                     (truncate-string-to-width
                      des sweeprolog-pack-description-max-width
                      nil nil t)
                     (propertize " " 'display
                                 `(space :align-to
                                         ,(+ 2 max-pack 2 max-desc)))
                     ver))))))
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

(make-obsolete-variable
 'sweeprolog-faces-style
 (concat
  "This option is deprecated and will be removed "
  "in a future version of Sweep.  "
  "To emulate the color scheme of PceEmacs, use the "
  "`sweeprolog-pce' custom theme instead.")
 "Sweep version 0.21.0")

(defface sweeprolog-debug-prefix
  '((t :inherit shadow))
  "Face for highlighting the \"DEBUG\" message prefix."
  :group 'sweeprolog-faces)

(defface sweeprolog-debug-topic
  '((t :inherit shadow))
  "Face for highlighting the topic in debug messages."
  :group 'sweeprolog-faces)

(defface sweeprolog-info-prefix
  '((t :inherit default))
  "Face for highlighting the \"INFO\" message prefix."
  :group 'sweeprolog-faces)

(defface sweeprolog-warning-prefix
  '((t :inherit font-lock-warning-face))
  "Face for highlighting the \"WARNING\" message prefix."
  :group 'sweeprolog-faces)

(defface sweeprolog-error-prefix
  '((t :inherit error))
  "Face for highlighting the \"ERROR\" message prefix."
  :group 'sweeprolog-faces)

(defface sweeprolog-function
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog arithmetic functions."
  :group 'sweeprolog-faces)

(defface sweeprolog-no-function
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog unknown arithmetic functions."
  :group 'sweeprolog-faces)

(defface sweeprolog-functor
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog functors."
  :group 'sweeprolog-faces)

(defface sweeprolog-arity
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog arities."
  :group 'sweeprolog-faces)

(defface sweeprolog-predicate-indicator
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog predicate indicators."
  :group 'sweeprolog-faces)

(defface sweeprolog-built-in
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog built in predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-neck
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog necks."
  :group 'sweeprolog-faces)

(defface sweeprolog-goal
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog unspecified predicate goals."
  :group 'sweeprolog-faces)

(defface sweeprolog-string
  '((t :inherit font-lock-string-face))
  "Face for highlighting Prolog strings."
  :group 'sweeprolog-faces)

(defface sweeprolog-comment
  '((t :inherit font-lock-comment-face))
  "Face for highlighting Prolog comments."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-built-in
  '((t :background "orange" :weight bold))
  "Face for highlighting Prolog built-in predicate definitons."
  :group 'sweeprolog-faces)

(defface sweeprolog-method
  '((t :weight bold))
  "Face for highlighting Prolog pce classes."
  :group 'sweeprolog-faces)

(defface sweeprolog-class
  '((t :underline t))
  "Face for highlighting Prolog pce classes."
  :group 'sweeprolog-faces)

(defface sweeprolog-no-file
  '((t :foreground "red"))
  "Face for highlighting Prolog non-existsing file specifications."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-local
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting Prolog local predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-meta
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog meta predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-dynamic
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog dynamic predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-multifile
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog multifile predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-extern
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog external predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-test
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog unreferenced predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-unreferenced
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog unreferenced predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-exported
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting Prolog exported predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-hook
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog hook definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-iso
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog iso specified predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-def-iso
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting Prolog built-in ISO specified predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-def-swi
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting Prolog built-in SWI-Prolog predicate definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-imported
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog imported head terms."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-undefined
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog undefined head terms."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-public
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting Prolog public definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-head-constraint
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog constraint definitions."
  :group 'sweeprolog-faces)

(defface sweeprolog-meta-spec
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog meta argument specifiers."
  :group 'sweeprolog-faces)

(defface sweeprolog-recursion
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting Prolog recursive calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-local
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog local predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-expanded
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog expanded predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-autoload
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog autoloaded predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-imported
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog imported predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-extern
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog external predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-foreign
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog foreign predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-meta
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog meta predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-undefined
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog undefined predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-thread-local
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog thread local predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-not-callable
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog terms that are not callable."
  :group 'sweeprolog-faces)

(defface sweeprolog-constraint
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog constraint calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-deprecated
  '((t :inherit font-lock-warning-face))
  "Face for highlighting deprecated predicates."
  :group 'sweeprolog-faces)

(defface sweeprolog-global
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog global predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-multifile
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog multifile predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-dynamic
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog dynamic predicate calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-undefined-import
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog undefined imports."
  :group 'sweeprolog-faces)

(defface sweeprolog-html-attribute
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog html attributes."
  :group 'sweeprolog-faces)

(defface sweeprolog-html-call
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog html calls."
  :group 'sweeprolog-faces)

(defface sweeprolog-option-name
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog option names."
  :group 'sweeprolog-faces)

(defface sweeprolog-no-option-name
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog non-existent option names."
  :group 'sweeprolog-faces)

(defface sweeprolog-flag-name
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog flag names."
  :group 'sweeprolog-faces)

(defface sweeprolog-no-flag-name
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog non-existent flag names."
  :group 'sweeprolog-faces)

(defface sweeprolog-qq-type
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog quasi-quotation types."
  :group 'sweeprolog-faces)

(defface sweeprolog-qq-sep
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog quasi-quotation separators."
  :group 'sweeprolog-faces)

(defface sweeprolog-qq-open
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog quasi-quotation open sequences."
  :group 'sweeprolog-faces)

(defface sweeprolog-qq-content
  '((t))
  "Face for highlighting Prolog quasi-quotation content."
  :group 'sweeprolog-faces)

(defface sweeprolog-qq-close
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog quasi-quotation close sequences."
  :group 'sweeprolog-faces)

(defface sweeprolog-op-type
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog operator types."
  :group 'sweeprolog-faces)

(defface sweeprolog-dict-tag
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog dict tags."
  :group 'sweeprolog-faces)

(defface sweeprolog-dict-key
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog dict keys."
  :group 'sweeprolog-faces)

(defface sweeprolog-dict-sep
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog dict separators."
  :group 'sweeprolog-faces)

(defface sweeprolog-dict-return-op
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog dict return operators."
  :group 'sweeprolog-faces)

(defface sweeprolog-dict-function
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting Prolog dict functions."
  :group 'sweeprolog-faces)

(defface sweeprolog-func-dot
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog dict function dots."
  :group 'sweeprolog-faces)

(defface sweeprolog-file
  '((t :inherit button))
  "Face for highlighting Prolog file specifiers."
  :group 'sweeprolog-faces)

(defface sweeprolog-file-no-depend
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog unused file specifiers."
  :group 'sweeprolog-faces)

(defface sweeprolog-unused-import
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog unused imports."
  :group 'sweeprolog-faces)

(defface sweeprolog-identifier
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog identifiers."
  :group 'sweeprolog-faces)

(defface sweeprolog-hook
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog hooks."
  :group 'sweeprolog-faces)

(defface sweeprolog-module
  '((t :inherit font-lock-type-face))
  "Face for highlighting Prolog module names."
  :group 'sweeprolog-faces)

(defface sweeprolog-singleton
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog singletons."
  :group 'sweeprolog-faces)

(defface sweeprolog-fullstop
  '((t :inherit font-lock-negation-char-face))
  "Face for highlighting Prolog fullstops."
  :group 'sweeprolog-faces)

(defface sweeprolog-nil
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting the Prolog empty list."
  :group 'sweeprolog-faces)

(defface sweeprolog-variable-at-point
  '((t :underline t))
  "Face for highlighting Prolog variables."
  :group 'sweeprolog-faces)

(defface sweeprolog-variable
  '((t :inherit font-lock-variable-name-face))
  "Face for highlighting Prolog variables."
  :group 'sweeprolog-faces)

(defface sweeprolog-ext-quant
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog existential quantifiers."
  :group 'sweeprolog-faces)

(defface sweeprolog-keyword
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog control constructs."
  :group 'sweeprolog-faces)

(defface sweeprolog-control
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting Prolog control constructs."
  :group 'sweeprolog-faces)

(defface sweeprolog-atom
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog atoms."
  :group 'sweeprolog-faces)

(defface sweeprolog-int
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog integers."
  :group 'sweeprolog-faces)

(defface sweeprolog-float
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog floats."
  :group 'sweeprolog-faces)

(defface sweeprolog-rational
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog rationals."
  :group 'sweeprolog-faces)

(defface sweeprolog-chars
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog chars."
  :group 'sweeprolog-faces)

(defface sweeprolog-codes
  '((t :inherit font-lock-constant-face))
  "Face for highlighting Prolog character codes."
  :group 'sweeprolog-faces)

(defface sweeprolog-error
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog unspecified errors."
  :group 'sweeprolog-faces)

(defface sweeprolog-type-error
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog type errors."
  :group 'sweeprolog-faces)

(defface sweeprolog-instantiation-error
  '((t :inherit font-lock-warning-face))
  "Face for highlighting Prolog instantiation errors."
  :group 'sweeprolog-faces)

(defface sweeprolog-syntax-error
  '((t :inherit error))
  "Face for highlighting Prolog syntax errors."
  :group 'sweeprolog-faces)

(defface sweeprolog-around-syntax-error
  '((t))
  "Face for highlighting text around a syntax error."
  :group 'sweeprolog-faces)

(defface sweeprolog-clause
  '((t))
  "Face for highlighting Prolog predicate clauses."
  :group 'sweeprolog-faces)

(defface sweeprolog-grammar-rule
  '((t))
  "Face for highlighting Prolog DCG grammar rules."
  :group 'sweeprolog-faces)

(defface sweeprolog-term
  '((t))
  "Face for highlighting Prolog top terms."
  :group 'sweeprolog-faces)

(defface sweeprolog-body
  '((t))
  "Face for highlighting Prolog clause and query bodies."
  :group 'sweeprolog-faces)

(defface sweeprolog-directive
  '((t))
  "Face for highlighting Prolog directives."
  :group 'sweeprolog-faces)

(defface sweeprolog-string-comment
  '((t :inherit font-lock-doc-face))
  "Face for highlighting Prolog string comments."
  :group 'sweeprolog-faces)

(defface sweeprolog-structured-comment
  '((t :inherit font-lock-doc-face))
  "Face for highlighting Prolog structured comments."
  :group 'sweeprolog-faces)

(defface sweeprolog-hole
  '((t :box (:line-width (-1 . -1))))
  "Face for highlighting Prolog holes."
  :group 'sweeprolog-faces)

(defface sweeprolog-macro
  '((t :inherit font-lock-preprocessor-face))
  "Face for highlighting Prolog macros."
  :group 'sweeprolog-faces)

(defface sweeprolog-declaration-option
  '((t :weight bold))
  "Face for highlighting Prolog declaration options."
  :group 'sweeprolog-faces)

(defface sweeprolog-dcg-string
  '((t :inherit font-lock-string-face))
  "Face for highlighting Prolog DCG terminal strings."
  :group 'sweeprolog-faces)

(defface sweeprolog-delimiter
  '((t :inherit bold))
  "Face for highlighting CHR delimiters."
  :group 'sweeprolog-faces)

(defface sweeprolog-pragma
  '((t :inherit default))
  "Face for highlighting CHR pragma terms."
  :group 'sweeprolog-faces)

(defface sweeprolog-chr-type
  '((t :inherit font-lock-type-face))
  "Face for highlighting CHR types."
  :group 'sweeprolog-faces)

(defface sweeprolog-breakpoint
  '((((background light)) :background "lightgreen")
    (((background dark))  :background "darkgreen"))
  "Face for highlighting Prolog breakpoints."
  :group 'sweeprolog-faces)

(defface sweeprolog-term-search-match
  '((t :inherit lazy-highlight))
  "Face for highlighting term search matches."
  :group 'sweeprolog-faces)

(defface sweeprolog-term-search-current
  '((t :inherit isearch))
  "Face for highlighting the current term search match."
  :group 'sweeprolog-faces)

(defface sweeprolog-query-replace-term-match
  '((t :inherit sweeprolog-term-search-match))
  "Face for highlighting term replacement matches."
  :group 'sweeprolog-faces)

(defface sweeprolog-query-replace-term-current
  '((t :inherit sweeprolog-term-search-current))
  "Face for highlighting the current term replacement match."
  :group 'sweeprolog-faces)

(defface sweeprolog-query-replace-term-prompt-old
  '((((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#553333" :extend t)
    (((class color))
     :foreground "red" :extend t))
  "Face to use for the old term in `sweeprolog-query-replace-term' queries.")

(defface sweeprolog-query-replace-term-prompt-new
  '((((class color) (min-colors 88) (background light))
     :background "#ddffdd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#335533" :extend t)
    (((class color))
     :foreground "green" :extend t))
  "Face to use for the new term in `sweeprolog-query-replace-term' queries.")

(defface sweeprolog-eldoc-argument-highlight
  '((t :inherit eldoc-highlight-function-argument))
  "Face to use for highliting the argument at point in ElDoc messages.")

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
                          (point))
                        (1+ end) sweeprolog--analyze-point))
               (< (save-excursion
                    (goto-char sweeprolog--analyze-point)
                    (sweeprolog-end-of-top-term) (point))
                  (save-excursion
                    (goto-char sweeprolog--analyze-point)
                    (sweeprolog-beginning-of-next-top-term) (point))
                  (point-max)))
           'sweeprolog-syntax-error)
      'sweeprolog-around-syntax-error))

(defun sweeprolog-analyze-fragment-to-faces (beg end arg)
  (pcase arg
    (`("comment" . "structured")
     (list (list beg end nil)
           (list beg end 'sweeprolog-structured-comment)))
    (`("comment" . "string")
     (list (list beg end nil)
           (list beg end 'sweeprolog-string-comment)))
    (`("comment" . ,_)
     (list (list beg end nil)
           (list beg end 'sweeprolog-comment)))
    (`("head" "unreferenced" . ,_)
     (list (list beg end 'sweeprolog-head-unreferenced)))
    (`("head" "undefined" . ,_)
     (list (list beg end 'sweeprolog-head-undefined)))
    (`("head" "test" . ,_)
     (list (list beg end 'sweeprolog-head-test)))
    (`("head" "meta" . ,_)
     (list (list beg end 'sweeprolog-head-meta)))
    (`("head" "def_iso" . ,_)
     (list (list beg end 'sweeprolog-head-def-iso)))
    (`("head" "def_swi" . ,_)
     (list (list beg end 'sweeprolog-head-def-swi)))
    (`("head" "iso" . ,_)
     (list (list beg end 'sweeprolog-head-iso)))
    (`("head" "exported" . ,_)
     (list (list beg end 'sweeprolog-head-exported)))
    (`("head" "hook" . ,_)
     (list (list beg end 'sweeprolog-head-hook)))
    (`("head" "built_in" . ,_)
     (list (list beg end 'sweeprolog-head-built-in)))
    (`("head" ("imported" . ,_) . ,_)
     (list (list beg end 'sweeprolog-head-imported)))
    (`("head" ("extern" . ,_) . ,_)
     (list (list beg end 'sweeprolog-head-extern)))
    (`("head" "public" . ,_)
     (list (list beg end 'sweeprolog-head-public)))
    (`("head" "dynamic" . ,_)
     (list (list beg end 'sweeprolog-head-dynamic)))
    (`("head" "multifile" . ,_)
     (list (list beg end 'sweeprolog-head-multifile)))
    (`("head" "local" . ,_)
     (list (list beg end 'sweeprolog-head-local)))
    (`("head" "constraint" . ,_)
     (list (list beg end 'sweeprolog-head-constraint)))
    (`("goal" ("autoload" . ,_) . ,_)
     (list (list beg end 'sweeprolog-autoload)))
    (`("goal" "expanded" . ,_)
     (list (list beg end 'sweeprolog-expanded)))
    (`("goal" "recursion" . ,_)
     (list (list beg end 'sweeprolog-recursion)))
    (`("goal" "meta"      . ,_)
     (list (list beg end 'sweeprolog-meta)))
    (`("goal" "built_in"  . ,_)
     (list (list beg end 'sweeprolog-built-in)))
    (`("goal" "undefined" . ,_)
     (list (list beg end 'sweeprolog-undefined)))
    (`("goal" "global" . ,_)
     (list (list beg end 'sweeprolog-global)))
    (`("goal" "not_callable" . ,_)
     (list (list beg end 'sweeprolog-not-callable)))
    (`("goal" "dynamic" . ,_)
     (list (list beg end 'sweeprolog-dynamic)))
    (`("goal" "foreign" . ,_)
     (list (list beg end 'sweeprolog-foreign)))
    (`("goal" "multifile" . ,_)
     (list (list beg end 'sweeprolog-multifile)))
    (`("goal" "thread_local" . ,_)
     (list (list beg end 'sweeprolog-thread-local)))
    (`("goal" ("extern" . ,_) . ,_)
     (list (list beg end 'sweeprolog-extern)))
    (`("goal" ("imported" . ,_) . ,_)
     (list (list beg end 'sweeprolog-imported)))
    (`("goal" ("global" . ,_) . ,_)
     (list (list beg end 'sweeprolog-global)))
    (`("goal" "local" . ,_)
     (list (list beg end 'sweeprolog-local)))
    (`("goal" "constraint" . ,_)
     (list (list beg end 'sweeprolog-constraint)))
    (`("goal" "deprecated" . ,_)
     (list (list beg end 'sweeprolog-deprecated)))
    (`("macro" . ,_)
     (list (list beg end 'sweeprolog-macro)))
    ("expanded"
     (list (list beg end 'sweeprolog-expanded)))
    ("instantiation_error"
     (list (list beg end 'sweeprolog-instantiation-error)))
    (`("type_error" . ,_)
     (list (list beg end 'sweeprolog-type-error)))
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
                                 (list eb ee 'sweeprolog-around-syntax-error)
                                 (list beg end face))
                           ws)))))))
    ("unused_import"
     (list (list beg end 'sweeprolog-unused-import)))
    ("undefined_import"
     (list (list beg end 'sweeprolog-undefined-import)))
    ("error"
     (list (list beg end 'sweeprolog-error)))
    ("keyword"
     (list (list beg end 'sweeprolog-keyword)))
    ("html_attribute"
     (list (list beg end 'sweeprolog-html-attribute)))
    ("html"
     (list (list beg end 'sweeprolog-html-call)))
    ("dict_tag"
     (list (list beg end 'sweeprolog-dict-tag)))
    ("dict_key"
     (list (list beg end 'sweeprolog-dict-key)))
    ("dict_sep"
     (list (list beg end 'sweeprolog-dict-sep)))
    ("dict_function"
     (list (list beg end 'sweeprolog-dict-function)))
    ("dict_return_op"
     (list (list beg end 'sweeprolog-dict-return-op)))
    ("func_dot"
     (list (list beg end 'sweeprolog-func-dot)))
    ("meta"
     (list (list beg end 'sweeprolog-meta-spec)))
    ("flag_name"
     (list (list beg end 'sweeprolog-flag-name)))
    ("no_flag_name"
     (list (list beg end 'sweeprolog-no-flag-name)))
    ("ext_quant"
     (list (list beg end 'sweeprolog-ext-quant)))
    ("atom"
     (list (list beg end 'sweeprolog-atom)))
    ("float"
     (list (list beg end 'sweeprolog-float)))
    ("rational"
     (list (list beg end 'sweeprolog-rational)))
    ("int"
     (list (list beg end 'sweeprolog-int)))
    ("singleton"
     (list (list beg end 'sweeprolog-singleton)))
    ("option_name"
     (list (list beg end 'sweeprolog-option-name)))
    ("no_option_name"
     (list (list beg end 'sweeprolog-no-option-name)))
    ("control"
     (list (list beg end 'sweeprolog-control)))
    ("var"
     (list (list beg end 'sweeprolog-variable)))
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
               (cons (list beg end 'sweeprolog-fullstop)
                     ws)))))
    ("functor"
     (list (list beg end 'sweeprolog-functor)))
    ("arity"
     (list (list beg end 'sweeprolog-arity)))
    ("predicate_indicator"
     (list (list beg end 'sweeprolog-predicate-indicator)))
    ("chars"
     (list (list beg end 'sweeprolog-chars)))
    ("codes"
     (list (list beg end 'sweeprolog-codes)))
    ("string"
     (list (list beg end 'sweeprolog-string)))
    (`("module" . ,_)
     (list (list beg end 'sweeprolog-module)))
    ("neck"
     (list (list beg end 'sweeprolog-neck)))
    (`("hook" . ,_)
     (list (list beg end 'sweeprolog-hook)))
    ("hook"
     (list (list beg end 'sweeprolog-hook)))
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
               (unless (derived-mode-p mode) (funcall mode))
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
         (list (list beg end 'sweeprolog-qq-content)))))
    ("qq_type"
     (list (list beg end 'sweeprolog-qq-type)))
    ("qq_sep"
     (list (list beg end 'sweeprolog-qq-sep)))
    ("qq_open"
     (list (list beg end 'sweeprolog-qq-open)))
    ("qq_close"
     (list (list beg end 'sweeprolog-qq-close)))
    ("identifier"
     (list (list beg end 'sweeprolog-identifier)))
    (`("file" . ,_)
     (list (list beg end 'sweeprolog-file)))
    (`("file_no_depend" . ,_)
     (list (list beg end 'sweeprolog-file-no-depend)))
    ("function"
     (list (list beg end 'sweeprolog-function)))
    ("no_function"
     (list (list beg end 'sweeprolog-no-function)))
    ("nofile"
     (list (list beg end 'sweeprolog-no-file)))
    ("op_type"
     (list (list beg end 'sweeprolog-op-type)))
    ("directive"
     (list (list beg end nil) (list beg end 'sweeprolog-directive)))
    ("body"
     (list (list beg end nil) (list beg end 'sweeprolog-body)))
    ("clause"
     (list (list beg end nil) (list beg end 'sweeprolog-clause)))
    ("term"
     (list (list beg end nil) (list beg end 'sweeprolog-term)))
    ("grammar_rule"
     (list (list beg end nil) (list beg end 'sweeprolog-grammar-rule)))
    ("method"
     (list (list beg end nil) (list beg end 'sweeprolog-method)))
    ("class"
     (list (list beg end 'sweeprolog-class)))
    (`("decl_option" . ,_)
     (list (list beg end 'sweeprolog-declaration-option)))
    (`("dcg" . "string")
     (list (list beg end 'sweeprolog-dcg-string)))
    ("delimiter"
     (list (list beg end 'sweeprolog-delimiter)))
    ("pragma"
     (list (list beg end 'sweeprolog-pragma)))
    ("chr_type"
     (list (list beg end 'sweeprolog-chr-type)))
    ("built_in"
     (list (list beg end 'sweeprolog-built-in)))))

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
    (sweeprolog--highlight-holes beg end))
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
                                       'sweeprolog-predicate-indicator))
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
    ("built_in" (format "Built-in predicate %s/%s"
                        functor arity))
    (`("imported" . ,file) (format "Predicate %s/%s imported from %s"
                                   functor arity file))
    (`("autoload" . ,file) (format "Predicate %s/%s autoloaded from %s"
                                   functor arity file))
    ("global" (format "Global predicate %s/%s"
                      functor arity))
    (`("global" . ,type) (format "%s global predicate %s/%s"
                                 type functor arity))
    ("undefined" (format "Undefined predicate %s/%s"
                         functor arity))
    ("thread_local" (format "Thread-local predicate %s/%s"
                            functor arity))
    ("dynamic" (format "Dynamic predicate %s/%s"
                       functor arity))
    ("multifile" (format "Multifile predicate %s/%s"
                         functor arity))
    ("expanded" (format "Expanded predicate %s/%s"
                        functor arity))
    (`("extern" ,module . ,_) (format "External predicate %s/%s from module %s"
                                      functor arity module))
    ("recursion" (format "Recursive call to predicate %s/%s"
                         functor arity))
    ("meta" (format "Meta predicate %s/%s"
                    functor arity))
    ("foreign" (format "Foreign predicate %s/%s"
                       functor arity))
    ("local" (format "Local predicate %s/%s"
                     functor arity))
    ("constraint" (format "Constraint %s/%s"
                          functor arity))
    ("deprecated" (format "Deprecated predicate %s/%s"
                          functor arity))
    ("not_callable" "Non-callable term")))

(defun sweeprolog--help-echo-for-macro (expansion)
  (format "Macro indicator, expands to (%s)" expansion))

(defun sweeprolog--help-echo-for-declaration-option (option)
  (pcase option
    ("incremental"
     (concat
      "Incremental declaration "
      "(tables that depend on this predicate are updated it changes)"))
    ("volatile"
     (concat
      "Volatile declaration "
      "(predicate excluded from saved program)"))
    (_ (format "%s predicate property declaration" option))))

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
         (`("macro" . ,expansion)
          (sweeprolog--help-echo-for-macro expansion))
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
         ("func_dot" "Dict function dot")
         (`("decl_option" . ,option)
          (sweeprolog--help-echo-for-declaration-option option))
         (`("dcg" . "string") "DCG terminal")
         ("delimiter" "Delimiter")
         ("pragma" "Pragma")
         ("chr_type" "CHR type")
         ("built_in" "Built-in")))
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
                                     (point))
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
                 (cons :note "Singleton variable, use \\[sweeprolog-replace-with-anonymous-variable] to replace with anonymous variable"))
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
    (without-restriction
      (let ((sweeprolog--analyze-point (point)))
        (sweeprolog-analyze-region (point-min) (point-max))))
    (setq sweeprolog--buffer-modified nil)))

(defun sweeprolog--buffer-string (filename)
  (when-let ((buf (find-buffer-visiting filename)))
    (with-current-buffer buf
      (without-restriction
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
      (let ((start (point)))
        (sweeprolog-end-of-top-term)
        (sweeprolog-analyze-region start (point) "true")))))

(defun sweeprolog-analyze-some-terms (beg end &optional verbose)
  (let ((sweeprolog--analyze-point (point)))
    (save-match-data
      (save-mark-and-excursion
        (goto-char beg)
        (sweeprolog-beginning-of-top-term)
        (unless (bobp)
          (sweeprolog-beginning-of-top-term))
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
          (when font-lock-keywords
            (font-lock-fontify-keywords-region start (point) verbose))
          `(jit-lock-bounds ,start . ,(point)))))))

(defconst sweeprolog-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (group-n 1 (one-or-more "\\")))
    (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
         (string-to-syntax "."))))
   ((rx (not alnum) (group-n 2 "0'" anychar))
    (2 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
         (string-to-syntax "w"))))))

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
                                              'sweeprolog-variable-at-point
                                              (current-buffer) nil))))))))))

(defun sweeprolog-cursor-sensor-functions (var)
  (list
   (lambda (_win old dir)
     (if (eq dir 'entered)
         (sweeprolog-highlight-variable (point) var)
       (sweeprolog-highlight-variable old)))))


;;;; Flymake

(defun sweeprolog-diagnostic-function (report-fn &rest _)
  (setq sweeprolog--diagnostics nil
        sweeprolog--diagnostics-report-fn report-fn))

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

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

;;;; Top-level

(defvar sweeprolog-top-level-output-filter nil)

(defun sweeprolog-colourise-query (change-beg change-end &rest _)
  (unless sweeprolog-top-level-output-filter
    (let ((beg (cdr comint-last-prompt))
          (end (point-max)))
      (when (and beg (<= beg change-beg change-end end))
        (with-silent-modifications
          (font-lock-unfontify-region beg end))
        (sweeprolog--query-once "sweep" "sweep_colourise_query"
                                (cons (buffer-substring-no-properties beg end)
                                      (marker-position beg)))
        (when sweeprolog-highlight-holes
          (sweeprolog--highlight-holes beg end))))))

(defun sweeprolog-top-level-sentinel (proc msg)
  "Sentinel for Prolog top-level processes.

Calls `comint-write-input-ring' to update the top-level's
persistent history before calling the default process sentinel
function with PROC and MSG."
  (comint-write-input-ring)
  (let ((sweeprolog-top-level-output-filter t))
    (internal-default-process-sentinel proc msg)))

(defun sweeprolog-top-level-maybe-delete-process ()
  (let ((process (get-buffer-process (current-buffer))))
    (or (not process)
        (not (memq (process-status process) '(run stop open listen)))
        (and (yes-or-no-p
              (format "Buffer %S has a running top-level; kill it? "
                      (buffer-name (current-buffer))))
             (prog1 t
               (sweeprolog-top-level-delete-process))))))

(defun sweeprolog-top-level-delete-process (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (when sweeprolog-top-level-thread-id
    (sweeprolog--query-once "sweep" "sweep_kill_thread"
                            sweeprolog-top-level-thread-id))
  (while (process-live-p (get-buffer-process buffer))
    (process-send-eof (get-buffer-process buffer))
    (accept-process-output (get-buffer-process buffer) 1))
  (setq sweeprolog-top-level-thread-id nil))

(defun sweeprolog-top-level-setup-history (buf)
  "Setup `comint-input-ring-file-name' for top-level buffer BUF."
  (with-current-buffer buf
    (setq-local comint-input-ring-file-name
                (pcase sweeprolog-top-level-persistent-history
                  ((pred stringp)
                   sweeprolog-top-level-persistent-history)
                  ((pred functionp)
                   (funcall sweeprolog-top-level-persistent-history))
                  (`(project . ,rel-def)
                   (if-let ((project (project-current)))
                       (expand-file-name (car rel-def)
                                         (project-root project))
                     (cadr rel-def)))))
    (comint-read-input-ring t)
    (set-process-sentinel (get-buffer-process buf)
                          #'sweeprolog-top-level-sentinel)
    (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)))

(defun sweeprolog-top-level-filter (process string)
  (let ((sweeprolog-top-level-output-filter t))
    (comint-output-filter process string))
  (when (string-match (rx "Sweep top-level thread exited") string)
    (with-current-buffer (process-buffer process)
      (setq sweeprolog-top-level-thread-id nil))
    (if (or (eq (process-type process) 'network)
            (not (daemonp)))
        (delete-process process)
      (sweeprolog--query-once "sweep" "sweep_nohup" 1)
      (delete-process process)
      (sweeprolog--query-once "sweep" "sweep_nohup" 0))))

(defun sweeprolog-top-level-buffer (&optional name)
  "Return a Prolog top-level buffer named NAME.

If NAME is nil, use the default name \"*sweeprolog-top-level*\".

If the buffer already exists, ensure it is associated with a live
top-level."
  (let ((buf (get-buffer-create (or name "*sweeprolog-top-level*"))))
    (unless (process-live-p (get-buffer-process buf))
      (with-current-buffer buf
        (unless (derived-mode-p 'sweeprolog-top-level-mode)
          (sweeprolog-top-level-mode))
        (setq sweeprolog-top-level-thread-id
              (if sweeprolog-top-level-use-pty
                  (progn
                    (make-comint-in-buffer "sweeprolog-top-level" buf nil)
                    (let* ((proc (get-buffer-process buf))
                           (tty (process-tty-name proc)))
                      (process-send-eof proc)
                      (prog1 (sweeprolog--query-once
                              "sweep" "sweep_top_level_start_pty" tty)
                        (unless comint-last-prompt buf
                                (accept-process-output proc 1))
                        (when (eq system-type 'gnu/linux)
                          ;; make sure the pty does not echo input
                          (call-process "stty" nil nil nil "-F" tty "-echo")))))
                (unless sweeprolog-prolog-server-port
                  (sweeprolog-start-prolog-server))
                (make-comint-in-buffer "sweeprolog-top-level"
                                       buf
                                       (cons "localhost"
                                             sweeprolog-prolog-server-port))
                (sweeprolog--query-once "sweep" "sweep_accept_top_level_client" nil)))
        (let ((proc (get-buffer-process buf)))
          (set-process-filter proc #'sweeprolog-top-level-filter)
          (unless comint-last-prompt buf (accept-process-output proc 1))
          (set-process-query-on-exit-flag proc nil)
          (setq-local comint-input-ring-file-name
                      (pcase sweeprolog-top-level-persistent-history
                        ((pred stringp)
                         sweeprolog-top-level-persistent-history)
                        ((pred functionp)
                         (funcall sweeprolog-top-level-persistent-history))
                        (`(project . ,rel-def)
                         (if-let ((project (project-current)))
                             (expand-file-name (car rel-def)
                                               (project-root project))
                           (cadr rel-def)))))
          (comint-read-input-ring t)
          (set-process-sentinel proc #'sweeprolog-top-level-sentinel)
          (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)
          (add-hook 'kill-buffer-query-functions #'sweeprolog-top-level-maybe-delete-process nil t)
          )))
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
                           (if (and (derived-mode-p 'sweeprolog-top-level-mode)
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
  (sweeprolog-signal-thread sweeprolog-top-level-thread-id goal))

(defun sweeprolog-top-level-completion-at-point ()
  "Call `sweeprolog-completion-at-point' while narrowing to current query."
  (when-let ((prompt-beg (car comint-last-prompt)))
    (with-restriction prompt-beg (point-max)
      (sweeprolog-completion-at-point))))

(defun sweeprolog--fill-query-holes (_)
  "Turn all holes in the last top-level query to plain variables."
  (when-let ((beg (cdr comint-last-prompt)))
    (sweeprolog-fill-holes beg (point-max))))

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
              comint-highlight-input nil
              comment-start "%"
              forward-sexp-function #'sweeprolog-forward-sexp-function
              beginning-of-defun-function #'sweeprolog-beginning-of-top-term
              end-of-defun-function #'sweeprolog-end-of-top-term
              syntax-propertize-function sweeprolog-syntax-propertize-function)
  (add-hook 'post-self-insert-hook #'sweeprolog-top-level--post-self-insert-function nil t)
  (add-hook 'completion-at-point-functions #'sweeprolog-top-level-completion-at-point nil t)
  (add-hook 'after-change-functions #'sweeprolog-colourise-query nil t)
  (add-hook 'xref-backend-functions #'sweeprolog--xref-backend nil t)
  (add-hook 'comint-input-filter-functions #'sweeprolog--fill-query-holes nil t)
  (when (fboundp 'eldoc-documentation-default)
    (setq-local eldoc-documentation-strategy #'eldoc-documentation-default))
  (add-hook 'eldoc-documentation-functions #'sweeprolog-predicate-modes-doc nil t)
  (unless (member 'sweeprolog-hole yank-excluded-properties)
    (setq-local yank-excluded-properties
                (cons 'sweeprolog-hole yank-excluded-properties)))
  (when (and (member sweeprolog-faces-style '(light dark))
             (not (custom-theme-enabled-p 'sweeprolog-pce)))
    (load-theme 'sweeprolog-pce t)
    (setq sweeprolog-faces-style nil)))

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
                         (derived-mode-p 'sweeprolog-mode))
                    (current-buffer)
                  (read-buffer "Load buffer: "
                               (when (derived-mode-p 'sweeprolog-mode)
                                 (buffer-name))
                               t
                               (lambda (b)
                                 (let ((n (or (and (consp b) (car b)) b)))
                                   (with-current-buffer n
                                     (derived-mode-p 'sweeprolog-mode))))))))
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

(defun sweeprolog-beginning-of-top-term-once ()
  "Move back from point to the beginning of a top term.

When there is no top term that begins before point, move to the
beginning of the buffer and return nil.  Otherwise, return
non-nil."
  (let ((go t)
        (result nil)
        (start nil))
    (while go
      (if (re-search-backward (rx bol graph) nil t)
          (cond
           ((setq start (nth 8 (syntax-ppss))) (goto-char start))
           ((looking-at (rx (or "%" "/*"))))
           ((setq go nil result t)))
        (goto-char (point-min))
        (setq go nil)))
    result))

(defun sweeprolog-beginning-of-top-term (&optional arg)
  "Move to the beginning of the ARGth term before point.

If ARG is negative, Move to the beginning of the ARGth term after
point instead.

The command `beginning-of-defun' calls this function in
`sweeprolog-mode' buffers (see `beginning-of-defun-function')."
  (let ((times (or arg 1))
        (result nil))
    (if (> 0 times)
        (sweeprolog-beginning-of-next-top-term (- times))
      (while (and (< 0 times) (not (bobp)))
        (setq times (1- times)
              result (sweeprolog-beginning-of-top-term-once)))
      result)))

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
                      (equal 0 (sweeprolog-syntax-class-at (point)))
                      (save-excursion
                        (nth 8 (syntax-ppss (max (point-min)
                                                 (1- (point))))))
                      (save-match-data
                        (looking-back (rx (or "#" "$" "&" "*" "+" "-"
                                              "." "/" ":" "<" "=" ">"
                                              "?" "@" "\\" "^" "~")
                                          "." (or white "\n"))
                                      (pos-bol))))
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

(defun sweeprolog--highlight-holes (beg end)
  "Add the `sweeprolog-hole' face to all holes between BEG and END."
  (with-silent-modifications
    (save-excursion
      (goto-char beg)
      (with-restriction beg end
        (while-let ((hole (sweeprolog--next-hole))
                    (hbeg (car hole)) (hend (cdr hole)))
          (font-lock--add-text-property hbeg hend
                                        'font-lock-face
                                        'sweeprolog-hole
                                        (current-buffer)
                                        nil)
          (goto-char hend))))))

(defun sweeprolog-fill-holes (beg end)
  "Turn all holes from BEG to END into plain variables."
  (interactive "r" sweeprolog-mode sweeprolog-top-level-mode)
  (with-silent-modifications
    (save-excursion
      (goto-char beg)
      (with-restriction beg end
        (while-let ((hole (sweeprolog--next-hole))
                    (hbeg (car hole)) (hend (cdr hole)))
          (font-lock--remove-face-from-text-property
           hbeg hend 'font-lock-face 'sweeprolog-hole)
          (remove-list-of-text-properties hbeg hend '(sweeprolog-hole))
          (goto-char hend))))))

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
                    (member (sweeprolog-syntax-class-at (1+ obeg))
                            '(0 12)))
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
                        'font-lock-face (list 'sweeprolog-hole)
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
  (let* ((point (point))
         (neck (or neck ":-"))
         (mod (or module (sweeprolog-buffer-module)))
         (head-format (sweeprolog--query-once "sweep" "sweep_format_head"
                                              (list mod
                                                    functor
                                                    arity
                                                    (if (string= neck "-->") 2 0)))))
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
                  'font-lock-face (list 'sweeprolog-hole)
                  'rear-nonsticky '(sweeprolog-hole
                                    cursor-sensor-functions
                                    font-lock-face)))))))))
      (insert " " neck " " (sweeprolog--hole "Body") ".\n"))
    (goto-char point)
    (sweeprolog-forward-hole)))

(defun sweeprolog-maybe-insert-next-clause (point _arg)
  (let* ((bounds (sweeprolog-last-token-boundaries point))
         (kind (car bounds))
         (beg  (cadr bounds))
         (end  (caddr bounds)))
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
                                  arity
                                  neck
                                  module)
        t))))

(defun sweeprolog-default-new-predicate-location (&rest _)
  (sweeprolog-end-of-predicate-at-point)
  (end-of-line))

(defun sweeprolog-new-predicate-location-above-current (&rest _)
  (sweeprolog-beginning-of-predicate-at-point)
  (let ((last (or (caddr (sweeprolog-last-token-boundaries))
                  (point-min))))
    (while (re-search-backward (rx bol "%" (or "%" "!")) last t))))

(defun sweeprolog-maybe-define-predicate (point _arg)
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
                                arity
                                neck)
      t)))

(defun sweeprolog-replace-with-anonymous-variable (&optional point)
  "Replace smallest Prolog term at POINT with an anonymous variable `_'."
  (interactive "d" sweeprolog-mode)
  (let (b e)
    (sweeprolog-analyze-term-at-point
     (lambda (beg end _arg)
       (when (<= (or b (point-min)) beg point (1+ point) end (or e (point-max)))
         (setq b beg e end))))
    (unless b (user-error "No term at this position"))
    (combine-after-change-calls
      (delete-region b e)
      (save-excursion
        (goto-char b)
        (insert "_")))))

(defun sweeprolog-insert-term-dwim (&optional point arg)
  "Insert an appropriate Prolog term at POINT.

Call the functions in `sweeprolog-insert-term-functions' one
after the other, with two arguments POINT and ARG, until one of
them returns non-nil.

Interactively, POINT is point and ARG is the prefix argument."
  (interactive "d\nP" sweeprolog-mode)
  (setq point (or point (point)))
  (unless (run-hook-with-args-until-success
           'sweeprolog-insert-term-functions point arg)
    (user-error "No term insertion function applies here")))

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
  "Get information about the Prolog definition at POINT.

If POINT is nil, it defaults to point.

If POINT is not inside a clause, return nil.  Otherwise, return a
list (BEG FUNCTOR ARITY END NECK MODULE).

When the head term of clause is module-qualified, MODULE is the
name of that module.  If the head is not module-qualified, MODULE
is nil.  FUNCTOR and ARITY are the functor and arity of predicate
that the clause is part of, and NECK is the neck operator that it
uses (e.g. \"-->\" for DCG rules).

BEG and END are buffer positions corresponding to the beginning
and end positions of the clause."
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
              (ignore-errors (sweeprolog-forward-predicate) t))
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

If there is no predicate definition at or directly above point,
return nil.  Otherwise, return a list (MOD FUN ARI NECK), where
MOD, FUN, ARI and NECK have the same meaning as in
`sweeprolog-definition-at-point'."
  (setq point (or point (point)))
  (goto-char point)
  (unless (sweeprolog-at-beginning-of-top-term-p)
    (sweeprolog-beginning-of-top-term))
  (when-let ((def (sweeprolog-definition-at-point)))
    (let ((start (point))
          (go t))
      (while (and go (sweeprolog-beginning-of-top-term-once))
        (let ((ndef (sweeprolog-definition-at-point)))
          (if (and (string= (nth 5 def)
                            (nth 5 ndef))
                   (string= (nth 1 def)
                            (nth 1 ndef))
                   (= (nth 2 def)
                      (nth 2 ndef)))
              (setq start (point)
                    def ndef)
            (setq go nil)
            (goto-char start)))))
    (list (nth 5 def) (nth 1 def) (nth 2 def) (nth 4 def))))

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
                'font-lock-face (list 'sweeprolog-hole)
                'rear-nonsticky '(sweeprolog-hole
                                  cursor-sensor-functions
                                  font-lock-face))
               term))))))
      term)))

(defun sweeprolog-read-predicate-documentation-with-holes
    (module functor arity neck)
  "Use holes for documentation of predicate MODULE:FUNCTOR/ARITY.
NECK is the neck operator the this predicate uses."
  (list module
        (concat (sweeprolog-format-term-with-holes
                 functor
                 (- arity (if (string= neck "-->") 2 0)))
                (if (string= neck "-->") "//" ""))
        (sweeprolog--hole "Det")
        nil))

(defun sweeprolog-read-predicate-documentation-default-function
    (module functor arity neck)
  "Prompt for documentation of the predicate MODULE:FUNCTOR/ARITY.
NECK is the neck operator the this predicate uses."
  (let ((cur 1)
        (ari (- arity (if (string= neck "-->") 2 0)))
        (arguments nil))
    (while (<= cur ari)
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
      (list module
            (concat (sweeprolog-format-string-as-atom functor)
                    (if arguments
                        (concat "("
                                (mapconcat #'identity arguments ", ")
                                ")")
                      "")
                    (if (string= neck "-->") "//" ""))
            det
            (and (not (string-empty-p summary))
                 summary)))))

(defun sweeprolog-insert-predicate-documentation (module head det sum)
  (insert
   (concat "%!  " (when module (concat module ":")) head " is " det "."
           "\n"
           (when sum (concat "%\n%   " sum "\n"))
           "\n"))
  (forward-char -2)
  (fill-paragraph t))

(defun sweeprolog-read-predicate-documentation (mod fun ari neck)
  "Return information for initial documentation of MOD:FUN/ARI.

NECK is the neck operator the this predicate uses.

Calls the function specified by
`sweeprolog-read-predicate-documentation-function' to do the
work."
  (funcall sweeprolog-read-predicate-documentation-function
           mod fun ari neck))

(defun sweeprolog-document-predicate-at-point (point)
  "Insert documentation comment for the predicate at or above POINT."
  (interactive "d" sweeprolog-mode)
  (pcase (sweeprolog-beginning-of-predicate-at-point point)
    (`(,mod ,fun ,ari ,neck)
     (apply #'sweeprolog-insert-predicate-documentation
            (sweeprolog-read-predicate-documentation mod fun ari neck)))
    (_ (user-error "No predicate found at point"))))

(defsubst sweeprolog--op-p (beg end)
  "Check if there is an operator between BEG and END in the current buffer."
  (sweeprolog--query-once
   "sweep" "sweep_op_info"
   (cons (buffer-substring-no-properties beg end) (buffer-file-name))))

(defun sweeprolog-next-token-boundaries (&optional pos)
  "Return a list (KIND BEG END) describing the Prolog token after POS, if any.

KIND is one of `symbol', `functor' `string', `operator', `open',
`close' and `else'.  BEG and END are the token boundaries.

If there is no token after POS, return nil."
  (let ((point (or pos (point))))
    (save-excursion
      (goto-char point)
      (while (forward-comment 1))
      (unless (eobp)
        (let ((beg (point))
              (syn (sweeprolog-syntax-class-at (point))))
          (cond
           ((member syn '(0 12))
            (skip-syntax-forward "w_")
            (if (= (sweeprolog-syntax-class-at (point)) 4)
                (progn
                  (forward-char)
                  (list 'functor beg (point)))
              (list 'symbol beg (point))))
           ((= syn 7)
            (unless (nth 8 (syntax-ppss))
              (forward-char)
              (while (and (not (eobp)) (nth 3 (syntax-ppss)))
                (forward-char))
              (list 'string beg (point))))
           ((member syn  '(1 9))
            (skip-syntax-forward ".")
            (let ((end (point)))
              (while (and (< beg (point))
                          (not (sweeprolog--op-p beg (point))))
                (forward-char -1))
              (list 'operator beg (if (= beg (point)) end (point)))))
           ((= syn 4)
            (list 'open beg (1+ beg)))
           ((= syn 5)
            (list 'close beg (1+ beg)))
           ((= syn 12) nil)
           (t (list 'else beg (1+ beg)))))))))

(defun sweeprolog-last-token-boundaries (&optional pos)
  "Return a list (KIND BEG END) describing the Prolog token before POS, if any.

KIND is one of `symbol', `functor' `string', `operator', `open',
`close' and `else'.  BEG and END are the token boundaries.

If there is no token before POS, return nil."
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
              (syn (sweeprolog-syntax-class-at (point))))
          (cond
           ((member syn '(2 3))
            (skip-syntax-backward "w_")
            (list 'symbol (point) end))
           ((= syn 7)
            (when-let ((beg (nth 8 (syntax-ppss))))
              (list 'string beg end)))
           ((and (= syn 4)
                 (member (sweeprolog-syntax-class-at (1- (point))) '(2 3)))
            (skip-syntax-backward "w_")
            (list 'functor (point) end))
           ((member syn '(1 9))
            (skip-syntax-backward ".")
            (let ((beg (point)))
              (while (and (< (point) end)
                          (not (sweeprolog--op-p (point) end)))
                (forward-char 1))
              (list 'operator (if (= end (point)) beg (point)) end)))
           ((= syn 4)
            (list 'open (1- end) end))
           ((= syn 5)
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
              (let ((sa (sweeprolog-syntax-class-at (1+ obeg))))
                (or (null sa) (member sa '(0 12)))))
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
                     (list (format "Cannot scan backwards beyond infix operator of higher precedence %s." opre)
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
                      (member (sweeprolog-syntax-class-at (1+ obeg))
                              '(0 12))))
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
         (when (member (sweeprolog-syntax-class-at (1- (point))) '(2 3))
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
        (prec (or (pcase (sweeprolog-last-token-boundaries)
                    (`(,(or 'operator 'symbol) ,obeg ,oend)
                     (when-let ((pprec
                                 (sweeprolog-op-infix-precedence
                                  (buffer-substring-no-properties obeg oend))))
                       (goto-char obeg)
                       (1- pprec))))
                  0)))
    (condition-case error
        (sweeprolog--backward-term prec)
      (scan-error (when (= point (point))
                    (signal 'scan-error (cdr error)))))))

(defun sweeprolog--forward-sexp ()
  (let ((point (point))
        (prec (or (pcase (sweeprolog-next-token-boundaries)
                    (`(,(or 'operator 'symbol) ,obeg ,oend)
                     (when-let ((pprec
                                 (sweeprolog-op-infix-precedence
                                  (buffer-substring-no-properties obeg oend))))
                       (goto-char oend)
                       (1- pprec))))
                  0)))
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

(defun sweeprolog-local-predicate-export-comment (fun ari ind)
  (sweeprolog--query-once "sweep" "sweep_local_predicate_export_comment"
                          (list (buffer-file-name) fun ari ind)))

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
                        (when-let ((def  (sweeprolog-definition-at-point))
                                   (fun  (nth 1 def))
                                   (ari  (nth 2 def))
                                   (neck (nth 4 def))
                                   (ind  "/"))
                          (unless (nth 5 def)
                            (when (string= neck "-->")
                              (setq ari (- ari 2)
                                    ind "//"))
                            (list (concat fun ind (number-to-string ari))
                                  (sweeprolog-local-predicate-export-comment fun ari ind)))))
                   (list
                    (sweeprolog-read-exportable-predicate)
                    (read-string "Export comment: ")))
               sweeprolog-mode)
  (without-restriction
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
              (let ((pos (- (point-max) (pos-eol))))
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
  (let ((bol (pos-bol)))
    (if (nth 4 (syntax-ppss))
        (combine-after-change-calls
          (delete-horizontal-space)
          (let* ((lend (point))
                 (lbeg (save-excursion
                         (while (and (< bol (point))
                                     (not
                                      (= (sweeprolog-syntax-class-at (1- (point)))
                                         0)))
                           (forward-char -1))
                         (point)))
                 (num (- 4 (% (- lend lbeg) 4))))
            (insert (make-string (if (< 0 num)
                                     num
                                   4)
                                 ?\s)))))
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
      (when (member (buffer-substring-no-properties (pos-bol)
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
                      (integerp (sweeprolog-context-callable-p)))
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
  (setq-local syntax-propertize-function sweeprolog-syntax-propertize-function)
  (setq-local indent-line-function #'sweeprolog-indent-line)
  (setq-local adaptive-fill-regexp "[ \t]*")
  (setq-local fill-indent-according-to-mode t)
  (setq-local comment-multi-line t)
  (setq-local add-log-current-defun-function
              #'sweeprolog-add-log-current-defun)
  (setq-local prettify-symbols-alist '((":-" . ?←)))
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
  (add-hook 'completion-at-point-functions #'sweeprolog-completion-at-point nil t)
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
                  (cancel-timer sweeprolog--timer)))
              nil t))
  (when sweeprolog-enable-cursor-sensor
    (add-hook 'sweeprolog-analyze-region-fragment-hook
              #'sweeprolog-analyze-fragment-variable nil t)
    (cursor-sensor-mode 1))
  (when (boundp 'context-menu-functions)
    (add-hook 'context-menu-functions
              #'sweeprolog-context-menu-function nil t))
  (unless (member 'sweeprolog-hole yank-excluded-properties)
    (setq-local yank-excluded-properties
                (cons 'sweeprolog-hole yank-excluded-properties)))
  (when (and (member sweeprolog-faces-style '(light dark))
             (not (custom-theme-enabled-p 'sweeprolog-pce)))
    (load-theme 'sweeprolog-pce t)
    (setq sweeprolog-faces-style nil)))


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
     (read-string (format-prompt "Test set name" def)
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
              (let ((prefix (buffer-substring (pos-bol)
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
          (line-beg (pos-bol)))
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
  "Indent the current line in a Sweep Prolog mode buffer."
  (interactive)
  (let ((pos (- (point-max) (point))))
    (back-to-indentation)
    (let ((column (if-let ((ppss (syntax-ppss))
                           (open (nth 8 ppss)))
                      ;; Inside a comment or a string.
                      (if (nth 4 ppss)
                          ;; It's a comment.  Indent like
                          ;; `indent--default-inside-comment'.
                          (save-excursion
                            (forward-line -1)
                            (skip-chars-forward " \t")
                            (when (< (1- (point)) open (line-end-position))
                              (goto-char open)
                              (when (looking-at comment-start-skip)
                                (goto-char (match-end 0))))
                            (current-column))
                        ;; It's a string.  Don't indent.
                        'noindent)
                    (if-let ((open (and (not (eobp))
                                        (= (sweeprolog-syntax-class-at (point)) 5)
                                        (nth 1 (syntax-ppss)))))
                        (save-excursion
                          (goto-char open)
                          (when (member (sweeprolog-syntax-class-at (1- (point)))
                                        '(2 3))
                            (when (save-excursion
                                    (forward-char)
                                    (skip-syntax-forward " " (pos-eol))
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
               (cons car (pos-bol))))
           (sweeprolog--query-once "sweep" "sweep_imenu_index"
                                   (buffer-file-name))))


;;;; ElDoc

(defun sweeprolog-predicate-modes-doc (cb)
  "Call CB with the documentation of the predicate at point, if any."
  (when-let ((clause-beg (save-excursion
                           (unless (sweeprolog-at-beginning-of-top-term-p)
                             (sweeprolog-beginning-of-top-term))
                           (point)))
             (clause-end (save-excursion
                           (sweeprolog-end-of-top-term)
                           (point)))
             (clause-str (buffer-substring-no-properties clause-beg
                                                         clause-end)))
    (pcase (sweeprolog--query-once
            "sweep" "sweep_short_documentation"
            (list clause-str (- (point) clause-beg) buffer-file-name))
      (`(,pi ,doc ,span)
       (when span
         (add-face-text-property (car span) (cdr span)
                                 'sweeprolog-eldoc-argument-highlight nil doc))
       (funcall cb doc :thing pi :face 'sweeprolog-predicate-indicator)))))

;;;; Top-level Menu

(defun sweeprolog-top-level-menu--entries ()
  (mapcar (lambda (th)
            (let ((id (nth 0 th))
                  (bn (nth 1 th))
                  (st (nth 2 th))
                  (sz (number-to-string (nth 3 th)))
                  (ct (number-to-string (nth 4 th))))
              (list id (vector bn st sz ct))))
          (sweeprolog--query-once
           "sweep" "sweep_list_threads"
           (delq nil
                 (mapcar (lambda (buffer)
                           (when-let
                               ((thread
                                 (buffer-local-value 'sweeprolog-top-level-thread-id
                                                     buffer)))
                             (cons (buffer-name buffer) thread)))
                         (buffer-list))))))

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
  "Kill the buffer of to the Sweep Top-level Menu entry at point."
  (interactive "" sweeprolog-top-level-menu-mode)
  (if-let ((vec (tabulated-list-get-entry)))
      (let ((bn (seq-elt vec 0)))
        (kill-buffer bn))
    (user-error "No top-level menu entry here")))

(defun sweeprolog-top-level-menu-go-to ()
  "Go to the buffer of to the Sweep Top-level Menu entry at point."
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
  "Major mode for browsing a list of active Sweep top-levels."
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
  (let ((buf (get-buffer-create "*Sweep Top-levels*")))
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
        (buttonize-region start
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
                (buttonize-region start
                                  (point)
                                  #'find-file
                                  (concat "library(" base ")")))))
           ((string= path "/pldoc/man")
            (pcase (url-parse-query-string query)
              (`(("predicate" ,pred))
               (buttonize-region start
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
                  (insert (buttonize mod #'sweeprolog-find-module mod)
                          " is a SWI-Prolog module.\n\n"
                          page)
                (insert (buttonize mod #'sweeprolog-find-module mod)
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
                   (insert (buttonize pred #'sweeprolog-find-predicate pred)
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
  (interactive (list (sweeprolog-read-predicate "Describe predicate")))
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

(defun sweeprolog-terms-at-point (&optional point)
  "Return boundaries of Prolog terms at POINT, innermost first."
  (setq point (or point (point)))
  (save-excursion
    (goto-char point)
    (unless (sweeprolog-at-beginning-of-top-term-p)
      (sweeprolog-beginning-of-top-term))
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
  (when (or (derived-mode-p 'sweeprolog-mode)
            (derived-mode-p 'sweeprolog-top-level-mode))
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

(defun sweeprolog-read-term (&optional prompt initial)
  "Prompt for a Prolog term with PROMPT (defaults to \"?- \").

If INITIAL is non-nil, use it as the initial contents of the
minibuffer."
  (setq prompt (or prompt "?- "))
  (read-from-minibuffer prompt initial
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
        (add-hook 'completion-at-point-functions #'sweeprolog-completion-at-point nil t))
    (read-from-minibuffer prompt nil
                          sweeprolog-read-goal-map nil
                          'sweeprolog-read-goal-history
                          (when (derived-mode-p 'sweeprolog-mode)
                            (sweeprolog-goals-at-point)))))

(defun sweeprolog-term-replace-edits (file template replacement condition classes)
  (let ((state (mapcar (lambda (class)
                         (pcase class
                           ('goal "goal(_)")
                           (_ (symbol-name class))))
                       classes)))
    (mapcar
     (pcase-lambda (`(compound "replace" ,beg ,end ,rep))
       (list (1+ beg) (1+ end) rep))
     (without-restriction
       (sweeprolog--query-once "sweep" "sweep_term_replace"
                               (list file
                                     sweeprolog-indent-offset
                                     template
                                     condition
                                     state
                                     replacement))))))

;;;###autoload
(defun sweeprolog-term-search (template &optional backward condition class)
  "Search for terms matching TEMPLATE.

If BACKWARD is non-nil, search backward from point, otherwise
search forward.

CONDITION is a Prolog goal that this commands runs for each
matching term.  If the goal fails this command disregards the
corresponding match.  CONDITION can share variables with
TEMPLATE, in which case this commands unifies these sharing
variables with the corresponding subterms of the matching term.
If CONDITION is omitted or nil, it defaults to \"true\".

CLASS is the class of terms to target, it can be one of `clause',
`head', `goal', `data' and `_'.  `clause' only matches whole
clauses, `head' only matches head terms, `goal' only matches goal
terms, `data' only matches data terms, and `_' matches any term.
CLASS can also be a list of one or more of these symbols, in
which a term matches if it matches any of the classes in CLASS.
If CLASS is omitted or nil, it defaults to `_'.

Interactively, prompt for TEMPLATE.  With a prefix argument
\\[universal-argument], prompt for CONDITION.  With a double
prefix argument \\[universal-argument] \\[universal-argument],
prompt for CLASS as well.  A negative prefix argument
\\[negative-argument] searches backward from point."
  (interactive (let* ((template (sweeprolog-read-term "[Search] ?- "))
                      (condition (when (member current-prefix-arg
                                               '((4)  (-4)
                                                 (16) (-16)))
                                   (sweeprolog-read-goal
                                    (concat "[Condition for matching "
                                            template
                                            "] ?- "))))
                      (class (when (member current-prefix-arg
                                           '((16) (-16)))
                               (mapcar #'intern
                                       (completing-read-multiple
                                        (format-prompt "Replace terms of class" "_")
                                        '("clause" "head" "goal" "data" "_")
                                        nil t nil nil "_")))))
                 (list template
                       (and current-prefix-arg
                            (< (prefix-numeric-value current-prefix-arg) 0))
                       condition class))
               sweeprolog-mode)
  (setq condition (or condition           "true")
        class     (or (ensure-list class) '(_)))
  (let* ((items
          (seq-filter
           (pcase-lambda (`(,beg ,end ,_))
             (<= (point-min) beg end (point-max)))
           (sweeprolog-term-replace-edits (buffer-file-name) template
                                          "true" condition class)))
         (groups
          (seq-group-by (pcase-lambda (`(,beg ,_ ,_)) (< beg (point)))
                        items))
         (after-point (alist-get nil groups))
         (before-point (alist-get t groups))
         (overlays
          (mapcar
           (pcase-lambda (`(,beg ,end ,_))
             (let ((overlay (make-overlay beg end)))
               (overlay-put overlay 'face 'sweeprolog-term-search-match)
               overlay))
           (append after-point before-point)))
         (length (length items))
         (index (if backward (1- length) 0)))
    (push-mark (point) t)
    (when backward
      (setq overlays (reverse overlays)))
    (let ((go t)
          (inhibit-quit t)
          (query-replace-map
           (let ((map (make-sparse-keymap)))
             (define-key map "\C-l" 'recenter)
             (define-key map "\C-v" 'scroll-up)
             (define-key map "\M-v" 'scroll-down)
             (define-key map [next] 'scroll-up)
             (define-key map [prior] 'scroll-down)
             (define-key map [?\C-\M-v] 'scroll-other-window)
             (define-key map [M-next] 'scroll-other-window)
             (define-key map [?\C-\M-\S-v] 'scroll-other-window-down)
             (define-key map [M-prior] 'scroll-other-window-down)
             map)))
      (if overlays
          (while go
            (unless
                (with-local-quit
                  (let* ((overlay (car overlays))
                         (pos (if backward
                                  (overlay-start overlay)
                                (overlay-end overlay))))
                    (overlay-put overlay 'priority 100)
                    (overlay-put overlay 'face 'sweeprolog-term-search-current)
                    (goto-char pos)
                    (pcase
                        (car (read-multiple-choice
                              ;; TODO - maybe indicate a wrapped
                              ;; search like Isearch does?
                              (format "Match %d/%d"
                                      (1+ index)
                                      length)
                              '((?  "next" "Next match")
                                (?  "back" "Last match")
                                (?  "exit" "Exit term search"))))
                      (?
                       (setq overlays (if backward
                                          (cons overlay
                                                (reverse (cdr overlays)))
                                        (append (cdr overlays)
                                                (list overlay)))
                             index (if backward
                                       index
                                     (mod (1+ index) length))
                             backward nil))
                      (?
                       (setq overlays (if backward
                                          (append (cdr overlays)
                                                  (list overlay))
                                        (cons overlay
                                              (reverse (cdr overlays))))
                             index (if backward
                                       (mod (1- index) length)
                                     index)
                             backward t))
                      (?
                       (setq go nil)
                       t))
                    (overlay-put overlay 'priority nil)
                    (overlay-put overlay 'face 'sweeprolog-term-search-match)))
              (goto-char (mark t))
              (pop-mark)
              (setq go nil)))
        (message "No matching term found."))
      (mapc #'delete-overlay overlays))))

(defvar sweeprolog-query-replace-term-include-match-function #'always
  "Function used to filter matching terms for `sweeprolog-query-replace-term'.

This function is called with one argument, a list (BEG END REP),
describing a term matching the current execution of
`sweeprolog-query-replace-term'.  BEG and END are the buffer
positions of the beginning and end of the matching term, and REP
is the replacement string for that match.  If this function
returns nil, the corresponding match is filtered out and
`sweeprolog-query-replace-term' does not suggest replacing it.

`sweeprolog-extract-region-to-predicate' let-binds this variable
when it calls `sweeprolog-query-replace-term' to replace other
occurrences of the extracted goal in the current buffer, in order
to filter out the body of the clause that it just created.")

;;;###autoload
(defun sweeprolog-query-replace-term (template replacement &optional condition class)
  "Replace some terms after point matching TEMPLATE with REPLACEMENT.

When the region is active, replace matching terms in region.

Query before performing each replacement.

Matching terms are those that the Prolog term TEMPLATE (given as
a string) subsumes.  REPLACEMENT is a Prolog term to insert in
place of matching terms.  REPLACEMENT can share variables with
TEMPLATE, in which case this commands unifies these sharing
variables with the corresponding subterms of the matching term.

CONDITION is a Prolog goal that this commands runs for each
matching term.  If the goal fails this command disregards the
corresponding match and does not suggest replacing it.  CONDITION
can share variables with TEMPLATE, similarly to REPLACEMENT.  If
CONDITION is omitted or nil, it defaults to \"true\".

CLASS is the class of terms to target, it can be one of `clause',
`head', `goal', `data' and `_'.  `clause' only matches whole
clauses, `head' only matches head terms, `goal' only matches goal
terms, `data' only matches data terms, and `_' matches any term.
CLASS can also be a list of one or more of these symbols, in
which a term matches if it matches any of the classes in CLASS.
If CLASS is omitted or nil, it defaults to `_'.

Interactively, prompt for TEMPLATE and REPLACEMENT.  With a
prefix argument \\[universal-argument], prompt for CONDITION.
With a double prefix argument \\[universal-argument] \\[universal-argument],
prompt for CLASS as well."
  (interactive (let* ((template (sweeprolog-read-term "[Replace] ?- "))
                      (replacement
                       (sweeprolog-read-term
                        (concat "[Replace " template " with] ?- ")))
                      (condition (when current-prefix-arg
                                   (sweeprolog-read-goal
                                    (concat "[Condition for replacing "
                                            template
                                            "] ?- "))))
                      (class (when (equal current-prefix-arg '(16))
                               (mapcar #'intern
                                       (completing-read-multiple
                                        (format-prompt "Replace terms of class" "_")
                                        '("clause" "head" "goal" "data" "_")
                                        nil t nil nil "_")))))
                 (list template replacement condition class))
               sweeprolog-mode)
  (setq condition (or condition           "true")
        class     (or (ensure-list class) '(_)))
  (let* ((bounds-beg (or (use-region-beginning) (point)))
         (bounds-end (or (use-region-end) (point-max)))
         (items
          (seq-filter
           (pcase-lambda (`(,beg ,end ,_))
             (<= bounds-beg beg end bounds-end))
           (sweeprolog-term-replace-edits (buffer-file-name) template
                                          replacement condition class)))
         (overlays
          (mapcar
           (pcase-lambda (`(,beg ,end ,rep))
             (let ((overlay (make-overlay beg end)))
               (overlay-put overlay 'face 'sweeprolog-query-replace-term-match)
               (overlay-put overlay 'sweeprolog-term-replacement rep)
               overlay))
           (seq-filter
            sweeprolog-query-replace-term-include-match-function
            items)))
         (count 0)
         (last nil)
         (try nil))
    (cl-labels ((replace (b e r i)
                  (undo-boundary)
                  (combine-after-change-calls
                    (delete-region b e)
                    (insert r)
                    ;; TODO - mark fresh variables in the replacement
                    ;; term as holes.
                    (let ((inhibit-message t))
                      (indent-region-line-by-line b (point))))
                  (cl-incf count (if i -1 1))))
      (if (use-region-p)
          (deactivate-mark)
        (push-mark (point) t))
      (let ((inhibit-quit t))
        (with-local-quit
          (while overlays
            (let* ((overlay (car overlays))
                   (start (overlay-start overlay))
                   (end (overlay-end overlay))
                   (cur (buffer-substring start end))
                   (rep (overlay-get overlay 'sweeprolog-term-replacement))
                   (max-mini-window-height 0.5))
              (overlay-put overlay 'priority 100)
              (overlay-put overlay 'face 'sweeprolog-query-replace-term-current)
              (goto-char start)
              (setq last overlay)
              (setq overlays (cdr overlays))
              (pcase
                  (car (read-multiple-choice
                        (mapconcat #'identity
                                   (list "Replace" (propertize cur 'face 'sweeprolog-query-replace-term-prompt-old)
                                         (if try "back to" "with")
                                         (propertize rep 'face 'sweeprolog-query-replace-term-prompt-new)
                                         "?")
                                   (if (or (string-search "\n" cur)
                                           (string-search "\n" rep))
                                       "\n"
                                     " "))
                        '((?y  "yes"                       "Replace occurrence")
                          (?n  "no"                        "Skip occurrence")
                          (?t  "try"                       "Replace and suggest reverting back")
                          (?q  "quit"                      "Quit")
                          (?.  "last"                      "Replace and exit")
                          (?!  "all"                       "Replace all remaining occurrences")
                          (?e  "edit"                      "Edit replacement"))))
                (?y
                 (replace start end rep try)
                 (setq try nil))
                (?n
                 (setq try nil))
                (?t
                 (replace start end rep try)
                 (setq try (not try))
                 (let ((rev (make-overlay start (point))))
                   (overlay-put rev 'face 'sweeprolog-query-replace-term-match)
                   (overlay-put rev 'sweeprolog-term-replacement cur)
                   (setq overlays (cons rev overlays))))
                (?q
                 (mapc #'delete-overlay overlays)
                 (setq overlays nil))
                (?.
                 (replace start end rep try)
                 (mapc #'delete-overlay overlays)
                 (setq overlays nil))
                (?!
                 (replace start end rep try)
                 (while overlays
                   (let* ((ov (car overlays))
                          (ov-start (overlay-start ov))
                          (ov-end (overlay-end ov))
                          (ov-rep (overlay-get ov 'sweeprolog-term-replacement)))
                     (goto-char ov-start)
                     (delete-region ov-start ov-end)
                     (insert ov-rep)
                     (indent-region-line-by-line start (point))
                     (cl-incf count)
                     (delete-overlay ov)
                     (setq overlays (cdr overlays)))))
                (?e
                 (replace start end
                          (sweeprolog-read-term "[Edit replacement] ?- "
                                                rep)
                          try)))
              (delete-overlay overlay)
              (setq last nil))))
        (mapc #'delete-overlay overlays)
        (when last (delete-overlay last)))
      (message (concat "Replaced %d "
                       (ngettext "occurrence"
                                 "occurrences"
                                 count)
                       " of %s.")
               count template))))


;;;; Right-Click Context Menu

(defvar sweeprolog-context-menu-point-at-click nil
  "Buffer position at mouse click.")

(defvar sweeprolog-context-menu-region-beg-at-click nil
  "Beginning of region at time of mouse click.")

(defvar sweeprolog-context-menu-region-end-at-click nil
  "End of region at time of mouse click.")

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

(defun sweeprolog-context-menu-expand-macro ()
  "Expand Prolog macro at mouse click."
  (interactive)
  (sweeprolog-expand-macro-at-pos sweeprolog-context-menu-point-at-click))

(defun sweeprolog-context-menu-rename-variable ()
  "Rename Prolog variable at mouse click."
  (interactive)
  (sweeprolog-rename-variable sweeprolog-context-menu-variable-at-click
                              nil
                              sweeprolog-context-menu-point-at-click
                              t))

(defun sweeprolog-context-menu-replace-with-anonymous-variable ()
  "Replace variable at click with anonymous variable `_'."
  (interactive)
  (sweeprolog-replace-with-anonymous-variable
   sweeprolog-context-menu-point-at-click))

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
  (when-let ((mfa (sweeprolog--fragment-to-mfa tok (sweeprolog-buffer-module)))
             (pred (apply #'sweeprolog--mfa-to-pi mfa)))
    (setq sweeprolog-context-menu-predicate-at-click pred)
    (define-key menu [sweeprolog-describe-predicate]
                `(menu-item "Describe This Predicate"
                            sweeprolog-context-menu-describe-predicate
                            :help ,(format "Describe predicate %s" pred)
                            :keys "\\[sweeprolog-describe-predicate]"))))

(defun sweeprolog-context-menu-for-module (menu tok _beg _end _point)
  "Extend MENU with module-related commands if TOK describes one."
  (pcase tok
    (`("module" . ,module)
     (setq sweeprolog-context-menu-module-at-click module)
     (define-key menu [sweeprolog-describe-module]
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
     (when (equal tok "singleton")
       (define-key menu [sweeprolog-replace-with-anonymous-variable]
                   `(menu-item "Replace with Anonymous Variable"
                               sweeprolog-context-menu-replace-with-anonymous-variable
                               :help ,(format "Replace variable %s with anonymous variable `_'"
                                              (sweeprolog--format-variable
                                               sweeprolog-context-menu-variable-at-click))
                               :keys "\\[sweeprolog-replace-with-anonymous-variable]")))
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

(defun sweeprolog-context-menu-for-macro (menu tok beg _end _point)
  "Extend MENU with macro-related commands if TOK at BEG is one."
  (pcase tok
    (`("macro" . ,expansion)
     (setq sweeprolog-context-menu-point-at-click beg)
     (define-key menu [sweeprolog-expand-macro]
                 `(menu-item "Expand Macro"
                             sweeprolog-context-menu-expand-macro
                             :help ,(format "Expand macro to %s" expansion)
                             :keys "\\[sweeprolog-expand-macro-at-point]")))))

(defun sweeprolog-context-menu-for-region (menu _tok _beg _end point)
  "Extend MENU with commands for when the region is active and includes POINT."
  (when (and (use-region-p)
             (<= sweeprolog-context-menu-region-beg-at-click
                 point
                 sweeprolog-context-menu-region-end-at-click))
    (when (integerp (sweeprolog-context-callable-p
                     sweeprolog-context-menu-region-beg-at-click))
      (define-key
       menu [sweeprolog-extract-region-to-predicate]
       `(menu-item "Extract to New Predicate"
                   sweeprolog-extract-region-to-predicate
                   :help "Extract the selected goal into a separate predicate"
                   :keys "\\[sweeprolog-insert-term-dwim] in active region")))))

(defvar sweeprolog-context-menu-functions
  '(sweeprolog-context-menu-for-clause
    sweeprolog-context-menu-for-file
    sweeprolog-context-menu-for-module
    sweeprolog-context-menu-for-predicate
    sweeprolog-context-menu-for-variable
    sweeprolog-context-menu-for-macro
    sweeprolog-context-menu-for-region)
  "Functions that create context menu entries for Prolog tokens.
Each function receives as its arguments the menu, the Prolog
token's description, its start position, its end position, and
the position for which the menu is created.")

(defun sweeprolog-context-menu-function (menu click)
  "Populate MENU with Prolog commands at CLICK."
  (let ((point (posn-point (event-start click)))
        (sweeprolog-context-menu-region-beg-at-click (region-beginning))
        (sweeprolog-context-menu-region-end-at-click (region-end)))
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
  (propertize var 'face 'sweeprolog-variable))

(defvar sweeprolog-read-new-variable--existing-vars nil)
(defvar-local sweeprolog-read-new-variable--warned nil)

(defun sweeprolog--variable-name-p (string)
  "Return non-nil if STRING is valid Prolog variable name."
  (save-match-data
    (let ((case-fold-search nil))
      (string-match (rx bos (or "_" upper) (* (or "_" alnum)) eos) string))))

(defun sweeprolog--decode-numbered-variable-name (string)
  "Decode numbered variable name STRING.

Return cons cell (VAR . NUM), where VAR is the variable name sans
numbered variable index, and NUM is the index."
  (save-match-data
    (let ((case-fold-search nil))
      (when (string-match (rx bos (group-n 1
                                    (or "_" upper)
                                    (or (seq (* (or "_" alnum)) (or letter "_")) ""))
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
     (format-prompt (or prompt "Rename variable")
                    (when default
                      (sweeprolog--format-variable default)))
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
  "Increment numbered variables at POINT starting with FROM.

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
     (completing-read (format-prompt (or prompt "Breakpoint") def)
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

(defun sweeprolog--highlight-breakpoint (beg end)
  (font-lock--add-text-property beg end
                                'font-lock-face
                                'sweeprolog-breakpoint
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
                                           'sweeprolog-predicate-indicator)
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

(defun sweeprolog-expand-macro-at-pos (pos)
  "Expand Prolog macro starting at POS.

Return nil if POS is not the beginning of a macro invocation."
  (let* ((end (save-excursion
                (goto-char pos)
                (sweeprolog--forward-sexp)
                (point)))
         (expansion
          (sweeprolog--query-once "sweep" "sweep_expand_macro"
                                  (buffer-substring-no-properties
                                   pos end))))
    (when expansion
      (combine-after-change-calls
        (delete-region pos end)
        (save-excursion
          (goto-char pos)
          (insert expansion)))
      t)))

(defun sweeprolog-expand-macro-at-point (point)
  "Expand Prolog macro starting at POINT."
  (interactive "d" sweeprolog-mode)
  (unless (sweeprolog-expand-macro-at-pos point)
    (user-error "No macro invocation at point")))

(define-minor-mode sweeprolog-top-level-example-mode
  "Minor mode for example top-level sessions.
This mode is enabled in top-level buffers created by
\\[sweeprolog-make-example-usage-comment]."
  :lighter " Example"
  :group 'sweeprolog)

(defun sweeprolog-top-level-example-display-source ()
  "Pop to the source position where this example session was started.
This is the position where
`sweeprolog-make-example-usage-comment' was invoked to create
the current top-level buffer, and where
\\[sweeprolog-top-level-example-done] inserts its contents of as
a comment."
  (interactive "" sweeprolog-top-level-mode)
  (unless sweeprolog-top-level-example-mode
    (user-error "Not in an example top-level session"))
  (let ((source-buffer (marker-buffer sweeprolog-top-level-example-marker)))
    (unless (buffer-live-p source-buffer)
      (user-error "Source buffer for this example session no longer alive"))
    (let ((marker sweeprolog-top-level-example-marker))
      (display-buffer source-buffer)
      (goto-char marker))))

(defun sweeprolog-top-level-example-done ()
  "Finalize the current example top-level session.
This kills the current top-level buffer and inserts its contents
as a comment in the source location where you invoked
`sweeprolog-make-example-usage-comment' to create it."
  (interactive "" sweeprolog-top-level-mode)
  (unless sweeprolog-top-level-example-mode
    (user-error "Not in an example top-level session"))
  (let ((source-buffer (marker-buffer sweeprolog-top-level-example-marker)))
    (unless (buffer-live-p source-buffer)
      (user-error "Source buffer for this example session no longer alive"))
    (let ((top-level-buffer (current-buffer))
          (example
           (thread-last (buffer-string)
                        (substring-no-properties)
                        (replace-regexp-in-string "\n\n" "\n")
                        (replace-regexp-in-string (rx "\n?- " eos) "")))
          (marker sweeprolog-top-level-example-marker))
      (pop-to-buffer source-buffer)
      (unless (string-empty-p example)
        (save-excursion
          (goto-char marker)
          (insert example)
          (comment-region marker (point))))
      (sweeprolog-top-level-delete-process top-level-buffer)
      (kill-buffer top-level-buffer))))

(defun sweeprolog-make-example-usage-comment (point)
  "Start a top-level and record it as a comment at POINT.
This creates a new example top-level buffer where you can perform
queries in this top-level as usual.  Use
\\<sweeprolog-top-level-example-mode-map>\\[sweeprolog-top-level-example-done]
in the example top-level buffer to finish and format the session
as a comment in the source buffer at starting at POINT."
  (interactive "d" sweeprolog-mode)
  (let ((marker (copy-marker point))
        (buffer (sweeprolog-top-level-buffer (generate-new-buffer-name
                                              "*Example Session*"))))
    (pop-to-buffer buffer)
    (setq sweeprolog-top-level-example-marker marker
          header-line-format (substitute-command-keys
                              (format "`\\<sweeprolog-top-level-example-mode-map>\\[sweeprolog-top-level-example-done]' to quit and write contents as a comment in buffer %s" (buffer-name (marker-buffer marker)))))
    (sweeprolog-top-level-example-mode)))

(defun sweeprolog-add-log-current-defun ()
  "Return the indicator of the predicate defined at point, or nil.

This function is used as a `add-log-current-defun-function' in
`sweeprolog-mode' buffers."
  (when-let ((def (sweeprolog-definition-at-point)))
    (let ((fun  (nth 1 def))
          (ari  (nth 2 def))
          (neck (nth 4 def))
          (ind  "/")
          (mod (nth 5 def)))
      (when (string= neck "-->")
        (setq ari (- ari 2)
              ind "//"))
      (concat (when mod (concat mod ":"))
              fun ind (number-to-string ari)))))


;;;; Extract goals to separate predicates

(defun sweeprolog--extract-goal (str beg end new &optional file-name)
  (sweeprolog--query-once "sweep" "sweep_extract_goal"
                          (list str beg end new (or file-name
                                                    (buffer-file-name)))))

(defun sweeprolog-extract-region-to-predicate (beg end new &optional all)
  "Extract the Prolog goal from BEG to END into a new predicate, NEW.

BEG and END are buffer positions, and NEW is a string used as the
functor of the new predicate.  If the optional argument ALL is
non-nil, after extracting the selected goal, search for other
occurrences of this goal in the current buffer and suggest
replacing them with calls to the newly defined predicate.

Interactively, BEG and END are the beginning and end of the
current region, ALL is the prefix argument, and this command
prompts for NEW in the minibuffer.

This command defines the new predicate with arguments based on
the variables that the goal to extract shares with the containing
clause.

The user option `sweeprolog-new-predicate-location-function' says
where in the buffer to insert the newly created predicate."
  (interactive "r\nsNew predicate functor: \nP" sweeprolog-mode)
  (let* ((pred-beg nil)
         (pred-end nil)
         (clause-beg (save-excursion
                       (goto-char end)
                       (sweeprolog-beginning-of-top-term)
                       (point)))
         (clause-end (save-excursion
                       (goto-char beg)
                       (sweeprolog-end-of-top-term)
                       (point)))
         (clause-str (buffer-substring-no-properties clause-beg
                                                     clause-end)))
    (pcase
        (condition-case error
            (sweeprolog--extract-goal clause-str
                                      (- beg clause-beg)
                                      (- end clause-beg)
                                      new)
          (prolog-exception
           (pcase error
             (`(prolog-exception
                compound "error"
                (compound "syntax_error" ,_)
                ,_)
              (user-error "Cannot extract goal from invalid term!")))))
      ('nil (user-error (format "Selection %s is not a valid goal!" (buffer-substring-no-properties beg end))))
      (`(,call ,head ,neck ,body ,safe ,functor ,arity ,in-use)
       (cond
        ((or (and (not safe)
                  (not (y-or-n-p (concat
                                  "The selected goal contains a cut whose "
                                  "scope may change as a result of this "
                                  "operation.  Continue?"))))
             (and in-use
                  (not (y-or-n-p (concat
                                  (format "Predicate %s/%d is already defined.  "
                                          functor arity)
                                  "Continue?")))))
         (message "Canceled."))
        (t
         (goto-char beg)
         (combine-after-change-calls
           (delete-region beg end)
           (insert call)
           (funcall sweeprolog-new-predicate-location-function
                    functor arity neck)
           (setq pred-beg (1+ (point)))
           (insert "\n" head " " neck "\n" body ".\n")
           (indent-region-line-by-line pred-beg (point))
           (setq pred-end (point))
           (goto-char pred-beg))
         (deactivate-mark)
         (when all
           (save-excursion
             (goto-char (point-min))
             (let ((sweeprolog-query-replace-term-include-match-function
                    (pcase-lambda (`(,beg ,end . ,_))
                      (not (<= pred-beg beg end pred-end)))))
               (sweeprolog-query-replace-term
                body head "true" '(goal)))))
         (sweeprolog-analyze-buffer)))))))

(defun sweeprolog-maybe-extract-region-to-predicate (_point arg)
  (when (and (use-region-p)
             (integerp (sweeprolog-context-callable-p (use-region-beginning))))
    (sweeprolog-extract-region-to-predicate
     (use-region-beginning)
     (use-region-end)
     (read-string "Extract region to new predicate: ")
     arg)
    t))

;;;; Bug Reports

(defvar reporter-prompt-for-summary-p)

(defun sweeprolog-submit-bug-report ()
  "Report a bug in Sweep to the maintainers via mail."
  (interactive)
  (require 'reporter)
  (let ((version
         (with-current-buffer (find-file-noselect
                               (expand-file-name "sweeprolog.el"
                                                 sweeprolog--directory))
           (package-get-version)))
        (reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     "Sweep Development <~eshel/dev@lists.sr.ht>"
     (format "Sweep v%s" version)
     '(sweeprolog--directory
       sweeprolog--initialized
       sweeprolog-init-args
       sweeprolog-swipl-path
       system-configuration-features)
     nil nil
     (propertize " "
                 'display
                 (propertize
                  "Insert your bug report below.
If possible, specify where you got Emacs, SWI-Prolog and Sweep,
and include a recipe for reproducing your issue.
[This line and the above text are not included in your report.]"
                  'face 'italic)))))

;;;; Footer

(provide 'sweeprolog)

;;; sweeprolog.el ends here
