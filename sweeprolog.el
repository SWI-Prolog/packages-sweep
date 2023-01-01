;;; sweeprolog.el --- Embedded SWI-Prolog -*- lexical-binding:t -*-

;; Copyright (C) 2022 Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <~eshel/dev@lists.sr.ht>
;; Keywords: prolog languages extensions
;; URL: https://git.sr.ht/~eshel/sweep
;; Package-Version: 0.10.0
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
attempt to find the C defintions of SWI-Prolog native built-in
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
highlight all occurences of the variable at point in the current
clause."
  :package-version '((sweeprolog "0.4.2"))
  :type 'boolean
  :group 'sweeprolog)

(defcustom sweeprolog-new-predicate-location-function
  #'sweeprolog-default-new-predicate-location
  "Function used to choose a location for a new predicate definition.

It should take three arguments describing the new predicate,
FUNCTOR, ARITY and NECK, and move point to a suitable position in
the current buffer where the new predicate defintion should be
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
  "If non-nil, highlight holes in a dedicated faces."
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


;;;; Keymaps

(defvar sweeprolog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'sweeprolog-analyze-buffer)
    (define-key map (kbd "C-c C-d") #'sweeprolog-document-predicate-at-point)
    (define-key map (kbd "C-c C-e") #'sweeprolog-export-predicate)
    (define-key map (kbd "C-c C-i") #'sweeprolog-forward-hole)
    (define-key map (kbd "C-c C-l") #'sweeprolog-load-buffer)
    (define-key map (kbd "C-c C-m") #'sweeprolog-insert-term-with-holes)
    (define-key map (kbd "C-c C-o") #'sweeprolog-find-file-at-point)
    (define-key map (kbd "C-c C-t") #'sweeprolog-top-level)
    (define-key map (kbd "C-c C-u") #'sweeprolog-update-dependencies)
    (define-key map (kbd "C-c C-`")
                (if (fboundp 'flymake-show-buffer-diagnostics)  ;; Flymake 1.2.1+
                    #'sweeprolog-show-diagnostics
                  #'flymake-show-diagnostics-buffer))
    (define-key map (kbd "C-M-^")   #'kill-backward-up-list)
    (define-key map (kbd "C-M-m")   #'sweeprolog-insert-term-dwim)
    (define-key map (kbd "M-p")     #'sweeprolog-backward-predicate)
    (define-key map (kbd "M-n")     #'sweeprolog-forward-predicate)
    (define-key map (kbd "M-h")     #'sweeprolog-mark-predicate)
    map)
  "Keymap for `sweeprolog-mode'.")

(defvar sweeprolog-forward-hole-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-i") #'sweeprolog-forward-hole)
    (define-key map (kbd "C-n") #'sweeprolog-forward-hole)
    (define-key map (kbd "C-p") #'sweeprolog-backward-hole)
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
    (define-key map "t" #'sweeprolog-top-level)
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
    [ "Set Prolog Flag" sweeprolog-set-prolog-flag t ]
    [ "Install Prolog Package" sweeprolog-pack-install t ]
    "--"
    [ "Open Top-level" sweeprolog-top-level t ]
    [ "Signal Top-level"
      sweeprolog-top-level-signal
      (seq-filter (lambda (b)
                    (with-current-buffer b
                      (and (derived-mode-p 'sweeprolog-top-level-mode)
                           sweeprolog-top-level-thread-id)))
                  (buffer-list)) ]
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
    [ "Read the Sweep Manual" sweeprolog-info-manual t]))


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
      (load (match-string 1 line)))))

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

(defface sweeprolog-debug-prefix-face
  '((default :inherit shadow))
  "Face used to highlight the \"DEBUG\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-debug-prefix-face 'sweeprolog-debug-prefix-face
  "Name of the face used to highlight the \"DEBUG\" message prefix.")

(defface sweeprolog-debug-topic-face
  '((default :inherit shadow))
  "Face used to highlight the topic in debug messages."
  :group 'sweeprolog-faces)

(defvar sweeprolog-debug-topic-face 'sweeprolog-debug-topic-face
  "Name of the face used to highlight the topic in debug messages.")

(defface sweeprolog-info-prefix-face
  '((default :inherit default))
  "Face used to highlight the \"INFO\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-info-prefix-face 'sweeprolog-info-prefix-face
  "Name of the face used to highlight the \"INFO\" message prefix.")

(defface sweeprolog-warning-prefix-face
  '((default :inherit font-lock-warning-face))
  "Face used to highlight the \"WARNING\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-warning-prefix-face 'sweeprolog-warning-prefix-face
  "Name of the face used to highlight the \"WARNING\" message prefix.")

(defface sweeprolog-error-prefix-face
  '((default :inherit error))
  "Face used to highlight the \"ERROR\" message prefix."
  :group 'sweeprolog-faces)

(defvar sweeprolog-error-prefix-face 'sweeprolog-error-prefix-face
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
  (when-let ((proj (or project (project-current))))
    (mapc (lambda (path)
            (sweeprolog--query-once "sweep" "sweep_xref_source" path))
          (seq-filter (lambda (path)
                        (string= "pl" (file-name-extension path)))
                      (project-files proj)))))

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
                (member (car
                         (reverse
                          (seq-filter
                           (lambda (s)
                             (not (string-empty-p s)))
                           (file-name-split root))))
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
  (sweeprolog-analyze-buffer)
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
(defun sweeprolog-find-module (mod)
  "Jump to the source file of the Prolog module MOD."
  (interactive (list (sweeprolog-read-module-name)))
  (find-file (sweeprolog-module-path mod)))


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
          ((col (sweeprolog--query-once "sweep" "sweep_predicate_completion_candidates"
                                        (sweeprolog-context-callable-p))))
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
     (doc-string 4))
    (let ((func (intern (concat "sweeprolog-" (symbol-name name) "-face")))
          (facd (intern (concat "sweeprolog-" (symbol-name name) "-dark-face")))
          (facl (intern (concat "sweeprolog-" (symbol-name name) "-light-face")))
          (face (intern (concat "sweeprolog-" (symbol-name name) "-default-face"))))
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
  (:inherit default)
  (:inherit default)
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
  (:inherit default)
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
  codes
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
  (:inherit font-lock-constant-face)
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

(sweeprolog-defface
  around-syntax-error
  (:inherit default)
  (:inherit default)
  (:inherit default)
  "Text around a syntax error.")

(sweeprolog-defface
  clause
  (:inherit default)
  (:inherit default)
  (:inherit default)
  "Predicate clauses.")

(sweeprolog-defface
  grammar-rule
  (:inherit default)
  (:inherit default)
  (:inherit default)
  "DCG grammar rules.")

(sweeprolog-defface
  term
  (:inherit default)
  (:inherit default)
  (:inherit default)
  "Top terms.")

(sweeprolog-defface
  body
  (:inherit default)
  (:inherit default)
  (:inherit default)
  "Clause and query bodies.")

(sweeprolog-defface
  directive
  (:inherit default)
  (:inherit default)
  (:inherit default)
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
    (`("goal" ("autoload" . ,_) . ,_)
     (list (list beg end (sweeprolog-autoload-face))))
    (`("head" ,(rx "imported(") . ,_)
     (list (list beg end (sweeprolog-head-imported-face))))
    (`("head" ,(rx "extern(") . ,_)
     (list (list beg end (sweeprolog-head-extern-face))))
    (`("head" ,(rx "public ") . ,_)
     (list (list beg end (sweeprolog-head-public-face))))
    (`("head" ,(rx "dynamic ") . ,_)
     (list (list beg end (sweeprolog-head-dynamic-face))))
    (`("head" ,(rx "multifile ") . ,_)
     (list (list beg end (sweeprolog-head-multifile-face))))
    (`("head" ,(rx "local(") . ,_)
     (list (list beg end (sweeprolog-head-local-face))))
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
    (`("goal" ,(rx "dynamic ") . ,_)
     (list (list beg end (sweeprolog-dynamic-face))))
    (`("goal" ,(rx "multifile ") . ,_)
     (list (list beg end (sweeprolog-multifile-face))))
    (`("goal" ,(rx "thread_local ") . ,_)
     (list (list beg end (sweeprolog-thread-local-face))))
    (`("goal" ,(rx "extern(") . ,_)
     (list (list beg end (sweeprolog-extern-face))))
    (`("goal" ,(rx "imported(") . ,_)
     (list (list beg end (sweeprolog-imported-face))))
    (`("goal" ,(rx "global(") . ,_)
     (list (list beg end (sweeprolog-global-face))))
    (`("goal" ,(rx "local(") . ,_)
     (list (list beg end (sweeprolog-local-face))))
    ("instantiation_error"
     (list (list beg end (sweeprolog-instantiation-error-face))))
    ("type_error"
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
                (setq hole (sweeprolog--next-hole))))))))))

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
                ("type_error"
                 (cons :warning "Type error"))
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
                 (cons :warning "No such file"))))
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
  "Highlight occurences of the variable VAR in the clause at POINT.

If VAR is nil, clear variable highlighting in the current clause
instead.

Interactively, operate on the clause at point.  If a prefix
argument is specificed, clear variable highlighting in the
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

;;;###autoload
(defun sweeprolog-top-level (&optional buffer)
  "Run a Prolog top-level in BUFFER.
If BUFFER is nil, a buffer called \"*sweeprolog-top-level*\" is used
by default.

Interactively, a prefix arg means to prompt for BUFFER."
  (interactive
   (let* ((buffer
           (and current-prefix-arg
                (read-buffer "Top-level buffer: "
                             (if (and (eq major-mode 'sweeprolog-top-level-mode)
                                      (null (get-buffer-process
                                             (current-buffer))))
                                 (buffer-name)
                               (generate-new-buffer-name "*sweeprolog-top-level*"))))))
     (list buffer)))
  (let ((buf (get-buffer-create (or buffer "*sweeprolog-top-level*"))))
    (with-current-buffer buf
      (unless (eq major-mode 'sweeprolog-top-level-mode)
        (sweeprolog-top-level-mode)))
    (unless sweeprolog-prolog-server-port
      (sweeprolog-start-prolog-server))
    (unless (sweeprolog--query-once "sweep" "sweep_accept_top_level_client"
                                    (buffer-name buf))
      (error "Failed to create new top-level!"))
    (with-current-buffer buf
      (make-comint-in-buffer "sweeprolog-top-level"
                             buf
                             (cons "localhost"
                                   sweeprolog-prolog-server-port))
      (unless comint-last-prompt
        (accept-process-output (get-buffer-process (current-buffer)) 1))
      (sweeprolog-top-level--populate-thread-id))
    (pop-to-buffer buf sweeprolog-top-level-display-action)))

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
  (sweeprolog-signal-thread (buffer-local-value 'sweeprolog-top-level-thread-id
                                                (get-buffer buffer))
                            goal))

(defun sweeprolog-top-level-signal-current (goal)
  "Signal the current top-level thread to run GOAL.

Interactively, when called with a prefix argument, prompt for
GOAL.  Otherwise, GOAL is set to a default value specified by
`sweeprolog-top-level-signal-default-goal'."
  (interactive (list (if current-prefix-arg
                         (read-string "Signal goal: ?- " nil
                                      sweeprolog-top-level-signal-goal-history)
                       sweeprolog-top-level-signal-default-goal)))
  (sweeprolog-signal-thread sweeprolog-top-level-thread-id goal))

;;;###autoload
(define-derived-mode sweeprolog-top-level-mode comint-mode "sweep Top-level"
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
    (let* ((beg (point-min))
           (end (point-max))
           (contents (buffer-substring-no-properties beg end)))
      (if (sweeprolog--query-once "sweep" "sweep_load_buffer"
                                  (cons contents (buffer-file-name)))
          (message "Loaded %s." (buffer-name))
        (user-error "Loading %s failed!" (buffer-name))))))


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

(defun sweeprolog-find-file-at-point (point)
  "Find file specificed by the Prolog file spec at POINT.

Interactively, POINT is set to the current point."
  (interactive "d" sweeprolog-mode)
  (if-let ((file (sweeprolog-file-at-point point)))
      (find-file file)
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
            (re-search-backward (rx bol graph) nil t)
            (let ((safe-start (or (nth 8 (syntax-ppss))
                                  (nth 8 (syntax-ppss (1+ (point)))))))
              (while (and safe-start (not (bobp)))
                (goto-char safe-start)
                (if (bobp)
                    (setq safe-start nil)
                  (backward-char)
                  (re-search-backward (rx bol graph) nil t)
                  (setq safe-start (or (nth 8 (syntax-ppss))
                                       (nth 8 (syntax-ppss (1+ (point))))))))))
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
  (let ((start (point)))
    (when (use-region-p)
      (goto-char (region-beginning))
      (deactivate-mark))
    (when (sweeprolog--backward-wrap wrap)
      (if-let ((current-hole-beg (sweeprolog-beginning-of-hole)))
          (cons current-hole-beg
                (sweeprolog-end-of-hole))
        (let ((point (point)))
          (while (not (or (sweeprolog-at-hole-p) (bobp)))
            (forward-char -1))
          (if (bobp)
              (or (and wrap
                       (save-restriction
                         (goto-char (point-max))
                         (narrow-to-region point (point))
                         (sweeprolog--previous-hole)))
                  (and (goto-char start) nil))
            (cons (sweeprolog-beginning-of-hole)
                  (sweeprolog-end-of-hole))))))))

(defun sweeprolog--forward-hole (&optional wrap)
  (if-let ((hole (sweeprolog--next-hole wrap))
           (beg  (car hole))
           (end  (cdr hole)))
      (progn
        (goto-char end)
        (push-mark beg t t))
    (user-error "No holes following point")))

(defun sweeprolog--backward-hole (&optional wrap)
  (if-let ((hole (sweeprolog--previous-hole wrap))
           (beg  (car hole))
           (end  (cdr hole)))
      (progn
        (goto-char end)
        (push-mark beg t t))
    (user-error "No holes before point")))

(defun sweeprolog-backward-hole (&optional arg)
  "Move point to the previous hole in a `sweeprolog-mode' buffer.

With negative prefix argument ARG, move to the next hole
instead."
  (interactive "p" sweeprolog-mode)
  (setq arg (or arg 1))
  (sweeprolog-forward-hole (- arg)))

(defun sweeprolog-forward-hole (&optional arg)
  "Move point to the next hole in a `sweeprolog-mode' buffer.

With negative prefix argument ARG, move to the previous hole
instead."
  (interactive "p" sweeprolog-mode)
  (setq arg (or arg 1)
        deactivate-mark nil)
  (if (> 0 arg)
      (sweeprolog--backward-hole t)
    (sweeprolog--forward-hole t)))

(put 'sweeprolog-backward-hole
     'repeat-map
     'sweeprolog-forward-hole-repeat-map)

(put 'sweeprolog-forward-hole
     'repeat-map
     'sweeprolog-forward-hole-repeat-map)

(put 'sweeprolog-insert-term-with-holes
     'repeat-map
     'sweeprolog-forward-hole-repeat-map)

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

(defun sweeprolog-analyze-term-at-point (cb)
  (let ((sweeprolog--analyze-point (point)))
    (add-hook 'sweeprolog-analyze-region-fragment-hook cb nil t)
    (sweeprolog-analyze-term (point))
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
  "Move forward over the ARGth next predicate defintion from point."
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
  "Move backward over the ARGth next predicate defintion from point."
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

(defun sweeprolog-read-predicate-documentation-with-holes (fun ari)
  (list (sweeprolog-format-term-with-holes fun ari)
        (sweeprolog--hole "Det")
        nil))

(defun sweeprolog-read-predicate-documentation-default-function (fun
                                                                 ari)
  (let ((cur 1)
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
      (list (concat (sweeprolog-format-string-as-atom fun)
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
(define-derived-mode sweeprolog-mode prog-mode "sweep"
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
  (when sweeprolog-enable-flymake
    (add-hook 'flymake-diagnostic-functions #'sweeprolog-diagnostic-function nil t)
    (flymake-mode)
    (add-hook 'sweeprolog-analyze-region-start-hook #'sweeprolog-analyze-start-flymake nil t)
    (add-hook 'sweeprolog-analyze-region-fragment-hook #'sweeprolog-analyze-fragment-flymake nil t)
    (add-hook 'sweeprolog-analyze-region-end-hook #'sweeprolog-analyze-end-flymake nil t)
    (setq-local next-error-function #'flymake-goto-next-error)
    (add-hook 'window-selection-change-functions
              (let ((buffer (current-buffer)))
                (lambda (win)
                  (when (equal buffer
                               (window-buffer win))
                    (next-error-select-buffer buffer))))
              nil t))
  (when (and (boundp 'cycle-spacing-actions)
             (consp cycle-spacing-actions)
             sweeprolog-enable-cycle-spacing
             (setq-local cycle-spacing-actions (cons #'sweeprolog-align-spaces cycle-spacing-actions))))
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
    (cursor-sensor-mode 1)))


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
  (let ((refs (sweeprolog-predicate-references mfn)))
    (seq-map (lambda (loc)
               (let ((by (car loc))
                     (path (cadr loc))
                     (line (or (cddr loc) 1)))
                 (xref-make by (xref-make-file-location path line 0))))
             refs)))

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
  tabulated-list-mode "sweep Top-level Menu"
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

(defun sweeprolog-info-manual ()
  "Display the Sweep manual in Info mode."
  (interactive)
  (info "sweep"))

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


;;;; Dependency Managagement

(defun sweeprolog-update-dependencies ()
  "Add explicit dependencies for implicitly autoaloaded predicates."
  (interactive "" sweeprolog-mode)
  (let ((existing nil)
        (current-directive-beg nil)
        (current-directive-end nil)
        (current-directive-file nil))
    (sweeprolog-analyze-buffer-with
     (lambda (beg end arg)
       (pcase arg
         (`("goal_term" "built_in" "autoload" 2)
          (setq current-directive-beg beg
                current-directive-end end))
         (`("goal_term" "built_in" "use_module" 2)
          (setq current-directive-beg beg
                current-directive-end end))
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
    (if-let ((missing
              (sweeprolog--query-once "sweep" "sweep_file_missing_dependencies"
                                      (buffer-file-name))))
        (progn
          (dolist (autoloaded missing)
            (let* ((file    (nth 0 autoloaded))
                   (pred    (nth 1 autoloaded))
                   (kind    (nth 2 autoloaded)))
              (if (not pred)
                  (save-mark-and-excursion
                    (goto-char (point-min))
                    (sweeprolog-end-of-top-term)
                    (while (forward-comment 1))
                    (insert ":- " kind "("
                            (sweeprolog--query-once "sweep"
                                                    "sweep_file_path_in_library"
                                                    file)
                            ").\n\n")
                    (indent-region-line-by-line (save-excursion
                                                  (sweeprolog-beginning-of-top-term)
                                                  (point))
                                                (point)))
                (message "Adding explicit dependency on %s from %s."
                         pred file)
                (if-let ((marker (cdr (assoc-string file existing))))
                    (save-mark-and-excursion
                      (goto-char marker)
                      (pcase (sweeprolog-last-token-boundaries)
                        (`(open ,_ ,oend)
                         (goto-char oend)
                         (insert " " pred "\n"))
                        (`(symbol ,_ ,oend)
                         (let ((point (point)))
                           (goto-char oend)
                           (insert ",")
                           (goto-char (1+ point))
                           (insert pred "\n")))
                        (tok
                         (user-error "Unexpected token %s while looking for import list"
                                     tok)))
                      (indent-region-line-by-line  (save-excursion
                                                     (sweeprolog-beginning-of-top-term)
                                                     (point))
                                                   (save-excursion
                                                     (sweeprolog-end-of-top-term)
                                                     (point))))
                  (save-mark-and-excursion
                    (goto-char (point-min))
                    (sweeprolog-end-of-top-term)
                    (while (forward-comment 1))
                    (insert ":- " kind "("
                            (sweeprolog--query-once "sweep"
                                                    "sweep_file_path_in_library"
                                                    file)
                            ", [ " pred "\n]).\n\n")
                    (indent-region-line-by-line (save-excursion
                                                  (sweeprolog-beginning-of-top-term)
                                                  (point))
                                                (point))
                    (push (cons file (copy-marker (- (point) 5) t)) existing))))))
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
propely."
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
  (add-to-list 'command-line-functions
               #'sweeprolog-command-line-function))


;;;; Footer

(provide 'sweeprolog)

;;; sweeprolog.el ends here
