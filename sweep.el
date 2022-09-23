;;; sweep.el --- Embedded SWI-Prolog -*- lexical-binding:t -*-

;; Copyright (C) 2022 Eshel Yaron

;; Author: Eshel Yaron <me(at)eshelyaron(dot)com>
;; Maintainer: Eshel Yaron <me(at)eshelyaron(dot)com>
;; Keywords: prolog languages extensions
;; URL: https://git.sr.ht/~eshel/sweep
;; Package-Version: 0.3.1
;; Package-Requires: ((emacs "28"))

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

(defgroup sweep nil
  "SWI-Prolog Embedded in Emacs."
  :group 'prolog)

(defcustom sweep-indent-offset 4
  "Number of columns to indent lines with in `sweep-mode' buffers."
  :package-version '((sweep . "0.3.1"))
  :type 'integer
  :group 'sweep)

(defcustom sweep-colourise-buffer-on-idle t
  "If non-nil, update highlighting of `sweep-mode' buffers on idle."
  :package-version '((sweep . "0.2.0"))
  :type 'boolean
  :group 'sweep)

(defcustom sweep-colourise-buffer-max-size 100000
  "Maximum buffer size to recolourise on idle."
  :package-version '((sweep . "0.2.0"))
  :type 'integer
  :group 'sweep)

(defcustom sweep-colourise-buffer-min-interval 2
  "Minimum idle time to wait before recolourising the buffer."
  :package-version '((sweep . "0.2.0"))
  :type 'float
  :group 'sweep)

(defcustom sweep-swipl-path nil
  "Path to the swipl executable.
When non-nil, this is used by the embedded SWI-Prolog runtime to
locate its \"home\" directory.  Otherwise, the `executable-find'
is used to find a the swipl executable."
  :package-version '((sweep . "0.1.1"))
  :type 'string
  :group 'sweep)

(defcustom sweep-messages-buffer-name "*sweep Messages*"
  "The name of the buffer to use for logging Prolog messages."
  :package-version '((sweep . "0.1.1"))
  :type 'string
  :group 'sweep)

(defcustom sweep-read-flag-prompt "Flag: "
  "Prompt used for reading a Prolog flag name from the minibuffer."
  :package-version '((sweep . "0.1.2"))
  :type 'string
  :group 'sweep)

(defcustom sweep-read-module-prompt "Module: "
  "Prompt used for reading a Prolog module name from the minibuffer."
  :package-version '((sweep . "0.1.0"))
  :type 'string
  :group 'sweep)

(defcustom sweep-read-predicate-prompt "Predicate: "
  "Prompt used for reading a Prolog precicate name from the minibuffer."
  :package-version '((sweep . "0.1.0"))
  :type 'string
  :group 'sweep)

(defcustom sweep-read-pack-prompt "Pack: "
  "Prompt used for reading a Prolog pack name from the minibuffer."
  :package-version '((sweep . "0.1.0"))
  :type 'string
  :group 'sweep)

(defcustom sweep-top-level-display-action nil
  "Display action used for displaying the `sweep-top-level' buffer."
  :package-version '((sweep . "0.1.0"))
  :type 'function
  :group 'sweep)

(defcustom sweep-top-level-min-history-length 3
  "Minimum input length to record in the `sweep-top-level' history.

Inputs shorther than the value of this variable will not be
inserted to the input history in `sweep-top-level-mode' buffers."
  :package-version '((sweep . "0.2.1"))
  :type 'string
  :group 'sweep)

(defcustom sweep-init-on-load t
  "If non-nil, initialize Prolog when `sweep' is loaded."
  :package-version '((sweep "0.1.0"))
  :type 'boolean
  :group 'sweep)

(defcustom sweep-init-args (list (expand-file-name
                                  "sweep.pl"
                                  (file-name-directory load-file-name)))
  "List of strings used as initialization arguments for Prolog."
  :package-version '((sweep "0.1.0"))
  :type '(list string)
  :group 'sweep)

(defvar sweep-prolog-server-port nil)

(declare-function sweep-initialize    "sweep-module")
(declare-function sweep-initialized-p "sweep-module")
(declare-function sweep-open-query    "sweep-module")
(declare-function sweep-next-solution "sweep-module")
(declare-function sweep-cut-query     "sweep-module")
(declare-function sweep-close-query   "sweep-module")
(declare-function sweep-cleanup       "sweep-module")

(defun sweep--ensure-module ()
  (add-to-list 'load-path
               (car
                (split-string-and-unquote
                 (shell-command-to-string (concat
                                           (or sweep-swipl-path (executable-find "swipl"))
                                           " --dump-runtime-variables |"
                                           " grep PLLIBDIR |"
                                           " cut -f 2 -d = |"
                                           " cut -f 1 -d ';'")))))
  (require 'sweep-module))


(defface sweep-debug-prefix-face
  '((default :inherit shadow))
  "Face used to highlight the \"DEBUG\" message prefix."
  :group 'sweep-faces)

(defvar sweep-debug-prefix-face 'sweep-debug-prefix-face
  "Name of the face used to highlight the \"DEBUG\" message prefix.")

(defface sweep-debug-topic-face
  '((default :inherit shadow))
  "Face used to highlight the topic in debug messages."
  :group 'sweep-faces)

(defvar sweep-debug-topic-face 'sweep-debug-topic-face
  "Name of the face used to highlight the topic in debug messages.")

(defface sweep-info-prefix-face
  '((default :inherit default))
  "Face used to highlight the \"INFO\" message prefix."
  :group 'sweep-faces)

(defvar sweep-info-prefix-face 'sweep-info-prefix-face
  "Name of the face used to highlight the \"INFO\" message prefix.")

(defface sweep-warning-prefix-face
  '((default :inherit font-lock-warning-face))
  "Face used to highlight the \"WARNING\" message prefix."
  :group 'sweep-faces)

(defvar sweep-warning-prefix-face 'sweep-warning-prefix-face
  "Name of the face used to highlight the \"WARNING\" message prefix.")

(defface sweep-error-prefix-face
  '((default :inherit error))
  "Face used to highlight the \"ERROR\" message prefix."
  :group 'sweep-faces)

(defvar sweep-error-prefix-face 'sweep-error-prefix-face
  "Name of the face used to highlight the \"ERROR\" message prefix.")

(defun sweep-view-messages ()
  "View the log of recent Prolog messages."
  (interactive)
  (with-current-buffer (get-buffer-create sweep-messages-buffer-name)
    (goto-char (point-max))
    (let ((win (display-buffer (current-buffer))))
      (set-window-point win (point))
      win)))

(defun sweep-current-prolog-flags (&optional prefix)
  (sweep-open-query "user" "sweep" "sweep_current_prolog_flags" (or prefix ""))
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-read-prolog-flag ()
  "Read a Prolog flag from the minibuffer, with completion."
  (let* ((col (sweep-current-prolog-flags))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (let* ((val (cdr (assoc-string key col))))
                    (if val
                        (concat (make-string
                                 (max (- 32 (length key)) 1) ? )
                                val)
                      nil))))))
    (completing-read sweep-read-flag-prompt col)))

(defun sweep-set-prolog-flag (flag value)
  "Set the Prolog flag FLAG to VALUE.
FLAG and VALUE are specified as strings and read as Prolog terms."
  (interactive (let ((f (sweep-read-prolog-flag)))
                 (list f (read-string (concat "Set " f " to: ")))))
  (sweep-open-query "user"
                    "sweep"
                    "sweep_set_prolog_flag"
                    (cons flag value))
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (if (sweep-true-p sol)
        (message "Prolog flag %s set to %s" flag value)
      (user-error "Setting %s to %s failed!" flag value))))

(defun sweep-setup-message-hook ()
  (with-current-buffer (get-buffer-create sweep-messages-buffer-name)
    (setq-local window-point-insertion-type t)
    (compilation-minor-mode 1))
  (sweep-open-query "user"
                    "sweep"
                    "sweep_setup_message_hook"
                    nil)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    sol))

(defun sweep-message (message)
  (with-current-buffer (get-buffer-create sweep-messages-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (let ((kind (car message))
            (content (cdr message)))
        (pcase kind
          (`("debug" . ,topic)
           (insert (propertize "DEBUG" 'face sweep-debug-prefix-face))
           (insert "[")
           (insert (propertize topic 'face sweep-debug-topic-face))
           (insert "]: ")
           (insert content))
          ("informational"
           (insert (propertize "INFO" 'face sweep-info-prefix-face))
           (insert ": ")
           (insert content))
          ("warning"
           (insert (propertize "WARNING" 'face sweep-warning-prefix-face))
           (insert ": ")
           (insert content))
          ("error"
           (insert (propertize "ERROR" 'face sweep-error-prefix-face))
           (insert ": ")
           (insert content))))
      (newline))))

(defun sweep-start-prolog-server ()
  (sweep-open-query "user"
                    "prolog_server"
                    "prolog_server"
                    nil t)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (setq sweep-prolog-server-port (cdr sol)))))

(defun sweep-init ()
  (apply #'sweep-initialize
         (cons (or sweep-swipl-path (executable-find "swipl"))
               (cons "-q" (cons "--no-signals" sweep-init-args))))
  (sweep-setup-message-hook)
  (sweep-start-prolog-server))

(defvar sweep-predicate-completion-collection nil)

(defvar-local sweep-buffer-module "user")

(defun sweep-local-predicates-collection (&optional prefix)
  (sweep-open-query "user" "sweep" "sweep_local_predicate_completion"
                    (cons sweep-buffer-module
                          prefix))
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (setq sweep-predicate-completion-collection (cdr sol)))))

(defun sweep-predicates-collection (&optional prefix)
  (sweep-open-query "user" "sweep" "sweep_predicates_collection" prefix)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-predicate-references (mfn)
  (sweep-open-query "user" "sweep" "sweep_predicate_references" mfn)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-predicate-location (mfn)
  (sweep-open-query "user" "sweep" "sweep_predicate_location" mfn)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-predicate-apropos (pattern)
  (sweep-open-query "user" "sweep" "sweep_predicate_apropos" pattern)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-read-predicate ()
  "Read a Prolog predicate (M:F/N) from the minibuffer, with completion."
  (let* ((col (sweep-predicates-collection))
         (completion-extra-properties
          (list :annotation-function
                (lambda (key)
                  (let* ((val (cdr (assoc-string key col))))
                    (if val
                        (concat (make-string (- 64 (length key)) ? ) (car val))
                      nil))))))
    (completing-read sweep-read-predicate-prompt col)))

(defun sweep-predicate-prefix-boundaries (&optional point)
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

(defun sweep-prefix-operators (&optional file)
  (sweep-open-query "user"
                    "sweep" "sweep_prefix_ops"
                    (or file (buffer-file-name)))
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-completion-at-point-function ()
  (when-let ((bounds (sweep-predicate-prefix-boundaries)))
    (let ((start (car bounds))
          (end   (cdr bounds)))
      (list start end
            (completion-table-with-cache #'sweep-local-predicates-collection)
            :exclusive 'no
            :annotation-function
            (lambda (key)
              (when-let ((ann (cdr (assoc-string key sweep-predicate-completion-collection))))
                (concat " " (mapconcat #'identity ann ","))))
            :exit-function
            (lambda (key sts)
              (when (eq sts 'finished)
                (let ((opoint (point)))
                  (save-match-data
                    (combine-after-change-calls
                      (skip-chars-backward "1234567890")
                      (when (= ?/ (preceding-char))
                        (backward-char)
                        (let ((arity (string-to-number (buffer-substring-no-properties (1+ (point)) opoint))))
                          (delete-region (point) opoint)
                          (when (and
                                 (< 0 arity)
                                 (not
                                  (string=
                                   "op"
                                   (cadr
                                    (assoc-string
                                     key
                                     sweep-predicate-completion-collection)))))
                            (insert "(")
                            (dotimes (_ (1- arity))
                              (insert "_, "))
                            (insert "_)")
                            (goto-char (1- opoint))))))))))))))

;;;###autoload
(defun sweep-find-predicate (mfn)
  "Jump to the definition of the Prolog predicate MFN.
MFN must be a string of the form \"M:F/N\" where M is a Prolog
module name, F is a functor name and N is its arity."
  (interactive (list (sweep-read-predicate)))
  (if-let ((loc (sweep-predicate-location mfn)))
      (let ((path (car loc))
            (line (or (cdr loc) 1)))
        (find-file path)
        (goto-char (point-min))
        (forward-line (1- line)))
    (user-error "Unable to locate predicate %s" mfn)))

(defun sweep-modules-collection ()
  (sweep-open-query "user" "sweep" "sweep_modules_collection" nil)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-module-path (mod)
  (sweep-open-query "user" "sweep" "sweep_module_path" mod)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-read-module-name ()
  "Read a Prolog module name from the minibuffer, with completion."
  (let* ((col (sweep-modules-collection))
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
    (completing-read sweep-read-module-prompt col)))


(defun sweep--set-buffer-module ()
  (sweep-open-query "user" "sweep" "sweep_path_module" (buffer-file-name))
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (setq sweep-buffer-module (cdr sol)))))

;;;###autoload
(defun sweep-find-module (mod)
  "Jump to the source file of the Prolog module MOD."
  (interactive (list (sweep-read-module-name)))
  (find-file (sweep-module-path mod)))

(defun sweep-packs-collection ()
  (sweep-open-query "user" "sweep" "sweep_packs_collection" "")
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (when (sweep-true-p sol)
      (cdr sol))))

(defun sweep-read-pack-name ()
  "Read a Prolog pack name from the minibuffer, with completion."
  (let* ((col (sweep-packs-collection))
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
    (completing-read sweep-read-pack-prompt col)))

(defun sweep-true-p (sol)
  (or (eq (car sol) '!)
      (eq (car sol) t)))

;;;###autoload
(defun sweep-pack-install (pack)
  "Install or upgrade Prolog package PACK."
  (interactive (list (sweep-read-pack-name)))
  (sweep-open-query "user" "sweep" "sweep_pack_install" pack)
  (let ((sol (sweep-next-solution)))
    (sweep-close-query)
    (if (sweep-true-p sol)
        (message "Package install successful.")
      (user-error "Pacakge installation failed!"))))

;; (defun sweep-file-handler (operation &rest args)
;;   (cond ((eq operation 'expand-file-name) (apply sweep-expand-file-name args) )
;;         ;; ((eq operation 'file-name-all-completions))
;;         ;; ((eq operation 'file-name-completion))
;;         (t (let ((inhibit-file-name-handlers
;;                   (cons 'my-file-handler
;;                         (and (eq inhibit-file-name-operation operation)
;;                              inhibit-file-name-handlers)))
;;                  (inhibit-file-name-operation operation))
;;              (apply operation args)))))

;; (defun sweep-expand-file-name (name &optional dir)
;;   (sweep-open-query "user" "sweep" "sweep_expand_file_name" (cons name dir))
;;   (let ((sol (sweep-next-solution)))
;;     (sweep-close-query)
;;     (when (sweep-true-p sol)
;;       (cdr sol))))

(defgroup sweep-faces nil
  "Faces used to highlight Prolog code."
  :group 'sweep)

(eval-when-compile
  (defmacro sweep-defface (name def doc)
    "Define sweep face FACE with doc DOC."
    (declare
     (indent defun)
     (doc-string 3))
    (let ((face (intern (concat "sweep-" (symbol-name name) "-face"))))
      `(progn
         (defface ,face
           '((default :inherit ,def))
           ,(concat "Face used to highlight " (downcase doc))
           :group 'sweep-faces)
         (defvar ,face ',face
           ,(concat "Name of the face used to highlight " (downcase doc)))))))

(sweep-defface functor font-lock-function-name-face
  "Functors.")

(sweep-defface arity font-lock-function-name-face
  "Arities.")

(sweep-defface predicate-indicator font-lock-function-name-face
  "Predicate indicators.")

(sweep-defface built-in font-lock-keyword-face
  "Built in predicate calls.")

(sweep-defface neck font-lock-preprocessor-face
  "Necks.")

(sweep-defface goal font-lock-function-name-face
  "Unspecified predicate goals.")

(sweep-defface string font-lock-string-face
  "Strings.")

(sweep-defface comment font-lock-comment-face
  "Comments.")

(defface sweep-head-built-in-face
  '((default . (:foreground "black" :background "orange" :weight bold)))
  "Face used to highlight built-in predicate definitons."
  :group 'sweep-faces)

(defface sweep-method-face
  '((default . (:weight bold)))
  "Face used to highlight PCE methods."
  :group 'sweep-faces)

(defface sweep-class-face
  '((default . (:underline t)))
  "Face used to highlight PCE classes."
  :group 'sweep-faces)

(defface sweep-no-file-face
  '((default . (:foreground "red")))
  "Face used to highlight non-existsing file specifications."
  :group 'sweep-faces)

(sweep-defface head-local font-lock-builtin-face
  "Local predicate definitions.")

(sweep-defface head-meta font-lock-preprocessor-face
  "Meta predicate definitions.")

(sweep-defface head-multifile font-lock-type-face
  "Multifile predicate definitions.")

(sweep-defface head-extern font-lock-type-face
  "External predicate definitions.")

(sweep-defface head-unreferenced font-lock-warning-face
  "Unreferenced predicate definitions.")

(sweep-defface head-exported font-lock-builtin-face
  "Exported predicate definitions.")

(sweep-defface head-hook font-lock-type-face
  "Hook definitions.")

(sweep-defface head-iso font-lock-keyword-face
  "Hook definitions.")

(sweep-defface head-undefined font-lock-warning-face
  "Undefind head terms.")

(sweep-defface head-public font-lock-builtin-face
  "Public definitions.")

(sweep-defface meta-spec font-lock-preprocessor-face
  "Meta argument specifiers.")

(sweep-defface recursion font-lock-builtin-face
  "Recursive calls.")

(sweep-defface local font-lock-function-name-face
  "Local predicate calls.")

(sweep-defface autoload font-lock-function-name-face
  "Autoloaded predicate calls.")

(sweep-defface imported font-lock-function-name-face
  "Imported predicate calls.")

(sweep-defface extern font-lock-function-name-face
  "External predicate calls.")

(sweep-defface foreign font-lock-keyword-face
  "Foreign predicate calls.")

(sweep-defface meta font-lock-type-face
  "Meta predicate calls.")

(sweep-defface undefined font-lock-warning-face
  "Undefined predicate calls.")

(sweep-defface thread-local font-lock-constant-face
  "Thread local predicate calls.")

(sweep-defface global font-lock-keyword-face
  "Global predicate calls.")

(sweep-defface multifile font-lock-function-name-face
  "Multifile predicate calls.")

(sweep-defface dynamic font-lock-constant-face
  "Dynamic predicate calls.")

(sweep-defface undefined-import font-lock-warning-face
  "Undefined imports.")

(sweep-defface html-attribute font-lock-function-name-face
  "HTML attributes.")

(sweep-defface html-call font-lock-keyword-face
  "Multifile predicate calls.")

(sweep-defface option-name font-lock-constant-face
  "Option names.")

(sweep-defface no-option-name font-lock-warning-face
  "Non-existent option names.")

(sweep-defface flag-name font-lock-constant-face
  "Flag names.")

(sweep-defface no-flag-name font-lock-warning-face
  "Non-existent flag names.")

(sweep-defface qq-type font-lock-type-face
  "Quasi-quotation types.")

(sweep-defface qq-sep font-lock-type-face
  "Quasi-quotation separators.")

(sweep-defface qq-open font-lock-type-face
  "Quasi-quotation open sequences.")

(sweep-defface qq-close font-lock-type-face
  "Quasi-quotation close sequences.")

(sweep-defface op-type font-lock-type-face
  "Operator types.")

(sweep-defface dict-tag font-lock-constant-face
  "Dict tags.")

(sweep-defface dict-key font-lock-keyword-face
  "Dict keys.")

(sweep-defface dict-sep font-lock-keyword-face
  "Dict separators.")

(sweep-defface type-error font-lock-warning-face
  "Type errors.")

(sweep-defface instantiation-error font-lock-warning-face
  "Instantiation errors.")

(sweep-defface file button
  "File specifiers.")

(sweep-defface file-no-depend font-lock-warning-face
  "Unused file specifiers.")

(sweep-defface unused-import font-lock-warning-face
  "Unused imports.")

(sweep-defface identifier font-lock-type-face
  "Identifiers.")

(sweep-defface hook font-lock-preprocessor-face
  "Hooks.")

(sweep-defface module font-lock-type-face
  "Module names.")

(sweep-defface singleton font-lock-warning-face
  "Singletons.")

(sweep-defface fullstop font-lock-negation-char-face
  "Fullstops.")

(sweep-defface nil font-lock-keyword-face
  "The empty list.")

(sweep-defface variable font-lock-variable-name-face
  "Variables.")

(sweep-defface ext-quant font-lock-keyword-face
  "Existential quantifiers.")

(sweep-defface control font-lock-keyword-face
  "Control constructs.")

(sweep-defface atom font-lock-constant-face
  "Atoms.")

(sweep-defface int font-lock-constant-face
  "Integers.")

(sweep-defface float font-lock-constant-face
  "Floats.")

(sweep-defface codes font-lock-constant-face
  "Codes.")

(sweep-defface error font-lock-warning-face
  "Unspecified errors.")

(sweep-defface syntax-error error
  "Syntax errors.")

(sweep-defface structured-comment font-lock-doc-face
  "Structured comments.")

(defun sweep--colourise (args)
  "ARGS is a list of the form (BEG LEN . SEM)."
  (let* ((beg (max (point-min) (car  args)))
         (end (min (point-max) (+ beg (cadr args))))
         (arg (cddr args)))
    (with-silent-modifications
      (pcase arg
        (`("comment" . ,k)
         (put-text-property beg end 'font-lock-face
                            (pcase k
                              ("structured" sweep-structured-comment-face)
                              (_ sweep-comment-face))))
        (`("head" . ,h)
         (put-text-property beg end 'font-lock-face
                            (pcase h
                              (`("unreferenced" . ,_) sweep-head-unreferenced-face)
                              (`("meta" . ,_) sweep-head-meta-face)
                              (`("exported" . ,_) sweep-head-exported-face)
                              (`("hook" . ,_) sweep-head-hook-face)
                              (`("built_in" . ,_) 'sweep-head-built-in-face)
                              (`(,(rx "extern(") . ,_) sweep-head-extern-face)
                              (`(,(rx "public ") . ,_) sweep-head-public-face)
                              (`(,(rx "local(")  . ,_) sweep-head-local-face)
                              (other (message "unknown head color term %S" other) sweep-head-local-face))))
        (`("goal" . ,g)
         (put-text-property beg end 'font-lock-face
                            (pcase g
                              (`("recursion" . ,_) sweep-recursion-face)
                              (`("meta"      . ,_) sweep-meta-face)
                              (`("built_in"  . ,_) sweep-built-in-face)
                              (`("undefined" . ,_) sweep-undefined-face)
                              (`("global" . ,_) sweep-global-face)
                              (`(,(rx "dynamic ") . ,_) sweep-dynamic-face)
                              (`(,(rx "multifile ") . ,_) sweep-multifile-face)
                              (`(,(rx "thread_local ") . ,_) sweep-thread-local-face)
                              (`(,(rx "extern(") . ,_) sweep-extern-face)
                              (`(,(rx "autoload(") . ,_) sweep-autoload-face)
                              (`(,(rx "imported(") . ,_) sweep-imported-face)
                              (`(,(rx "global(") . ,_) sweep-global-face)
                              (`(,(rx "local(") . ,_) sweep-local-face)
                              (other (message "unknown goal color term %S" other) sweep-goal-face))))
        (`("syntax_error" ,_message ,_eb ,_ee)
         (put-text-property beg end 'font-lock-face sweep-syntax-error-face))
        ("unused_import"       (put-text-property beg end 'font-lock-face sweep-unused-import-face))
        ("undefined_import"    (put-text-property beg end 'font-lock-face sweep-undefined-import-face))
        ("html_attribute"      (put-text-property beg end 'font-lock-face sweep-html-attribute-face))
        ("html_call"           (put-text-property beg end 'font-lock-face sweep-html-call-face))
        ("dict_tag"            (put-text-property beg end 'font-lock-face sweep-dict-tag-face))
        ("dict_key"            (put-text-property beg end 'font-lock-face sweep-dict-key-face))
        ("dict_sep"            (put-text-property beg end 'font-lock-face sweep-dict-sep-face))
        ("meta"                (put-text-property beg end 'font-lock-face sweep-meta-spec-face))
        ("flag_name"           (put-text-property beg end 'font-lock-face sweep-flag-name-face))
        ("no_flag_name"        (put-text-property beg end 'font-lock-face sweep-flag-name-face))
        ("ext_quant"           (put-text-property beg end 'font-lock-face sweep-ext-quant-face))
        ("atom"                (put-text-property beg end 'font-lock-face sweep-atom-face))
        ("float"               (put-text-property beg end 'font-lock-face sweep-float-face))
        ("int"                 (put-text-property beg end 'font-lock-face sweep-int-face))
        ("singleton"           (put-text-property beg end 'font-lock-face sweep-singleton-face))
        ("option_name"         (put-text-property beg end 'font-lock-face sweep-option-name-face))
        ("no_option_name"      (put-text-property beg end 'font-lock-face sweep-no-option-name-face))
        ("control"             (put-text-property beg end 'font-lock-face sweep-control-face))
        ("var"                 (put-text-property beg end 'font-lock-face sweep-variable-face))
        ("fullstop"            (put-text-property beg end 'font-lock-face sweep-fullstop-face))
        ("functor"             (put-text-property beg end 'font-lock-face sweep-functor-face))
        ("arity"               (put-text-property beg end 'font-lock-face sweep-arity-face))
        ("predicate_indicator" (put-text-property beg end 'font-lock-face sweep-predicate-indicator-face))
        ("string"              (put-text-property beg end 'font-lock-face sweep-string-face))
        ("module"              (put-text-property beg end 'font-lock-face sweep-module-face))
        ("neck"                (put-text-property beg end 'font-lock-face sweep-neck-face))
        ("hook"                (put-text-property beg end 'font-lock-face sweep-hook-face))
        ("qq_type"             (put-text-property beg end 'font-lock-face sweep-qq-type-face))
        ("qq_sep"              (put-text-property beg end 'font-lock-face sweep-qq-sep-face))
        ("qq_open"             (put-text-property beg end 'font-lock-face sweep-qq-open-face))
        ("qq_close"            (put-text-property beg end 'font-lock-face sweep-qq-close-face))
        ("identifier"          (put-text-property beg end 'font-lock-face sweep-identifier-face))
        ("file"                (put-text-property beg end 'font-lock-face sweep-file-face))
        ("file_no_depend"      (put-text-property beg end 'font-lock-face sweep-file-no-depend-face))
        ("nofile"              (put-text-property beg end 'font-lock-face 'sweep-no-file-face))
        ("op_type"             (put-text-property beg end 'font-lock-face sweep-op-type-face))
        ("method"              (put-text-property beg end 'font-lock-face 'sweep-method-face))
        ("class"               (put-text-property beg end 'font-lock-face 'sweep-class-face))
        (`("goal_term" . ,_)   nil)
        (`("head_term" . ,_)   nil)
        ("clause"              nil)
        ("directive"           nil)
        ("body"                nil)
        ("html"                nil)
        ("parentheses"         nil)
        ("pce"                 nil)
        ("term"                nil)
        ("expanded"            nil)
        ("list"                nil)
        ("grammar_rule"        nil)
        ("dict"                nil)
        ("dict_content"        nil)
        ("brace_term"          nil)
        ("rule_condition"      nil)
        ("exported_operator"   nil)
        ("empty_list"          nil)
        ("dcg"                 nil)
        ("qq_content"          nil)
        ("qq"                  nil)
        (other (message "Unknown color term %S" other))))))

(defun sweep-colourise-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((beg (point-min))
           (end (point-max))
           (contents (buffer-substring-no-properties beg end)))
      (with-silent-modifications
        (font-lock-unfontify-region beg end))
      (sweep-open-query "user"
                        "sweep"
                        "sweep_colourise_buffer"
                        (cons contents (buffer-file-name)))
      (let ((sol (sweep-next-solution)))
        (sweep-close-query)
        sol))))

(defun sweep-colourise-some-terms (beg0 end0 &optional _verbose)
  (let* ((beg (save-mark-and-excursion
                (goto-char beg0)
                (sweep-beginning-of-top-term)
                (max (1- (point)) (point-min))))
         (end (save-mark-and-excursion
                (goto-char end0)
                (sweep-end-of-top-term)
                (point)))
         (contents (buffer-substring-no-properties beg end)))
    (with-silent-modifications
      (font-lock-unfontify-region beg end))
    (sweep-open-query "user"
                      "sweep"
                      "sweep_colourise_some_terms"
                      (list contents
                            (buffer-file-name)
                            beg))
    (let ((sol (sweep-next-solution)))
      (sweep-close-query)
      (when (sweep-true-p sol)
        `(jit-lock-bounds ,beg . ,end)))))

(defun sweep-colourise-query (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((beg (cdr comint-last-prompt))
                 (end (point-max))
                 (query (buffer-substring-no-properties beg end)))
        (with-silent-modifications
          (font-lock-unfontify-region beg end))
        (sweep-open-query "user"
                          "sweep"
                          "sweep_colourise_query"
                          (cons query (marker-position beg)))
        (let ((sol (sweep-next-solution)))
          (sweep-close-query)
          sol)))))

(defun sweep-load-buffer (buffer)
  "Load the Prolog buffer BUFFER into the embedded SWI-Prolog runtime.

Interactively, if the major mode of the current buffer is
`sweep-mode' and the command is called without a prefix argument,
load the current buffer.  Otherwise, prompt for a `sweep-mode'
buffer to load."
  (interactive (list
                (if (and (not current-prefix-arg)
                         (eq major-mode 'sweep-mode))
                    (current-buffer)
                  (read-buffer "Load buffer: "
                               (when (eq major-mode 'sweep-mode)
                                 (buffer-name))
                               t
                               (lambda (b)
                                 (let ((n (or (and (consp b) (car b)) b)))
                                   (with-current-buffer n
                                     (eq major-mode 'sweep-mode))))))))
  (with-current-buffer buffer
    (let* ((beg (point-min))
           (end (point-max))
           (contents (buffer-substring-no-properties beg end)))
      (sweep-open-query "user"
                        "sweep"
                        "sweep_load_buffer"
                        (cons contents (buffer-file-name)))
      (let ((sol (sweep-next-solution)))
        (sweep-close-query)
        (if (sweep-true-p sol)
            (message "Loaded %s." (buffer-name))
          (user-error "Loading %s failed!" (buffer-name)))))))

;;;###autoload
(defun sweep-top-level (&optional buffer)
  "Run a Prolog top-level in BUFFER.
If BUFFER is nil, a buffer called \"*sweep-top-level*\" is used
by default.

Interactively, a prefix arg means to prompt for BUFFER."
  (interactive
   (let* ((buffer
           (and current-prefix-arg
                (read-buffer "Top-level buffer: "
                             (if (and (eq major-mode 'sweep-top-level-mode)
                                      (null (get-buffer-process
                                             (current-buffer))))
                                 (buffer-name)
                               (generate-new-buffer-name "*sweep-top-level*"))))))
     (list buffer)))
  (let ((buf (get-buffer-create (or buffer "*sweep-top-level*"))))
   (with-current-buffer buf
     (unless (eq major-mode 'sweep-top-level-mode)
       (sweep-top-level-mode)))
   (make-comint-in-buffer "sweep-top-level"
                          buf
                          (cons "localhost"
                                sweep-prolog-server-port))
   (pop-to-buffer buf sweep-top-level-display-action)))

(defun sweep-top-level--post-self-insert-function ()
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

(defvar-local sweep-top-level-timer nil "Buffer-local timer.")

;;;###autoload
(define-derived-mode sweep-top-level-mode comint-mode "sweep Top-level"
  "Major mode for interacting with an inferior Prolog interpreter."
  :group 'sweep-top-level
  (setq-local comint-prompt-regexp           (rx line-start "?- ")
              comint-input-ignoredups        t
              comint-prompt-read-only        t
              comint-input-filter            (lambda (s)
                                               (< sweep-top-level-min-history-length
                                                  (length s)))
              comint-delimiter-argument-list '(?,)
              comment-start "%")
  (add-hook 'post-self-insert-hook #'sweep-top-level--post-self-insert-function nil t)
  (setq sweep-buffer-module "user")
  (add-hook 'completion-at-point-functions #'sweep-completion-at-point-function nil t)
  (setq sweep-top-level-timer (run-with-idle-timer 0.2 t #'sweep-colourise-query (current-buffer)))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (timerp sweep-top-level-timer)
                (cancel-timer sweep-top-level-timer)))))

(sweep--ensure-module)
(when sweep-init-on-load (sweep-init))

;;;###autoload
(defvar sweep-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'sweep-find-module)
    (define-key map "p" #'sweep-find-predicate)
    (define-key map "t" #'sweep-top-level)
    (define-key map "l" #'sweep-load-buffer)
    (define-key map "P" #'sweep-pack-install)
    (define-key map "F" #'sweep-set-prolog-flag)
    (define-key map "e" #'sweep-view-messages)
    map)
  "Keymap for `sweep' global commands.")

;;;###autoload
(defun sweep-file-name-handler (operation &rest args)
  (cond ((eq operation 'expand-file-name)
         (let ((fn (car  args))
               (dn (cadr args)))
           (sweep-open-query "user"
                             "sweep"
                             "sweep_expand_file_name"
                             (cons fn dn))
           (let ((sol (sweep-next-solution)))
             (sweep-close-query)
             (if (sweep-true-p sol)
                 (cdr sol)
               (let ((inhibit-file-name-handlers
                      (cons 'sweep-file-name-handler
                            (and (eq inhibit-file-name-operation operation)
                                 inhibit-file-name-handlers)))
                     (inhibit-file-name-operation operation))
                 (apply operation args))))))
        (t (let ((inhibit-file-name-handlers
                  (cons 'sweep-file-name-handler
                        (and (eq inhibit-file-name-operation operation)
                             inhibit-file-name-handlers)))
                 (inhibit-file-name-operation operation))
             (apply operation args)))))

(add-to-list 'file-name-handler-alist
             (cons (rx bol (one-or-more lower) "(")
                   #'sweep-file-name-handler))

(defun sweep-beginning-of-top-term (&optional arg)
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
                (backward-char)
                (re-search-backward (rx bol graph) nil t)
                (setq safe-start (or (nth 8 (syntax-ppss))
                                     (nth 8 (syntax-ppss (1+ (point)))))))))
          (not (= p (point))))
      (sweep-beginning-of-next-top-term (- times)))))

(defun sweep-beginning-of-next-top-term (times)
  (let ((p (point)))
    (while (and (< 0 times) (not (eobp)))
      (setq times (1- times))
      (unless (eobp)
        (forward-char)
        (re-search-forward (rx bol graph) nil t))
      (while (and (nth 8 (syntax-ppss)) (not (eobp)))
        (forward-char)
        (re-search-forward (rx bol graph) nil t)))
    (not (= p (point)))))

(defun sweep-end-of-top-term ()
  (unless (eobp)
    (while (and (nth 8 (syntax-ppss)) (not (eobp)))
        (forward-char))
    (or (re-search-forward (rx "." (or white "\n")) nil t)
        (goto-char (point-max)))
    (while (and (nth 8 (syntax-ppss)) (not (eobp)))
      (while (and (nth 8 (syntax-ppss)) (not (eobp)))
        (forward-char))
      (or (re-search-forward (rx "." (or white "\n")) nil t)
          (goto-char (point-max))))))

(defvar sweep-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14" table)
    table))

(defvar sweep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'sweep-load-buffer)
    (define-key map (kbd "C-c C-c") #'sweep-colourise-buffer)
    (define-key map (kbd "C-c C-t") #'sweep-top-level)
    (define-key map (kbd "C-c C-o") #'sweep-find-file-at-point)
    map)
  "Keymap for `sweep-mode'.")

(defun sweep-token-boundaries (&optional pos)
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

(defun sweep-last-token-boundaries (&optional pos)
  (let ((point (or pos (point)))
        (go t))
    (save-excursion
      (goto-char point)
      (while (and (not (bobp)) go)
        (skip-chars-backward " \t\n")
        (unless (bobp)
          (forward-char -1)
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
           ((= syn ?.)
            (skip-syntax-backward ".")
            (list 'operator (point) end))
           ((= syn ?\()
            (list 'open (1- end) end))
           ((= syn ?\))
            (list 'close (1- end) end))
           (t (list 'else (1- end) end))))))))

(defun sweep-backward-term (pre)
  (pcase (sweep-last-token-boundaries)
    ('nil nil)
    (`(open,_ ,_) nil)
    (`(functor,_ ,_) nil)
    (`(operator ,obeg ,oend)
     (unless (and (string= "." (buffer-substring-no-properties obeg oend))
                  (member (char-syntax (char-after (1+ obeg))) '(?> ? )))
      (if-let ((opre (sweep-op-infix-precedence
                      (buffer-substring-no-properties obeg oend))))
          (when (<= opre pre)
            (goto-char obeg)
            (sweep-backward-term pre))
        (if-let ((ppre (sweep-op-prefix-precedence
                        (buffer-substring-no-properties obeg oend))))
            (when (<= ppre pre)
              (goto-char obeg)
              (sweep-backward-term pre))
          (goto-char obeg)
          (sweep-backward-term pre)))))
    (`(symbol ,obeg ,oend)
     (if-let ((opre (sweep-op-infix-precedence
                     (buffer-substring-no-properties obeg oend))))
         (when (<= opre pre)
           (goto-char obeg)
           (sweep-backward-term pre))
       (if-let ((ppre (sweep-op-prefix-precedence
                       (buffer-substring-no-properties obeg oend))))
           (when (<= ppre pre)
             (goto-char obeg)
             (sweep-backward-term pre))
         (goto-char obeg)
         (sweep-backward-term pre))))
    (`(close ,lbeg ,_lend)
     (goto-char (nth 1 (syntax-ppss lbeg)))
     (when (or (= (char-syntax (char-before)) ?w)
               (= (char-syntax (char-before)) ?_))
       (skip-syntax-backward "w_"))
     (sweep-backward-term pre))
    (`(,_ ,lbeg ,_)
     (goto-char lbeg)
     (sweep-backward-term pre))))

(defun sweep-op-suffix-precedence (token)
  (sweep-open-query "user" "sweep" "sweep_op_info" (cons token (buffer-file-name)))
  (let ((res nil) (go t))
    (while go
      (if-let ((sol (sweep-next-solution))
               (det (car sol))
               (fix (cadr sol))
               (pre (cddr sol)))
          (if (member fix '("xf" "yf"))
              (setq res pre go nil)
            (when (eq '! det)
              (setq go nil)))
        (setq go nil)))
    (sweep-close-query)
    res))

(defun sweep-op-prefix-precedence (token)
  (sweep-open-query "user" "sweep" "sweep_op_info" (cons token (buffer-file-name)))
  (let ((res nil) (go t))
    (while go
      (if-let ((sol (sweep-next-solution))
               (det (car sol))
               (fix (cadr sol))
               (pre (cddr sol)))
          (if (member fix '("fx" "fy"))
              (setq res pre go nil)
            (when (eq '! det)
              (setq go nil)))
        (setq go nil)))
    (sweep-close-query)
    res))

(defun sweep-op-infix-precedence (token)
  (sweep-open-query "user" "sweep" "sweep_op_info" (cons token (buffer-file-name)))
  (let ((res nil) (go t))
    (while go
      (if-let ((sol (sweep-next-solution))
               (det (car sol))
               (fix (cadr sol))
               (pre (cddr sol)))
          (if (member fix '("xfx" "xfy" "yfx"))
              (setq res pre go nil)
            (when (eq '! det)
              (setq go nil)))
        (setq go nil)))
    (sweep-close-query)
    res))

(defun sweep-indent-line-after-functor (fbeg _fend)
  (save-excursion
    (goto-char fbeg)
    (+ (current-column) sweep-indent-offset)))

(defun sweep-indent-line-after-open (fbeg _fend)
  (save-excursion
    (goto-char fbeg)
    (+ (current-column) sweep-indent-offset)))

(defun sweep-indent-line-after-prefix (fbeg _fend _pre)
  (save-excursion
    (goto-char fbeg)
    (+ (current-column) 4)))

(defun sweep-indent-line-after-term ()
  (if-let ((open (nth 1 (syntax-ppss))))
      (save-excursion
        (goto-char open)
        (current-column))
    'noindent))

(defun sweep-indent-line-after-neck (fbeg _fend)
  (save-excursion
    (goto-char fbeg)
    (sweep-backward-term 1200)
    (+ (current-column) sweep-indent-offset)))

(defun sweep-indent-line-after-infix (fbeg _fend pre)
  (save-excursion
    (goto-char fbeg)
    (let ((lim (or (nth 1 (syntax-ppss)) (point-min)))
          (cur (point))
          (go t))
      (while go
        (setq cur (point))
        (sweep-backward-term pre)
        (when (< (point) lim)
          (goto-char cur))
        (when (= (point) cur)
          (setq go nil))))
    (current-column)))

(defun sweep-indent-line ()
  "Indent the current line in a `sweep-mode' buffer."
  (interactive)
  (let ((pos (- (point-max) (point))))
    (back-to-indentation)
    (let ((indent (if (nth 8 (syntax-ppss))
                      'noindent
                    (if-let ((open (and (= (char-syntax (char-after)) ?\))
                                        (nth 1 (syntax-ppss)))))
                        (save-excursion
                          (goto-char open)
                          (when (or (= (char-syntax (char-before)) ?w)
                                    (= (char-syntax (char-before)) ?_))
                            (when (save-excursion
                                    (forward-char)
                                    (skip-syntax-forward " " (line-end-position))
                                    (eolp))
                              (skip-syntax-backward "w_")))
                          (current-column))
                      (pcase (sweep-last-token-boundaries)
                        ('nil 'noindent)
                        (`(functor ,lbeg ,lend)
                         (sweep-indent-line-after-functor lbeg lend))
                        (`(open ,lbeg ,lend)
                         (sweep-indent-line-after-open lbeg lend))
                        (`(symbol ,lbeg ,lend)
                         (let ((sym (buffer-substring-no-properties lbeg lend)))
                           (cond
                            ((pcase (sweep-op-prefix-precedence sym)
                               ('nil (sweep-indent-line-after-term))
                               (pre  (sweep-indent-line-after-prefix lbeg lend pre)))))))
                        (`(operator ,lbeg ,lend)
                         (let ((op (buffer-substring-no-properties lbeg lend)))
                           (cond
                            ((string= op ".") 'noindent)
                            ((pcase (sweep-op-infix-precedence op)
                               ('nil nil)
                               (1200 (sweep-indent-line-after-neck lbeg lend))
                               (pre  (sweep-indent-line-after-infix lbeg lend pre))))
                            ((pcase (sweep-op-prefix-precedence op)
                               ('nil nil)
                               (pre  (sweep-indent-line-after-prefix lbeg lend pre)))))))
                        (`(,_ltyp ,_lbeg ,_lend)
                         (sweep-indent-line-after-term)))))))
      (when (numberp indent)
        (unless (= indent (current-column))
          (combine-after-change-calls
            (delete-horizontal-space)
            (insert (make-string indent ? )))))
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))
      indent)))

(defun sweep-syntax-propertize (start end)
  (goto-char start)
  (let ((case-fold-search nil))
    (funcall
     (syntax-propertize-rules
      ((rx bow (group-n 1 "0'" anychar))
       (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
            (string-to-syntax "w"))))
      ((rx (group-n 1 "!"))
       (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
            (string-to-syntax "w")))))
     start end)))

(defun sweep-at-beginning-of-top-term-p ()
  (and (looking-at-p (rx bol graph))
       (not (nth 8 (syntax-ppss)))))

(defun sweep-file-at-point (&optional point)
  (let* ((p (or point (point)))
         (beg (save-mark-and-excursion
                (goto-char p)
                (unless (sweep-at-beginning-of-top-term-p)
                  (sweep-beginning-of-top-term))
                (max (1- (point)) (point-min))))
         (end (save-mark-and-excursion
                (goto-char p)
                (sweep-end-of-top-term)
                (point)))
         (contents (buffer-substring-no-properties beg end)))
    (sweep-open-query "user"
                      "sweep"
                      "sweep_file_at_point"
                      (list contents
                            (buffer-file-name)
                            (- p beg)))
    (let ((sol (sweep-next-solution)))
      (sweep-close-query)
      (when (sweep-true-p sol)
        (cdr sol)))))

(defun sweep-find-file-at-point (point)
  "Find file specificed by the Prolog file spec at POINT.

Interactively, POINT is set to the current point."
  (interactive "d" sweep-mode)
  (if-let ((file (sweep-file-at-point point)))
      (find-file file)
    (user-error "No file specification found at point!")))

(defun sweep-identifier-at-point (&optional point)
  (let* ((p (or point (point)))
         (beg (save-mark-and-excursion
                (goto-char p)
                (unless (sweep-at-beginning-of-top-term-p)
                  (sweep-beginning-of-top-term))
                (max (1- (point)) (point-min))))
         (end (save-mark-and-excursion
                (goto-char p)
                (sweep-end-of-top-term)
                (point)))
         (contents (buffer-substring-no-properties beg end)))
    (sweep-open-query "user"
                      "sweep"
                      "sweep_identifier_at_point"
                      (list contents
                            (buffer-file-name)
                            (- p beg)))
    (let ((sol (sweep-next-solution)))
      (sweep-close-query)
      (when (sweep-true-p sol)
        (cdr sol)))))

(defun sweep--xref-backend ()
  "Hook for `xref-backend-functions'."
  'sweep)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql sweep)))
  (sweep-identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql sweep)))
  (completion-table-with-cache #'sweep-predicates-collection))

(cl-defmethod xref-backend-identifier-completion-ignore-case ((_backend (eql sweep)))
  "Case is always significant for Prolog identifiers, so return nil."
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql sweep)) mfn)
  (when-let ((loc (sweep-predicate-location mfn))
             (path (car loc))
             (line (or (cdr loc) 1)))
    (list (xref-make (concat path ":" (number-to-string line)) (xref-make-file-location path line 0)))))

(cl-defmethod xref-backend-references ((_backend (eql sweep)) mfn)
  (let ((refs (sweep-predicate-references mfn)))
    (seq-map (lambda (loc)
               (let ((by (car loc))
                     (path (cadr loc))
                     (line (or (cddr loc) 1)))
                 (xref-make by (xref-make-file-location path line 0))))
             refs)))

(cl-defmethod xref-backend-apropos ((_backend (eql sweep)) pattern)
  (let ((matches (sweep-predicate-apropos pattern)))
    (seq-map (lambda (match)
               (let ((mfn (car match))
                     (path (cadr match))
                     (line (or (cddr match) 1)))
                 (xref-make mfn
                            (xref-make-file-location path line 0))))
             matches)))

(defvar-local sweep--timer nil)
(defvar-local sweep--colourise-buffer-duration 0.2)

;;;###autoload
(define-derived-mode sweep-mode prog-mode "sweep"
  "Major mode for reading and editing Prolog code."
  :group 'sweep
  (setq-local comment-start "%")
  (setq-local comment-start-skip "\\(?:/\\*+ *\\|%+ *\\)")
  (setq-local parens-require-spaces nil)
  (setq-local beginning-of-defun-function #'sweep-beginning-of-top-term)
  (setq-local end-of-defun-function #'sweep-end-of-top-term)
  (setq-local syntax-propertize-function #'sweep-syntax-propertize)
  (setq-local indent-line-function #'sweep-indent-line)
  (setq-local font-lock-defaults
              '(nil
                nil
                nil
                nil
                nil
                (font-lock-fontify-region-function . sweep-colourise-some-terms)))
  (let ((time (current-time)))
    (sweep-colourise-buffer)
    (setq sweep--colourise-buffer-duration (float-time (time-since time))))
  (sweep--set-buffer-module)
  (add-hook 'xref-backend-functions #'sweep--xref-backend nil t)
  (add-hook 'file-name-at-point-functions #'sweep-file-at-point nil t)
  (add-hook 'completion-at-point-functions #'sweep-completion-at-point-function nil t)
  (when sweep-colourise-buffer-on-idle
    (setq sweep--timer (run-with-idle-timer (max sweep-colourise-buffer-min-interval
                                                 (* 10 sweep--colourise-buffer-duration))
                                            t
                                            (let ((buffer (current-buffer)))
                                              (lambda ()
                                                (unless (< sweep-colourise-buffer-max-size
                                                           (buffer-size buffer))
                                                  (sweep-colourise-buffer buffer))))))
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (timerp sweep--timer)
                  (cancel-timer sweep--timer))))))

;;;; Testing:

;; (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;; (require 'sweep)

(provide 'sweep)

;;; sweep.el ends here
