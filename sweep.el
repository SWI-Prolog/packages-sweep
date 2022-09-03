;;; sweep.el --- Embedded SWI-Prolog -*- lexical-binding:t -*-

;; Copyright (C) 2022 Eshel Yaron

;; Author: Eshel Yaron <me(at)eshelyaron(dot)com>
;; Maintainer: Eshel Yaron <me(at)eshelyaron(dot)com>
;; Keywords: prolog languages extensions
;; URL: https://git.sr.ht/~eshel/sweep
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; sweep is an embedding of SWI-Prolog in Emacs.  It uses the C
;; interfaces of both SWI-Prolog and Emacs Lisp to create a
;; dynamically loaded Emacs module that contains the SWI-Prolog
;; runtime.  As such, =sweep= has parts written in C, in Prolog and in
;; Emacs Lisp.
;;
;; For more information, see the sweep manual at
;; <https://eshelyaron.com/sweep.html>.  The manual can also be read
;; locally by evaluating (info "(sweep) Top")

;;; Code:

(require 'comint)

(defgroup sweep nil
  "SWI-Prolog Embedded in Emacs."
  :group 'prolog)

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

(defvar sweep-install-buffer-name "*Install sweep*"
  "Name of the buffer used for compiling sweep-module.")

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

;;;###autoload
(defun sweep--compile-module ()
  "Compile sweep-module."
  (interactive)
  (let* ((sweep-directory
          (shell-quote-argument (file-name-directory load-file-name)))
         (make-commands
          (concat
           "cd " sweep-directory "; make; cd -"))
         (buffer (get-buffer-create sweep-install-buffer-name)))
    (pop-to-buffer buffer)
    (compilation-mode)
    (if (zerop (let ((inhibit-read-only t))
                 (call-process "sh" nil buffer t "-c" make-commands)))
        (message "Compilation of `sweep' module succeeded")
      (error "Compilation of `sweep' module failed!"))))

(declare-function sweep-initialize    "sweep-module")
(declare-function sweep-initialized-p "sweep-module")
(declare-function sweep-open-query    "sweep-module")
(declare-function sweep-next-solution "sweep-module")
(declare-function sweep-cut-query     "sweep-module")
(declare-function sweep-close-query   "sweep-module")
(declare-function sweep-cleanup       "sweep-module")

(defun sweep--ensure-module ()
  (unless (require 'sweep-module nil t)
    (if (y-or-n-p "Sweep needs `sweep-module' to work.  Compile it now? ")
        (progn
          (sweep--compile-module)
          (require 'sweep-module))
      (error "Sweep will not work until `sweep-module' is compiled!"))))

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
         (cons (expand-file-name "bin/swipl" (file-name-directory
                                              load-file-name))
               (cons "-q" (cons "--no-signals" sweep-init-args))))
  (sweep-start-prolog-server))


(defvar sweep-predicate-completion-collection nil)

(defvar-local sweep-buffer-module nil)

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

(defun sweep-predicate-location (mfn)
  (sweep-open-query "user" "sweep" "sweep_predicate_location" mfn)
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
        (while (looking-at "[[:alnum:]_]" t)
          (backward-char))
        (when (looking-at ":[[:lower:]]")
          (unless (bobp) (backward-char))
          (while (looking-at "[[:alnum:]_]" t)
            (backward-char)))
        (forward-char)
        (when (looking-at "[[:lower:]]" t)
          (let ((start (point)))
            (while (looking-at "[[:alnum:]:_]" t)
              (forward-char))
            (cons start (point))))))))

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
                    (with-silent-modifications
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
  "Jump to the definiton of the Prolog predicate MFN.
MFN must be a string of the form \"M:F/N\" where M is a Prolog
module name, F is a functor name and N is its arity."
  (interactive (list (sweep-read-predicate)))
  (let* ((loc (sweep-predicate-location mfn))
         (path (car loc))
         (line (cdr loc)))
    (find-file path)
    (goto-char (point-min))
    (forward-line (1- line))))

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
      (user-error "Pacakge installation failed"))))

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

(sweep-defface no-file font-lock-warning-face
  "Non-existent file specifiers.")

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
  (let* ((beg (car  args))
         (end (+ beg (cadr args)))
         (arg (cddr args)))
    (with-silent-modifications
      (pcase arg
        (`("goal" . ,g)
         (put-text-property beg end 'font-lock-face
                            (pcase g
                              (`("recursion" . ,_) sweep-recursion-face)
                              (`("meta"      . ,_) sweep-meta-face)
                              (`("built_in"  . ,_) sweep-built-in-face)
                              (`("undefined" . ,_) sweep-undefined-face)
                              (_ sweep-goal-face))))
        ("unused_import"       (put-text-property beg end 'font-lock-face sweep-unused-import-face))
        ("undefined_import"    (put-text-property beg end 'font-lock-face sweep-undefined-import-face))
        ("dict_tag"            (put-text-property beg end 'font-lock-face sweep-dict-tag-face))
        ("dict_key"            (put-text-property beg end 'font-lock-face sweep-dict-key-face))
        ("dict_sep"            (put-text-property beg end 'font-lock-face sweep-dict-sep-face))
        ("atom"                (put-text-property beg end 'font-lock-face sweep-atom-face))
        ("float"               (put-text-property beg end 'font-lock-face sweep-float-face))
        ("int"                 (put-text-property beg end 'font-lock-face sweep-int-face))
        ("singleton"           (put-text-property beg end 'font-lock-face sweep-singleton-face))
        ("option_name"         (put-text-property beg end 'font-lock-face sweep-option-name-face))
        ("no_option_name"      (put-text-property beg end 'font-lock-face sweep-no-option-name-face))
        ("control"             (put-text-property beg end 'font-lock-face sweep-control-face))
        ("var"                 (put-text-property beg end 'font-lock-face sweep-variable-face))
        ("body"                (put-text-property beg end 'font-lock-face 'default))
        ("fullstop"            (put-text-property beg end 'font-lock-face sweep-fullstop-face))
        ("functor"             (put-text-property beg end 'font-lock-face sweep-functor-face))
        ("arity"               (put-text-property beg end 'font-lock-face sweep-arity-face))
        ("predicate_indicator" (put-text-property beg end 'font-lock-face sweep-predicate-indicator-face))
        ("string"              (put-text-property beg end 'font-lock-face sweep-string-face))
        ("module"              (put-text-property beg end 'font-lock-face sweep-module-face))
        ;; (other (message "Unknown color term %S" other))
        ))))

(defun sweep-colourise-query (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((beg (cdr comint-last-prompt))
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
                 (not (string= "|    " prompt)))
        (comint-send-input)))))

(defvar-local sweep-top-level-timer nil "Buffer-local timer.")

;;;###autoload
(define-derived-mode sweep-top-level-mode comint-mode "sweep Top-level"
  "Major mode for interacting with an inferior Prolog interpreter."
  :group 'sweep-top-level
  (setq-local comint-prompt-regexp           (rx (seq line-start "?- "))
              comint-input-ignoredups        t
              comint-prompt-read-only        t
              comint-delimiter-argument-list '(?,)
              comment-start "%")
  (add-hook 'post-self-insert-hook #'sweep-top-level--post-self-insert-function nil t)
  (setq sweep-top-level-timer (run-with-idle-timer 0.2 t #'sweep-colourise-query (current-buffer)))
  (add-hook 'completion-at-point-functions #'sweep-completion-at-point-function nil t)
  (setq sweep-buffer-module "user")
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
    (define-key map "P" #'sweep-pack-install)
    map)
  "Keymap for `sweep' global commands.")

;;;; Testing:

;; (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;; (require 'sweep)

(provide 'sweep)

;;; sweep.el ends here
