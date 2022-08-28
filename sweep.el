;;; sweep.el --- Embedded SWI-Prolog -*- lexical-binding:t -*-

;; Copyright (C) 2022 Eshel Yaron

;; Author: Eshel Yaron <me(at)eshelyaron(dot)com>
;; Maintainer: Eshel Yaron <me(at)eshelyaron(dot)com>
;; Keywords: prolog languages extensions
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
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

(defun sweep-home-directory ()
  "Return the installation directory of `sweep'."
  (file-name-directory (locate-library "sweep.el" t)))

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

(defvar sweep-install-buffer-name "*Install sweep*"
  "Name of the buffer used for compiling sweep-module.")

(defcustom sweep-init-on-load t
  "If non-nil, initialize Prolog when `sweep' is loaded."
  :package-version '((sweep "0.1.0"))
  :type 'boolean
  :group 'sweep)

(defcustom sweep-init-args (list (expand-file-name
                                  "sweep.pl"
                                  (sweep-home-directory)))
  "List of strings used as initialization arguments for Prolog."
  :package-version '((sweep "0.1.0"))
  :type '(list string)
  :group 'sweep)

;;;###autoload
(defun sweep-module-compile ()
  "Compile sweep-module."
  (interactive)
  (let* ((sweep-directory
          (shell-quote-argument (sweep-home-directory)))
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

(defun sweep--ensure-module ()
  (unless (require 'sweep-module nil t)
    (if (y-or-n-p "Sweep needs `sweep-module' to work.  Compile it now? ")
        (progn
          (sweep-module-compile)
          (require 'sweep-module))
      (error "Sweep will not work until `sweep-module' is compiled!"))))

(defun sweep-init ()
  (apply #'sweep-initialize
         (cons (expand-file-name "bin/swipl" (sweep-home-directory))
               (cons "-q" sweep-init-args))))

(defun sweep-predicates-collection ()
  (sweep-open-query "user" "sweep" "sweep_predicates_collection" nil)
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
                  (message key)
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

;;;; Testing:

;; (add-to-list 'load-path (file-name-directory (buffer-file-name)))
;; (require 'sweep)

(sweep--ensure-module)
(when sweep-init-on-load (sweep-init))

(provide 'sweep)

;;; sweep.el ends here
