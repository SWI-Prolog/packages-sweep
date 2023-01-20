;;; sweeprolog-tests.el --- ERT suite for sweep  -*- lexical-binding:t -*-

(require 'sweeprolog)

(remove-hook 'flymake-diagnostic-functions
             #'flymake-proc-legacy-flymake)

(add-hook 'sweeprolog-mode-hook (lambda ()
                                  (setq-local indent-tabs-mode nil
                                              inhibit-message t)))

(defconst sweeprolog-tests-greeting
  "Hello from Elisp from Prolog from Elisp from Prolog from Elisp!")

(defun sweeprolog-tests-greet ()
  (sweeprolog--open-query "user" "user"
                          "sweep_funcall"
                          "sweeprolog-tests-greet-1")
  (let ((sol (sweeprolog-next-solution)))
    (sweeprolog-cut-query)
    (cdr sol)))

(defun sweeprolog-tests-greet-1 ()
  sweeprolog-tests-greeting)

(ert-deftest elisp->prolog->elisp->prolog->elisp ()
  "Tests calling Elisp from Prolog from Elisp from Prolog from Elisp."
  (should (equal (sweeprolog--open-query "user" "user"
                                        "sweep_funcall"
                                        "sweeprolog-tests-greet")
                 t))
  (should (equal (sweeprolog-next-solution) (cons '! sweeprolog-tests-greeting)))
  (should (equal (sweeprolog-cut-query) t)))

(ert-deftest lists:member/2 ()
  "Tests calling the Prolog predicate permutation/2 from Elisp."
  (should (equal (sweeprolog--open-query "user" "lists" "member" (list 1 2 3) t) t))
  (should (equal (sweeprolog-next-solution) (cons t 1)))
  (should (equal (sweeprolog-next-solution) (cons t 2)))
  (should (equal (sweeprolog-next-solution) (cons '! 3)))
  (should (equal (sweeprolog-cut-query) t)))

(ert-deftest lists:permutation/2 ()
  "Tests calling the Prolog predicate permutation/2 from Elisp."
  (should (equal (sweeprolog--open-query "user" "lists" "permutation" (list 1 2 3)) t))
  (should (equal (sweeprolog-next-solution) (list t 1 2 3)))
  (should (equal (sweeprolog-next-solution) (list t 1 3 2)))
  (should (equal (sweeprolog-next-solution) (list t 2 1 3)))
  (should (equal (sweeprolog-next-solution) (list t 2 3 1)))
  (should (equal (sweeprolog-next-solution) (list t 3 1 2)))
  (should (equal (sweeprolog-next-solution) (list t 3 2 1)))
  (should (equal (sweeprolog-next-solution) nil))
  (should (equal (sweeprolog-cut-query) t)))

(ert-deftest system:=/2 ()
  "Tests unifying Prolog terms with =/2 from Elisp."
  (should (equal (sweeprolog--open-query "user" "system" "=" (list 1 nil (list "foo" "bar") 3.14)) t))
  (should (equal (sweeprolog-next-solution) (list '! 1 nil (list "foo" "bar") 3.14)))
  (should (equal (sweeprolog-next-solution) nil))
  (should (equal (sweeprolog-cut-query) t)))

(ert-deftest beginning-of-next-top-term ()
  "Test finding the beginning of the next top term."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
foo(Bar) :- bar.
foo(Baz) :- baz.
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-min))
    (should (sweeprolog-beginning-of-next-top-term))
    (should (= (point) 2))
    (should (sweeprolog-beginning-of-next-top-term))
    (should (= (point) 19))
    (should (not (sweeprolog-beginning-of-next-top-term)))
    (should (= (point) 19))))

(ert-deftest help-echo-for-dependency ()
  "Test that the `help-echo' property is set correctly."
  (let ((temp (make-temp-file "sweeprolog-help-echo-text"
                              nil
                              "pl"
                              "
:- use_module(library(lists)).

foo(Foo, Bar) :- flatten(Bar, Baz), member(Foo, Baz).
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char 24)
    (should (string-match "Dependency on .*, resolves calls to flatten/2, member/2"
                          (help-at-pt-kbd-string)))))

(ert-deftest terms-at-point ()
  "Test `sweeprolog-term-search'."
  (let ((temp (make-temp-file "sweeprolog-terms-at-point-test"
                              nil
                              "pl"
                              "
recursive(Var) :-
    (   true
    ->  recursive(Bar)
    ;   var(Baz)
    *-> Bar is foo
    ).
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (should (equal (sweeprolog-terms-at-point 81)
                '("Bar"
                  "Bar is foo"
                  "var(Baz)
    *-> Bar is foo" "true
    ->  recursive(Bar)
    ;   var(Baz)
    *-> Bar is foo"
    "recursive(Var) :-
    (   true
    ->  recursive(Bar)
    ;   var(Baz)
    *-> Bar is foo
    )")))))

(ert-deftest predicate-location ()
  "Test `sweeprolog-predicate-location'."
  (should (sweeprolog-predicate-location "memory_file:new_memory_file/1")))

(ert-deftest term-search ()
  "Test `sweeprolog-term-search'."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
bar(bar(bar), bar{bar:bar}, [bar,bar|bar]).
foo([Bar|Baz]).
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-min))
    (sweeprolog-term-search "bar")
    (should (= (point) 10))
    (sweeprolog-term-search "bar")
    (should (= (point) 24))
    (sweeprolog-term-search "bar")
    (should (= (point) 31))
    (sweeprolog-term-search "bar")
    (should (= (point) 35))
    (sweeprolog-term-search "bar")
    (should (= (point) 39))))

(ert-deftest beginning-of-next-top-term-header ()
  "Test finding the beginning of the first top term."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "/*
    Author:        Eshel Yaron
    E-mail:        eshel@swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*
foobar :- baz.
*/

:- module(mod")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-min))
    (should (sweeprolog-beginning-of-next-top-term))
    (should (= (point) 1509))
    (should (not (sweeprolog-beginning-of-next-top-term)))
    (should (= (point) 1509))))

(ert-deftest font-lock ()
  "Test semantic highlighting of Prolog code."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              ":- module(foo, [foo/1]).

foo(Foo) :- bar.
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (should (equal (get-text-property (+ (point-min) 1)
                                      'font-lock-face)
                   '(sweeprolog-neck-default-face
                     sweeprolog-directive-default-face)))
    (should (equal (get-text-property (+ (point-min) 2)
                                      'font-lock-face)
                   '(sweeprolog-directive-default-face)))
    (should (equal (get-text-property (+ (point-min) 3)
                                      'font-lock-face)
                   '(sweeprolog-built-in-default-face
                     sweeprolog-body-default-face)))
    (should (equal (get-text-property (+ (point-min) 9)
                                      'font-lock-face)
                   '(sweeprolog-body-default-face)))
    (should (equal (get-text-property (+ (point-min) 10)
                                      'font-lock-face)
                   '(sweeprolog-identifier-default-face
                     sweeprolog-body-default-face)))
    (should (equal (get-text-property (+ (point-min) 13)
                                      'font-lock-face)
                   '(sweeprolog-body-default-face)))
    (should (equal (get-text-property (+ (point-min) 16)
                                      'font-lock-face)
                   '(sweeprolog-local-default-face
                     sweeprolog-body-default-face)))
    (should (equal (get-text-property (+ (point-min) 23)
                                      'font-lock-face)
                   '(sweeprolog-fullstop-default-face)))
    (should (equal (get-text-property (+ (point-min) 26)
                                      'font-lock-face)
                   '(sweeprolog-head-exported-default-face
                     sweeprolog-clause-default-face)))
    (should (equal (get-text-property (+ (point-min) 31)
                                      'font-lock-face)
                   '(sweeprolog-singleton-default-face
                     sweeprolog-clause-default-face)))
    (should (equal (get-text-property (+ (point-min) 39)
                                      'font-lock-face)
                   '(sweeprolog-undefined-default-face
                     sweeprolog-body-default-face)))))

(ert-deftest insert-term-with-holes ()
  "Test `sweeprolog-insert-term-with-holes'."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (sweeprolog-insert-term-with-holes ":-" 2)
    (call-interactively #'kill-region)
    (sweeprolog-insert-term-with-holes "foo" 3)
    (call-interactively #'kill-region)
    (sweeprolog-insert-term-with-holes "bar" 0)
    (call-interactively #'kill-region)
    (sweeprolog-insert-term-with-holes ";" 2)
    (call-interactively #'kill-region)
    (sweeprolog-insert-term-with-holes "->" 2)
    (should (string= (buffer-string)
                     "foo(bar, (_->_;_), _):-_."))))

(ert-deftest plunit-testset-skeleton ()
  "Tests inserting PlUnit test-set blocks."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (sweeprolog-plunit-testset-skeleton "foo")
    (should (string= (buffer-string)
                     ":- begin_tests(foo).

test() :- TestBody.

:- end_tests(foo).
"
                     ))))

(ert-deftest auto-insert-module-header ()
  "Tests inserting Prolog module header with `auto-insert'."
  (find-file-literally (expand-file-name "sweeprolog_test_auto_insert.pl"
                                         temporary-file-directory))
  (sweeprolog-mode)
  (let ((auto-insert-query nil))
    (call-interactively #'auto-insert))
  (let ((end (point)))
    (beginning-of-line -1)
    (should (string= (buffer-substring-no-properties (point) end)
                     ":- module(sweeprolog_test_auto_insert, []).

/** <module> "))))

(ert-deftest complete-atom ()
  "Tests completing atoms."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
baz(Baz) :- Baz = opa
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-char)
    (call-interactively #'completion-at-point)
    (should (string= (buffer-string)
                     "
baz(Baz) :- Baz = opaque
"
                     ))))

(ert-deftest complete-non-terminal ()
  "Tests completing DCG non-terminals."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
barbaz --> foo.

foo --> barb"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (call-interactively #'completion-at-point)
    (should (string= (buffer-string)
                     "
barbaz --> foo.

foo --> barbaz"

                     ))
    (insert ".\n\nfoo => barb")
    (call-interactively #'completion-at-point)
    (should (string= (buffer-string)
                     "
barbaz --> foo.

foo --> barbaz.

foo => barbaz(_, _)"

                     ))))

(ert-deftest complete-predicate-with-args ()
  "Tests completing predicate calls."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
:- module(foobarbaz, []).

%!  foobarbaz(:Bar, ?Baz:integer) is det.

foobarbaz(_, 5) :- spam.

spam :- foobarb
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-char)
    (call-interactively #'completion-at-point)
    (should (string= (buffer-string)
                     "
:- module(foobarbaz, []).

%!  foobarbaz(:Bar, ?Baz:integer) is det.

foobarbaz(_, 5) :- spam.

spam :- foobarbaz(Bar, Baz)
"
                     ))))

(ert-deftest complete-predicate ()
  "Tests completing predicate calls."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
baz(Baz) :- findall(X, b_g
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-char)
    (call-interactively #'completion-at-point)
    (should (string= (buffer-string)
                     "
baz(Baz) :- findall(X, b_getval(_, _)
"
                     ))))

(ert-deftest complete-variable ()
  "Tests completing variable names."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
baz(Baz) :- bar(B).
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (forward-word)
    (call-interactively #'completion-at-point)
    (should (string= (buffer-string)
                     "
baz(Baz) :- bar(Baz).
"
                     ))))

(ert-deftest mark-predicate ()
  "Test marking predicate definition."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
:- module(baz, []).


%!  baz(-Baz) is semidet.
%
%   Foobar.

baz(Baz) :- bar(Baz).
baz(_) :- false.

%!  bar(-Bar) is semidet.
%
%   Spam.

bar(Bar) :- baz(Bar).
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (call-interactively #'sweeprolog-mark-predicate)
    (should (= (point) 24))
    (should (= (mark) 104))))

(ert-deftest export-predicate-with-comment-header ()
  "Test exporting a predicate after a comment header."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "/*
Sed id ligula quis est convallis tempor.  Nam vestibulum accumsan
nisl.  Sed diam.  Pellentesque tristique imperdiet tortor.  Fusce
sagittis, libero non molestie mollis, magna orci ultrices dolor,
at vulputate neque nulla lacinia eros.
*/
:- module(sweeprolog_test_export_predicate, []).

%!  foo(+Bar) is det.

foo(Bar) :- bar(Bar).
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (call-interactively #'sweeprolog-export-predicate)
    (should (equal (buffer-string)
                              "/*
Sed id ligula quis est convallis tempor.  Nam vestibulum accumsan
nisl.  Sed diam.  Pellentesque tristique imperdiet tortor.  Fusce
sagittis, libero non molestie mollis, magna orci ultrices dolor,
at vulputate neque nulla lacinia eros.
*/
:- module(sweeprolog_test_export_predicate, [foo/1  % +Bar
                                            ]).

%!  foo(+Bar) is det.

foo(Bar) :- bar(Bar).
"))))

(ert-deftest export-predicate ()
  "Test exporting a predicate."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
:- module(sweeprolog_test_export_predicate, []).

%!  foo(+Bar) is det.

foo(Bar) :- bar(Bar).
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (call-interactively #'sweeprolog-export-predicate)
    (should (equal (buffer-string)
                   "
:- module(sweeprolog_test_export_predicate, [foo/1  % +Bar
                                            ]).

%!  foo(+Bar) is det.

foo(Bar) :- bar(Bar).
"))))

(ert-deftest export-predicate-with-op ()
  "Test exporting a predicate in presence of an exported operator."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
:- module(tester,
          [ instantiate_test_template/4,  % +In,+Replacement,-Dict,-Map
            op(200, fy, @)		  % @name
          ]).

%!  foo(+Bar) is det.

foo(Bar).
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (call-interactively #'sweeprolog-export-predicate)
    (should (equal (buffer-string)
                  "
:- module(tester,
          [ instantiate_test_template/4, % +In,+Replacement,-Dict,-Map
            foo/1,                       % +Bar
            op(200, fy, @)		 % @name
          ]).

%!  foo(+Bar) is det.

foo(Bar).
"
                  ))))

(ert-deftest export-predicate-with-only-op ()
  "Test exporting a predicate in presence of only exported operators."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
:- module(tester,
          [ op(200, fy, @)		  % @name
          ]).

%!  foo(+Bar) is det.

foo(Bar).
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (call-interactively #'sweeprolog-export-predicate)
    (should (equal (buffer-string)
                  "
:- module(tester,
          [ foo/1,         % +Bar
            op(200, fy, @) % @name
          ]).

%!  foo(+Bar) is det.

foo(Bar).
"
                  ))))

(ert-deftest identifier-at-point ()
  "Test recognizing predicate invocations."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "foo(Bar) :- bar(Bar).")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (should (equal (sweeprolog-identifier-at-point)
                   "bar/1"))))

(ert-deftest definition-at-point ()
  "Test recognizing predicate defintions."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "foo(Bar) :- bar(Bar).")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (should (equal (sweeprolog-definition-at-point)
                   '(1 "foo" 1 21 ":-" nil)))))

(ert-deftest syntax-errors ()
  "Test clearing syntax error face after errors are fixed."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
:- module(baz, []).


%!  baz(-Baz) is semidet.
%
%   Foobar.

baz(Baz) :- bar(Baz).
baz(Baz) :- Bar, Baz.

%!  bar(-Bar) is semidet.
%
%   Spam.

bar(Bar) :- baz(Bar).

% comment before eob...
")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-min))
    (search-forward ".\n" nil t)
    (replace-match ",,\n" nil t)
    (delete-char -3)
    (redisplay)
    (insert ".")
    (redisplay)
    (should (= (point-max)
               (prop-match-end
                (text-property-search-forward
                 'font-lock-face
                 '(sweeprolog-syntax-error-default-face
                   sweeprolog-around-syntax-error-default-face)))))))

(ert-deftest file-at-point ()
  "Test recognizing file specifications."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              ":- use_module(library(lists)).")))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word)
    (let ((fsap (sweeprolog-file-at-point)))
      (should fsap)
      (should (string= "lists" (file-name-base fsap))))))

(ert-deftest dwim-next-clause-fact ()
  "Tests inserting a new clause after a fact."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo.")
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                     "
foo.
foo :- Body.
"))))

(ert-deftest dwim-next-clause-module-qualified-dcg ()
  "Tests inserting new module-qualified DCG non-terminal."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
spam:foo --> bar.
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                     "
spam:foo --> bar.
spam:foo --> Body.

"
                     ))))

(ert-deftest dwim-next-clause-args ()
  "Tests inserting new clause with arguments."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
%!  foo(+Bar) is det.

foo(bar) :- bar.
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                              "
%!  foo(+Bar) is det.

foo(bar) :- bar.
foo(Bar) :- Body.

"))))

(ert-deftest dwim-next-clause-module-qualified ()
  "Tests inserting new module-qualified clause."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
spam:foo :- bar.
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                     "
spam:foo :- bar.
spam:foo :- Body.

"
                     ))))

(ert-deftest dwim-next-clause-prolog-message ()
  "Tests inserting new `prolog:message/1' clause."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
prolog:message(foo(bar, Baz, Spam)) -->
    [ 'baz: ~D spam: ~w'-[Baz, Spam] ].
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                     "
prolog:message(foo(bar, Baz, Spam)) -->
    [ 'baz: ~D spam: ~w'-[Baz, Spam] ].
prolog:message(_) --> Body.

"
                     ))))

(ert-deftest dwim-next-clause-dcg ()
  "Tests inserting a non-terminal with `sweeprolog-insert-term-dwim'."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo --> bar.")
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                     "
foo --> bar.
foo --> Body.
"))))

(ert-deftest dwim-next-clause-ssu ()
  "Tests inserting an SSU rule with `sweeprolog-insert-term-dwim'."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo => bar.")
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                     "
foo => bar.
foo => Body.
"))))

(ert-deftest dwim-next-clause ()
  "Tests inserting a new clause with `sweeprolog-insert-term-dwim'."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo :- bar.")
    (sweeprolog-insert-term-dwim)
    (should (string= (buffer-string)
                     "
foo :- bar.
foo :- Body.
"))))

(ert-deftest update-dependencies-no-autoload ()
  "Tests making adding a use_module/1 directive."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
:- module(foo, [bar/1]).

/** <module> Foo

*/

bar(X) :- arithmetic_function(X).
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (call-interactively #'sweeprolog-update-dependencies)
    (should (string= (buffer-string)
                              "
:- module(foo, [bar/1]).

/** <module> Foo

*/

:- use_module(library(arithmetic), [ arithmetic_function/1
                                   ]).

bar(X) :- arithmetic_function(X).
"))))

(ert-deftest append-dependencies ()
  "Tests making implicit autoloads explicit with existing directive."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
:- module(foo, [bar/1]).

/** <module> Foo

*/

:- use_module(library(lists), [ member/2
                              ]).

bar(X) :- member(X, [1,2,3]).
bar(X) :- permutation(X, [1,2,3]).
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (call-interactively #'sweeprolog-update-dependencies)
    (should (string= (buffer-string)
                     "
:- module(foo, [bar/1]).

/** <module> Foo

*/

:- use_module(library(lists), [ member/2,
                                permutation/2
                              ]).

bar(X) :- member(X, [1,2,3]).
bar(X) :- permutation(X, [1,2,3]).
"
                     ))))

(ert-deftest update-dependencies-autoload-from-package ()
  "Tests making implicit autoloads from a package explicit."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
:- module(foo, [bar/1]).

/** <module> Foo

*/

bar(X) :- http_open(X, X, X).
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (call-interactively #'sweeprolog-update-dependencies)
    (should (string= (buffer-string)
                     "
:- module(foo, [bar/1]).

/** <module> Foo

*/

:- autoload(library(http/http_open), [ http_open/3
                                     ]).

bar(X) :- http_open(X, X, X).
"))))

(ert-deftest update-dependencies ()
  "Tests making implicit autoloads explicit."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
:- module(foo, [bar/1]).

/** <module> Foo

*/

bar(X) :- member(X, [1,2,3]).
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (call-interactively #'sweeprolog-update-dependencies)
    (should (string= (buffer-string)
                     "
:- module(foo, [bar/1]).

/** <module> Foo

*/

:- autoload(library(lists), [ member/2
                            ]).

bar(X) :- member(X, [1,2,3]).
"

                     ))
    (goto-char (point-max))
    (insert "bar(X) :- permutation(X, [1,2,3]).")
    (call-interactively #'sweeprolog-update-dependencies)
    (should (string= (buffer-string)
                     "
:- module(foo, [bar/1]).

/** <module> Foo

*/

:- autoload(library(lists), [ member/2,
                              permutation/2
                            ]).

bar(X) :- member(X, [1,2,3]).
bar(X) :- permutation(X, [1,2,3])."))))

(ert-deftest dwim-define-nested-phrase ()
  "Tests complex undefined predicate scenario."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
foo --> {baz, phrase(bar, Baz)}.
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word 2)
    (sweeprolog-insert-term-dwim)
    (call-interactively #'kill-region)
    (insert "foo")
    (should (string= (buffer-string)
                     "
foo --> {baz, phrase(bar, Baz)}.

bar --> foo.
"
                     ))))

(ert-deftest dwim-define-phrase-non-terminal ()
  "Tests defining an undefined DCG non-terminal from a clause."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              "pl"
                              "
foo :- phrase(bar, Baz).
"
                              )))
    (find-file-literally temp)
    (sweeprolog-mode)
    (goto-char (point-max))
    (backward-word 2)
    (sweeprolog-insert-term-dwim)
    (call-interactively #'kill-region)
    (insert "foo")
    (should (string= (buffer-string)
                     "
foo :- phrase(bar, Baz).

bar --> foo.
"
                     ))))

(ert-deftest dwim-define-braces-predicate ()
  "Tests defining an undefined predicate from a DCG non-terminal."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo --> {bar}.
")
    (backward-word)
    (sweeprolog-insert-term-dwim)
    (call-interactively #'kill-region)
    (insert "foo")
    (should (string= (buffer-string)
                     "
foo --> {bar}.

bar :- foo.
"
                     ))))

(ert-deftest dwim-define-non-terminal ()
  "Tests defining an undefined DCG non-terminal."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo --> bar.
")
    (backward-word)
    (sweeprolog-insert-term-dwim)
    (call-interactively #'kill-region)
    (insert "foo")
    (should (string= (buffer-string)
                     "
foo --> bar.

bar --> foo.
"
                     ))))

(ert-deftest dwim-define-predicate ()
  "Tests defining a new predicate with `sweeprolog-insert-term-dwim'."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo :- bar.
")
    (backward-word)
    (sweeprolog-insert-term-dwim)
    (call-interactively #'kill-region)
    (insert "foo")
    (should (string= (buffer-string)
                     "
foo :- bar.

bar :- foo.
"
                     ))))


(ert-deftest dwim-define-predicate-above ()
  "Tests adherence to `sweeprolog-new-predicate-location-function'."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
%!  foo is det.

foo :- bar.
")
    (backward-word)
    (let ((sweeprolog-new-predicate-location-function
           #'sweeprolog-new-predicate-location-above-current))
      (sweeprolog-insert-term-dwim))
    (call-interactively #'kill-region)
    (insert "foo")
    (should (string= (buffer-string)
                     "
bar :- foo.

%!  foo is det.

foo :- bar.
"
                     ))))

(ert-deftest end-of-top-term-with-univ ()
  "Tests detecting the fullstop in presence of `=..'."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
html_program_section(Section, Dict) -->
    { _{module:M, options:Options} :< Dict,
      Content = Dict.get(Section),
      Content \= [],
      scasp_code_section_title(Section, Default, Title),
      Opt =.. [Section,true],
      option(Opt, Options, Default)
    },
    !,
    html(h2(Title)),
    (   {Section == query}
    ->  {ovar_set_bindings(Dict.bindings)},
        html_query(M:Content, Options)
    ;   sequence(predicate_r(M:Options), Content)
    ).
")
    (goto-char (point-min))
    (sweeprolog-end-of-top-term)
    (should (= (point) 466))))


(ert-deftest fullstop-detection ()
  "Tests detecting the fullstop in presence of confusing comments."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
scasp_and_show(Q, Model, Tree) :-
    scasp_mode(M0, T0),
    setup_call_cleanup(
        set_scasp_mode(Model, Tree),
        (   scasp(Q, [])
        ;   false                       % make always nondet.
        ),
        set_scasp_mode(M0, T0)).
")
    (goto-char (point-min))
    (sweeprolog-end-of-top-term)
    (should (= (point) 252))))

(ert-deftest beginning-of-predicate-definition-near-bob ()
  "Test finding the beginning of the first predicate definition."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "foo :- bar.")
    (goto-char (point-min))
    (sweeprolog-beginning-of-predicate-at-point)
    (should (= (point) (point-min)))))

(ert-deftest align-spacs-in-line-comment ()
  "Test using `sweeprolog-align-spaces' in a line comment."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
%!  foo is det.
%
%")
    (sweeprolog-align-spaces)
    (should (string= (buffer-string)
                     "
%!  foo is det.
%
%   "))))

(ert-deftest auto-fill-pldoc-comments ()
  "Test writing PlDoc comments with `auto-fill-mode' enable."
  (with-temp-buffer
    (sweeprolog-mode)
    (auto-fill-mode)
    (seq-do (lambda (c)
              (let ((last-command-event c))
                (call-interactively #'self-insert-command)))
            "
%!  foobar is det.
%
%   Nam vestibulum accumsan nisl.  Donec pretium posuere tellus.  Aenean in sem ac leo mollis blandit.  Nam a sapien.  Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus.
"
            )
    (should (string= (buffer-string)
                     "
%!  foobar is det.
%
%   Nam vestibulum accumsan nisl.  Donec pretium posuere tellus.
%   Aenean in sem ac leo mollis blandit.  Nam a sapien.  Proin quam
%   nisl, tincidunt et, mattis eget, convallis nec, purus.
"))))

(ert-deftest electric-layout ()
  "Test `sweeprolog-electric-layout-mode'."
  (with-temp-buffer
    (sweeprolog-mode)
    (sweeprolog-electric-layout-mode)
    (seq-do (lambda (c)
              (let ((last-command-event c))
                (call-interactively #'self-insert-command)))
            "
foobar :-
(bar
;baz
->spam
).
")
    (should (string= (buffer-string)
                     "
foobar :-
    (   bar
    ;   baz
    ->  spam
    ).
"
                     ))))

(ert-deftest end-of-top-term-with-other-symbols ()
  "Tests detecting the fullstop in presence of `.=.'."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
loop_term(I, Arity, Goal1, Goal2) :-
    I =< Arity,
    arg(I, Goal1, A),
    arg(I, Goal2, B),
    (   loop_var_disequality(A,B)
    ->  true
    ;   A .=. B,
        I2 is I+1,
        loop_term(I2, Arity, Goal1, Goal2)
    ).
")
    (goto-char (point-min))
    (sweeprolog-end-of-top-term)
    (should (= (point) 232))))

(ert-deftest beginning-of-next-top-term-at-last-clause ()
  "Test finding the beginning of next top term when there isn't one."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
test_bindings(Name-Value) -->
    ['    '~w = ~p'-[Name-Value] ]..
")
    (goto-char 43)
    (backward-delete-char 1)
    (end-of-line)
    (backward-delete-char 1)
    (should (string= (buffer-string) "
test_bindings(Name-Value) -->
    ['    ~w = ~p'-[Name-Value] ].
"
                     ))))

(ert-deftest infer-indent-style ()
  "Test inferring indentation style from buffer contents."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo :-
  bar.")
    (sweeprolog-infer-indent-style)
    (should (= sweeprolog-indent-offset 2))
    (should (not indent-tabs-mode)))
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo :-
\tbar.")
    (sweeprolog-infer-indent-style)
    (should (= sweeprolog-indent-offset tab-width))
    (should indent-tabs-mode)))

(ert-deftest custom-indentation ()
  "Test forcefully setting custom indentation levels."
  (with-temp-buffer
    (sweeprolog-mode)
    (insert "
foo :-
    repeat,
      bar,
      baz")
    (call-interactively #'indent-for-tab-command)
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "
foo :-
    repeat,
      bar,
      baz"))))

(defun sweeprolog-test-indentation (given expected)
  (with-temp-buffer
    (sweeprolog-mode)
    (insert given)
    (let ((inhibit-message t))
      (indent-region-line-by-line (point-min) (point-max)))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     expected))))

(defun sweeprolog-test-context-callable-p (given expected)
  (with-temp-buffer
    (sweeprolog-mode)
    (insert given)
    (should (equal expected (sweeprolog-context-callable-p)))))

(ert-deftest context-callable ()
  "Test recognizing callable contexts."
  (sweeprolog-test-context-callable-p "foo(Bar) :- include( " 1)
  (sweeprolog-test-context-callable-p "foo(Bar) --> " 2)
  (sweeprolog-test-context-callable-p "foo(Bar) --> {include(" 1)
  (sweeprolog-test-context-callable-p "foo(Bar) --> {include(phrase(" 2)
  (sweeprolog-test-context-callable-p "foo" nil)
  (sweeprolog-test-context-callable-p "foo(" nil)
  (sweeprolog-test-context-callable-p "foo(bar)" nil)
  (sweeprolog-test-context-callable-p "foo(bar) :- " 0)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(" nil)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar" nil)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar), " 0)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar), findall(" nil)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar), findall(X" nil)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar), findall(X," 0)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar), findall(X, false" 0)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar), findall(X, false," nil)
  (sweeprolog-test-context-callable-p "foo(bar) :- baz(bar), findall(X, false, Xs). " nil))

(ert-deftest indentation ()
  "Tests indentation rules."
  (sweeprolog-test-indentation
   "
colourise_declaration(Module:Goal, table, TB,
                      term_position(_,_,QF,QT,
"
   "
colourise_declaration(Module:Goal, table, TB,
                      term_position(_,_,QF,QT,
")
  (sweeprolog-test-indentation
   "
some_functor(
arg1,
arg2,
)."
   "
some_functor(
    arg1,
    arg2,
)."
   )
  (sweeprolog-test-indentation
   "
asserta( some_functor(arg1, arg2) :-
body_term
).
"
   "
asserta( some_functor(arg1, arg2) :-
             body_term
       ).
"
   )
  (sweeprolog-test-indentation
   "
:- module(spam, [ foo,
bar,
baz
]
).
"
   "
:- module(spam, [ foo,
                  bar,
                  baz
                ]
         ).
"
   )
  (sweeprolog-test-indentation
   "
:- module(spam, [
foo,
bar,
baz
]
).
"
   "
:- module(spam, [
                    foo,
                    bar,
                    baz
                ]
         ).
"
   )
  (sweeprolog-test-indentation
   "
[
    ].
"
   "
[
].
"
   )
  (sweeprolog-test-indentation
   "
:-
use_module(foo),
use_module(bar).
"
   "
:-
    use_module(foo),
    use_module(bar).
"
   )
  (sweeprolog-test-indentation
   "
colourise_declaration(Module:PI, _, TB,
                      term_position(_,_,QF,QT,[PM,PG])) :-
    atom(Module), nonvar(PI), PI = Name/Arity,
    !,                                  % partial predicate indicators
    colourise_module(Module, TB, PM),
    colour_item(functor, TB, QF-QT),
    (   (var(Name) ; atom(Name)),
        (var(Arity) ; integer(Arity),
                      Arity >= 0)
    ->  colourise_term_arg(PI, TB, PG)
    ;   colour_item(type_error(predicate_indicator), TB, PG)
    ).
"
   "
colourise_declaration(Module:PI, _, TB,
                      term_position(_,_,QF,QT,[PM,PG])) :-
    atom(Module), nonvar(PI), PI = Name/Arity,
    !,                                  % partial predicate indicators
    colourise_module(Module, TB, PM),
    colour_item(functor, TB, QF-QT),
    (   (var(Name) ; atom(Name)),
        (var(Arity) ; integer(Arity),
                      Arity >= 0)
    ->  colourise_term_arg(PI, TB, PG)
    ;   colour_item(type_error(predicate_indicator), TB, PG)
    ).
")
  (sweeprolog-test-indentation
   "
A is 1 * 2 + 3 *
4.
"
   "
A is 1 * 2 + 3 *
             4.
")
  (sweeprolog-test-indentation
   "
A is 1 * 2 ^ 3 *
4.
"
   "
A is 1 * 2 ^ 3 *
     4.
")
  (sweeprolog-test-indentation
   "
(   if
    ->  (   iff1, iff2, iff3,
iff4
->  thenn
;   elsee
)
        ;   else
            )
"
   "
(   if
->  (   iff1, iff2, iff3,
        iff4
    ->  thenn
    ;   elsee
    )
;   else
)
")
  (sweeprolog-test-indentation
   "
(   if
    ->  (   iff
->  thenn
;   elsee
)
        ;   else
            )
"
   "
(   if
->  (   iff
    ->  thenn
    ;   elsee
    )
;   else
)
")
  (sweeprolog-test-indentation
   "
(   if
    ;   then
        ->  else
            )
"
   "
(   if
;   then
->  else
)
")
  (sweeprolog-test-indentation
   "
asserta(   foo(bar, baz) :-
true).
"
   "
asserta(   foo(bar, baz) :-
               true).
")
  (sweeprolog-test-indentation
   "
foo(bar, baz) :-
true.
"
   "
foo(bar, baz) :-
    true.
")

  (sweeprolog-test-indentation
   "
:- multifile
foo/2.
"
   "
:- multifile
       foo/2.
")

  (sweeprolog-test-indentation
   "
    %%%%
    %%%%
"
   "
    %%%%
    %%%%
")

  (sweeprolog-test-indentation
   "
(
foo"
   "
(
    foo")
  (sweeprolog-test-indentation
   "
functor(
foo"
   "
functor(
    foo")
  (sweeprolog-test-indentation
   "
replace_key_value(Replacement, Key - AtVar, Out, Used0, Used1),
atom_concat(@, Var, AtVar) =>
foo.
"
   "
replace_key_value(Replacement, Key - AtVar, Out, Used0, Used1),
  atom_concat(@, Var, AtVar) =>
    foo.
"
   )
  (sweeprolog-test-indentation
   "
head,
right_hand_context -->
body.
"
   "
head,
  right_hand_context -->
    body.
"))

;;; sweeprolog-tests.el ends here
