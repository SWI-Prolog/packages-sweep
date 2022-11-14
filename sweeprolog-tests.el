;;; sweeprolog-tests.el --- ERT suite for sweep  -*- lexical-binding:t -*-

(require 'sweeprolog)

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
  (message sweeprolog-tests-greeting))

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
                     sweeprolog-predicate-indicator-default-face
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

(ert-deftest export-predicate ()
  "Test exporting a predicate."
  (let ((temp (make-temp-file "sweeprolog-test"
                              nil
                              ".pl"
                              "
:- module(sweeprolog_test_export_predicate, []).

%!  foo(+Bar) is det

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

%!  foo(+Bar) is det

foo(Bar) :- bar(Bar).
"))))

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
                   "user:bar/1"))))

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
                   '(1 "foo" 1 21)))))

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
foo :- _.
"))))

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
"))))


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
"))))

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


(defun sweeprolog-test-indentation (given expected)
  (with-temp-buffer
    (sweeprolog-mode)
    (insert given)
    (indent-region-line-by-line (point-min) (point-max))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     expected))))

(ert-deftest indentation ()
  "Tests indentation rules."
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
  )

;;; sweeprolog-tests.el ends here
