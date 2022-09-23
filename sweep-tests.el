;;; sweep-tests.el --- ERT suite for sweep  -*- lexical-binding:t -*-

(ert-deftest lists:member/2 ()
  "Tests calling the Prolog predicate permutation/2 from Elisp."
  (should (equal (sweep-open-query "user" "lists" "member" (list 1 2 3) t) t))
  (should (equal (sweep-next-solution) (cons t 1)))
  (should (equal (sweep-next-solution) (cons t 2)))
  (should (equal (sweep-next-solution) (cons '! 3)))
  (should (equal (sweep-cut-query) t)))

(ert-deftest lists:permutation/2 ()
  "Tests calling the Prolog predicate permutation/2 from Elisp."
  (should (equal (sweep-open-query "user" "lists" "permutation" (list 1 2 3)) t))
  (should (equal (sweep-next-solution) (list t 1 2 3)))
  (should (equal (sweep-next-solution) (list t 1 3 2)))
  (should (equal (sweep-next-solution) (list t 2 1 3)))
  (should (equal (sweep-next-solution) (list t 2 3 1)))
  (should (equal (sweep-next-solution) (list t 3 1 2)))
  (should (equal (sweep-next-solution) (list t 3 2 1)))
  (should (equal (sweep-next-solution) nil))
  (should (equal (sweep-cut-query) t)))

(ert-deftest system:=/2 ()
  "Tests unifying Prolog terms with =/2 from Elisp."
  (should (equal (sweep-open-query "user" "system" "=" (list 1 nil (list "foo" "bar") 3.14)) t))
  (should (equal (sweep-next-solution) (list '! 1 nil (list "foo" "bar") 3.14)))
  (should (equal (sweep-next-solution) nil))
  (should (equal (sweep-cut-query) t)))


(defun sweep-test-indentation (given expected)
  (with-temp-buffer
    (sweep-mode)
    (insert given)
    (indent-region-line-by-line (point-min) (point-max))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     expected))))

(ert-deftest indentation ()
  "Tests indentation rules."
  (sweep-test-indentation
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
  (sweep-test-indentation
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
  (sweep-test-indentation
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
  (sweep-test-indentation
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
  (sweep-test-indentation
   "
[
    ].
"
   "
[
].
"
   )
  (sweep-test-indentation
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
  (sweep-test-indentation
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
  (sweep-test-indentation
   "
A is 1 * 2 + 3 *
4.
"
   "
A is 1 * 2 + 3 *
             4.
")
  (sweep-test-indentation
   "
A is 1 * 2 ^ 3 *
4.
"
   "
A is 1 * 2 ^ 3 *
     4.
")
  (sweep-test-indentation
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
  (sweep-test-indentation
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
  (sweep-test-indentation
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
  (sweep-test-indentation
   "
asserta(   foo(bar, baz) :-
true).
"
   "
asserta(   foo(bar, baz) :-
               true).
")
  (sweep-test-indentation
   "
foo(bar, baz) :-
true.
"
   "
foo(bar, baz) :-
    true.
")

  (sweep-test-indentation
   "
:- multifile
foo/2.
"
   "
:- multifile
       foo/2.
")

  (sweep-test-indentation
   "
    %%%%
    %%%%
"
   "
    %%%%
    %%%%
")

  (sweep-test-indentation
   "
(
foo"
   "
(
    foo")
  (sweep-test-indentation
   "
functor(
foo"
   "
functor(
    foo")
  )

;;; sweep-tests.el ends here
