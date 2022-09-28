;;; sweeprolog-tests.el --- ERT suite for sweep  -*- lexical-binding:t -*-

(require 'sweeprolog)

(ert-deftest lists:member/2 ()
  "Tests calling the Prolog predicate permutation/2 from Elisp."
  (should (equal (sweeprolog-open-query "user" "lists" "member" (list 1 2 3) t) t))
  (should (equal (sweeprolog-next-solution) (cons t 1)))
  (should (equal (sweeprolog-next-solution) (cons t 2)))
  (should (equal (sweeprolog-next-solution) (cons '! 3)))
  (should (equal (sweeprolog-cut-query) t)))

(ert-deftest lists:permutation/2 ()
  "Tests calling the Prolog predicate permutation/2 from Elisp."
  (should (equal (sweeprolog-open-query "user" "lists" "permutation" (list 1 2 3)) t))
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
  (should (equal (sweeprolog-open-query "user" "system" "=" (list 1 nil (list "foo" "bar") 3.14)) t))
  (should (equal (sweeprolog-next-solution) (list '! 1 nil (list "foo" "bar") 3.14)))
  (should (equal (sweeprolog-next-solution) nil))
  (should (equal (sweeprolog-cut-query) t)))


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
