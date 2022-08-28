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
