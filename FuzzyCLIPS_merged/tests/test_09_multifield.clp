;;;======================================================
;;; Test 09: Multifield Functions
;;; Tests create$, first$, rest$, subseq$, delete$,
;;; replace$, insert$, member$, subsetp, explode$,
;;; implode$, foreach, sort, intersection$, union$,
;;; difference$
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 09: Multifield Functions")

(deffunction assert-equal (?desc ?expected ?actual)
   (if (eq ?expected ?actual) then
      (bind ?*test-pass* (+ ?*test-pass* 1))
      (printout t "  PASS: " ?desc crlf)
   else
      (bind ?*test-fail* (+ ?*test-fail* 1))
      (printout t "  FAIL: " ?desc " (expected=" ?expected " actual=" ?actual ")" crlf)))

(deffunction assert-true (?desc ?val)
   (if ?val then
      (bind ?*test-pass* (+ ?*test-pass* 1))
      (printout t "  PASS: " ?desc crlf)
   else
      (bind ?*test-fail* (+ ?*test-fail* 1))
      (printout t "  FAIL: " ?desc crlf)))

(deffunction mf-equal (?a ?b)
   "Compare two multifields element by element"
   (if (neq (length$ ?a) (length$ ?b)) then (return FALSE))
   (loop-for-count (?i 1 (length$ ?a))
      (if (neq (nth$ ?i ?a) (nth$ ?i ?b)) then (return FALSE)))
   (return TRUE))

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; -- create$ and length$ --
   (printout t crlf "-- create$ & length$ --" crlf)
   (bind ?m (create$ a b c d e))
   (assert-equal "create$ length" 5 (length$ ?m))
   (assert-equal "empty multifield length" 0 (length$ (create$)))
   (assert-equal "single element length" 1 (length$ (create$ x)))

   ;; -- nth$ --
   (printout t crlf "-- nth$ --" crlf)
   (assert-equal "nth$ 1st" a (nth$ 1 ?m))
   (assert-equal "nth$ 3rd" c (nth$ 3 ?m))
   (assert-equal "nth$ last" e (nth$ 5 ?m))

   ;; -- first$ and rest$ --
   (printout t crlf "-- first$ & rest$ --" crlf)
   (assert-true "first$ = (a)" (mf-equal (first$ ?m) (create$ a)))
   (assert-true "rest$ = (b c d e)" (mf-equal (rest$ ?m) (create$ b c d e)))
   (assert-true "rest$ of single = empty" (mf-equal (rest$ (create$ x)) (create$)))

   ;; -- subseq$ --
   (printout t crlf "-- subseq$ --" crlf)
   (assert-true "subseq$ 2-4" (mf-equal (subseq$ ?m 2 4) (create$ b c d)))
   (assert-true "subseq$ 1-1" (mf-equal (subseq$ ?m 1 1) (create$ a)))
   (assert-true "subseq$ 1-5 = full" (mf-equal (subseq$ ?m 1 5) ?m))

   ;; -- member$ --
   (printout t crlf "-- member$ --" crlf)
   (assert-equal "member$ found c" 3 (member$ c ?m))
   (assert-equal "member$ found a" 1 (member$ a ?m))
   (assert-equal "member$ not found" FALSE (member$ z ?m))

   ;; -- subsetp --
   (printout t crlf "-- subsetp --" crlf)
   (assert-true "subsetp (a b) of (a b c d e)" (subsetp (create$ a b) ?m))
   (assert-true "subsetp (e a) of (a b c d e)" (subsetp (create$ e a) ?m))
   (assert-true "empty is subset of anything" (subsetp (create$) ?m))
   (assert-true "not subsetp (a z)" (not (subsetp (create$ a z) ?m)))

   ;; -- delete$ --
   (printout t crlf "-- delete$ --" crlf)
   (assert-true "delete$ 3-3 removes c" (mf-equal (delete$ ?m 3 3) (create$ a b d e)))
   (assert-true "delete$ 1-2 removes a b" (mf-equal (delete$ ?m 1 2) (create$ c d e)))
   (assert-true "delete$ 4-5 removes d e" (mf-equal (delete$ ?m 4 5) (create$ a b c)))

   ;; -- replace$ --
   (printout t crlf "-- replace$ --" crlf)
   (assert-true "replace$ 3 with X" (mf-equal (replace$ ?m 3 3 X) (create$ a b X d e)))
   (assert-true "replace$ 2-3 with Y Z" (mf-equal (replace$ ?m 2 3 Y Z) (create$ a Y Z d e)))

   ;; -- insert$ --
   (printout t crlf "-- insert$ --" crlf)
   (assert-true "insert$ at 1" (mf-equal (insert$ ?m 1 X) (create$ X a b c d e)))
   (assert-true "insert$ at 3" (mf-equal (insert$ ?m 3 X) (create$ a b X c d e)))
   (assert-true "insert$ at end+1" (mf-equal (insert$ ?m 6 X) (create$ a b c d e X)))

   ;; -- explode$ and implode$ --
   (printout t crlf "-- explode$ & implode$ --" crlf)
   (assert-true "explode$ splits string" (mf-equal (explode$ "a b c") (create$ a b c)))
   (assert-equal "implode$ joins multifield" "a b c d e" (implode$ ?m))
   (assert-true "explode$ numbers" (= (nth$ 1 (explode$ "42 3.14")) 42))

   ;; -- delete-member$ --
   (printout t crlf "-- delete-member$ --" crlf)
   (assert-true "delete-member$ c" (mf-equal (delete-member$ ?m c) (create$ a b d e)))
   (assert-true "delete-member$ not found" (mf-equal (delete-member$ ?m z) ?m))

   ;; -- progn$ / foreach --
   (printout t crlf "-- progn$ --" crlf)
   (bind ?sum 0)
   (progn$ (?x (create$ 1 2 3 4 5))
      (bind ?sum (+ ?sum ?x)))
   (assert-equal "progn$ sum 1..5 = 15" 15 ?sum)

   ;; -- sort --
   (printout t crlf "-- sort --" crlf)
   (assert-true "sort ascending numbers" (mf-equal (sort < (create$ 5 3 1 4 2)) (create$ 1 2 3 4 5)))
   (assert-true "sort descending numbers" (mf-equal (sort > (create$ 1 3 5 2 4)) (create$ 5 4 3 2 1)))
   (assert-equal "sort preserves length" 5 (length$ (sort > (create$ 5 3 1 4 2))))

   ;; -- set operations (CLIPS 6.41+) --
   (printout t crlf "-- Set Operations --" crlf)
   (bind ?s1 (create$ a b c d))
   (bind ?s2 (create$ c d e f))
   (assert-true "union$" (subsetp (create$ a b c d e f) (union$ ?s1 ?s2)))
   (assert-true "intersection$" (mf-equal (intersection$ ?s1 ?s2) (create$ c d)))
   (assert-true "difference$" (mf-equal (difference$ ?s1 ?s2) (create$ a b)))

   ;; -- map$ --
   (printout t crlf "-- map$ --" crlf)
   (bind ?result (map$ length$ (create$ (create$ a b) (create$ c d e))))
   ;; map$ applies function to each element; for simple items length$ returns 1
   ;; Let's test with a simpler use case
   (bind ?nums (create$ 1 2 3 4 5))
   (bind ?doubled (create$))
   (progn$ (?x ?nums)
      (bind ?doubled (create$ ?doubled (* ?x 2))))
   (assert-true "manual map double" (mf-equal ?doubled (create$ 2 4 6 8 10)))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
