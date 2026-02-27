;;;======================================================
;;; Test 06: Procedural Functions
;;; Tests if/then/else, while, loop-for-count, switch,
;;; progn, bind, break, return
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 06: Procedural Functions")

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

(deffunction approx-equal (?a ?b ?tol)
   (< (abs (- ?a ?b)) ?tol))

;;; ---- Helper deffunctions ----

(deffunction sum-to-n (?n)
   "Sum integers from 1 to n using while loop"
   (bind ?i 1)
   (bind ?sum 0)
   (while (<= ?i ?n)
      (bind ?sum (+ ?sum ?i))
      (bind ?i (+ ?i 1)))
   (return ?sum))

(deffunction sum-loop-for-count (?n)
   "Sum integers from 1 to n using loop-for-count"
   (bind ?sum 0)
   (loop-for-count (?i 1 ?n)
      (bind ?sum (+ ?sum ?i)))
   (return ?sum))

(deffunction early-return-test (?x)
   "Return early if x < 0"
   (if (< ?x 0) then (return negative))
   (if (= ?x 0) then (return zero))
   (return positive))

(deffunction break-test ()
   "Sum until reaching 5 using break"
   (bind ?sum 0)
   (bind ?i 0)
   (while TRUE
      (bind ?i (+ ?i 1))
      (if (> ?i 5) then (break))
      (bind ?sum (+ ?sum ?i)))
   (return ?sum))

(deffunction switch-test (?day)
   "Classify day using switch"
   (switch ?day
      (case monday then weekday)
      (case tuesday then weekday)
      (case wednesday then weekday)
      (case thursday then weekday)
      (case friday then weekday)
      (case saturday then weekend)
      (case sunday then weekend)
      (default unknown)))

(deffunction progn-test ()
   "Progn returns last value"
   (progn
      (bind ?a 10)
      (bind ?b 20)
      (+ ?a ?b)))

(deffunction nested-if-test (?x ?y)
   "Nested if/then/else classification"
   (if (> ?x 0) then
      (if (> ?y 0) then both-positive
       else x-positive-only)
   else
      (if (> ?y 0) then y-positive-only
       else both-nonpositive)))

(deffunction collatz-steps (?n)
   "Count Collatz conjecture steps to reach 1"
   (bind ?steps 0)
   (while (!= ?n 1)
      (if (= (mod ?n 2) 0) then
         (bind ?n (div ?n 2))
       else
         (bind ?n (+ (* 3 ?n) 1)))
      (bind ?steps (+ ?steps 1)))
   (return ?steps))

(deffunction fizzbuzz (?n)
   "FizzBuzz classification"
   (if (= (mod ?n 15) 0) then fizzbuzz
    else (if (= (mod ?n 3) 0) then fizz
    else (if (= (mod ?n 5) 0) then buzz
    else ?n))))

;;; --- Run tests ---
(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; -- if/then/else --
   (printout t crlf "-- if/then/else --" crlf)
   (assert-equal "if true branch" yes (if TRUE then yes else no))
   (assert-equal "if false branch" no (if FALSE then yes else no))
   (assert-equal "nested if both pos" both-positive (nested-if-test 1 1))
   (assert-equal "nested if x-only pos" x-positive-only (nested-if-test 1 -1))
   (assert-equal "nested if y-only pos" y-positive-only (nested-if-test -1 1))
   (assert-equal "nested if both nonpos" both-nonpositive (nested-if-test -1 -1))

   ;; -- while loop --
   (printout t crlf "-- while --" crlf)
   (assert-equal "while sum 1..10 = 55" 55 (sum-to-n 10))
   (assert-equal "while sum 1..100 = 5050" 5050 (sum-to-n 100))
   (assert-equal "while sum 1..0 = 0" 0 (sum-to-n 0))

   ;; -- loop-for-count --
   (printout t crlf "-- loop-for-count --" crlf)
   (assert-equal "loop-for-count 1..10 = 55" 55 (sum-loop-for-count 10))
   (assert-equal "loop-for-count 1..100 = 5050" 5050 (sum-loop-for-count 100))

   ;; -- return --
   (printout t crlf "-- return --" crlf)
   (assert-equal "early return negative" negative (early-return-test -5))
   (assert-equal "early return zero" zero (early-return-test 0))
   (assert-equal "early return positive" positive (early-return-test 5))

   ;; -- break --
   (printout t crlf "-- break --" crlf)
   (assert-equal "break exits while at 5" 15 (break-test))

   ;; -- switch --
   (printout t crlf "-- switch --" crlf)
   (assert-equal "switch monday=weekday" weekday (switch-test monday))
   (assert-equal "switch saturday=weekend" weekend (switch-test saturday))
   (assert-equal "switch sunday=weekend" weekend (switch-test sunday))
   (assert-equal "switch holiday=unknown" unknown (switch-test holiday))

   ;; -- progn --
   (printout t crlf "-- progn --" crlf)
   (assert-equal "progn returns 30" 30 (progn-test))

   ;; -- complex algorithms --
   (printout t crlf "-- Complex Algorithms --" crlf)
   (assert-equal "collatz(1) = 0 steps" 0 (collatz-steps 1))
   (assert-equal "collatz(6) = 8 steps" 8 (collatz-steps 6))
   (assert-equal "collatz(27) = 111 steps" 111 (collatz-steps 27))

   ;; -- fizzbuzz --
   (assert-equal "fizzbuzz(1) = 1" 1 (fizzbuzz 1))
   (assert-equal "fizzbuzz(3) = fizz" fizz (fizzbuzz 3))
   (assert-equal "fizzbuzz(5) = buzz" buzz (fizzbuzz 5))
   (assert-equal "fizzbuzz(15) = fizzbuzz" fizzbuzz (fizzbuzz 15))

   ;; -- bind scope --
   (printout t crlf "-- bind scoping --" crlf)
   (bind ?local-var 42)
   (assert-equal "bind local var" 42 ?local-var)
   (bind ?local-var (+ ?local-var 8))
   (assert-equal "rebind local var" 50 ?local-var)

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
