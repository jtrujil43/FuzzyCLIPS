;;;======================================================
;;; Test 01: Basic CLIPS Functionality
;;; Verifies core CLIPS engine works after merge
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 01: Basic CLIPS Functionality")

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

;;; --- Templates/rules for fact system test ---
(deftemplate person
   (slot name (type SYMBOL))
   (slot age (type INTEGER)))

(defrule find-person
   (person (name ?n) (age ?a))
   =>
   (assert (found ?n ?a)))

(deffacts initial-facts
   (person (name Alice) (age 30))
   (person (name Bob) (age 25)))

;;; --- Run tests ---
(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; Test 1: math operations
   (assert-equal "addition 2+3=5" 5 (+ 2 3))
   (assert-equal "multiplication 4*5=20" 20 (* 4 5))
   (assert-equal "division 10/2=5.0" 5.0 (/ 10 2))
   (assert-equal "modulus 7 mod 3 = 1" 1 (mod 7 3))
   (assert-equal "abs -5 = 5" 5 (abs -5))
   (assert-equal "max 3 7 = 7" 7 (max 3 7))
   (assert-equal "min 3 7 = 3" 3 (min 3 7))

   ;; Test 2: string operations
   (assert-equal "str-cat" "hello world" (str-cat "hello" " " "world"))
   (assert-equal "str-length" 5 (str-length "hello"))
   (assert-equal "upcase" "HELLO" (upcase "hello"))
   (assert-equal "lowcase" "hello" (lowcase "HELLO"))

   ;; Test 3: type predicates
   (assert-equal "symbolp" TRUE (symbolp abc))
   (assert-equal "numberp" TRUE (numberp 42))
   (assert-equal "stringp" TRUE (stringp "test"))
   (assert-equal "integerp" TRUE (integerp 5))
   (assert-equal "floatp" TRUE (floatp 5.0))

   ;; Test 4: list operations
   (assert-equal "create$ length" 3 (length$ (create$ a b c)))
   (assert-equal "nth$ 2nd element" b (nth$ 2 (create$ a b c)))
   (assert-equal "member$" 2 (member$ b (create$ a b c)))

   ;; Test 5: fact system (reset then check)
   ;; Save current counts before reset
   (bind ?p ?*test-pass*)
   (bind ?f ?*test-fail*)
   (reset)
   ;; Restore counts (reset zeros the defglobals)
   (bind ?*test-pass* ?p)
   (bind ?*test-fail* ?f)
   (run)
   (assert-true "facts exist after reset+run"
      (> (length$ (find-all-facts ((?f person)) TRUE)) 0))
   (assert-true "found facts created by rule"
      (> (length$ (find-all-facts ((?f found)) TRUE)) 0))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
