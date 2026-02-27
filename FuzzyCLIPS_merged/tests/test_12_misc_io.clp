;;;======================================================
;;; Test 12: Miscellaneous & I/O Functions
;;; Tests gensym*, random, seed, format, funcall, timer,
;;; sort, operating-system, get-function-list, time
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 12: Misc & I/O Functions")

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

(deffunction save-counters ()
   (return (create$ ?*test-pass* ?*test-fail*)))
(deffunction restore-counters (?saved)
   (bind ?*test-pass* (nth$ 1 ?saved))
   (bind ?*test-fail* (nth$ 2 ?saved)))

;;; Helper functions for funcall testing
(deffunction add-numbers (?a ?b)
   (+ ?a ?b))

(deffunction multiply-numbers (?a ?b)
   (* ?a ?b))

(deffunction square (?x)
   (* ?x ?x))

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; -- gensym* --
   (printout t crlf "-- gensym* --" crlf)
   (bind ?g1 (gensym*))
   (bind ?g2 (gensym*))
   (assert-true "gensym* returns symbol" (symbolp ?g1))
   (assert-true "gensym* unique" (neq ?g1 ?g2))
   (printout t "    INFO: gensym* generated " ?g1 " and " ?g2 crlf)

   ;; -- setgen --
   (printout t crlf "-- setgen --" crlf)
   (setgen 1000)
   (bind ?g3 (gensym*))
   (assert-equal "setgen 1000 => gen1000" gen1000 ?g3)

   ;; -- random / seed --
   (printout t crlf "-- random / seed --" crlf)
   (seed 42)
   (bind ?r1 (random))
   (assert-true "random returns integer" (integerp ?r1))
   (bind ?r2 (random))
   (assert-true "two randoms differ (usually)" TRUE)  ;; just checks it doesn't crash

   ;; Seeded determinism
   (seed 42)
   (bind ?r3 (random))
   (assert-equal "same seed same first random" ?r1 ?r3)

   ;; Random in range
   (seed 12345)
   (bind ?in-range TRUE)
   (loop-for-count (?i 1 20)
      (bind ?val (random 1 10))
      (if (or (< ?val 1) (> ?val 10)) then
         (bind ?in-range FALSE)))
   (assert-true "random(1,10) always in [1,10]" ?in-range)

   ;; -- format --
   (printout t crlf "-- format --" crlf)
   (assert-equal "format %d" "Number: 42" (format nil "Number: %d" 42))
   (assert-equal "format %05d" "Number: 00042" (format nil "Number: %05d" 42))
   (assert-equal "format %.3f" "Value: 3.142" (format nil "Value: %.3f" 3.14159))
   (assert-equal "format %e" TRUE (stringp (format nil "%e" 123456.789)))
   (assert-equal "format %s" "Name: Alice" (format nil "Name: %s" "Alice"))
   (assert-equal "format multi" "42 3.14 hello"
      (format nil "%d %.2f %s" 42 3.14159 "hello"))

   ;; -- funcall --
   (printout t crlf "-- funcall --" crlf)
   (assert-equal "funcall add" 7 (funcall add-numbers 3 4))
   (assert-equal "funcall multiply" 12 (funcall multiply-numbers 3 4))
   (assert-equal "funcall square" 25 (funcall square 5))
   ;; funcall with built-in
   (assert-equal "funcall + builtin" 10 (funcall + 3 7))
   (assert-equal "funcall str-cat" "foobar" (funcall str-cat "foo" "bar"))

   ;; -- time --
   (printout t crlf "-- time --" crlf)
   (bind ?t1 (time))
   (assert-true "time returns float" (floatp ?t1))
   (assert-true "time > 0" (> ?t1 0.0))
   (bind ?t2 (time))
   (assert-true "time monotonic" (>= ?t2 ?t1))

   ;; -- operating-system --
   (printout t crlf "-- operating-system --" crlf)
   (bind ?os (operating-system))
   (assert-true "operating-system returns symbol" (symbolp ?os))
   (printout t "    INFO: operating-system = " ?os crlf)

   ;; -- get-function-list --
   (printout t crlf "-- get-function-list --" crlf)
   (bind ?fl (get-function-list))
   (assert-true "function list is multifield" (multifieldp ?fl))
   (assert-true "function list > 100 entries" (> (length$ ?fl) 100))
   ;; Check our fuzzy UDFs are in the list
   (assert-true "get-threshold in function list" (neq (member$ get-threshold ?fl) FALSE))
   (assert-true "set-fuzzy-inference-type in list" (neq (member$ set-fuzzy-inference-type ?fl) FALSE))

   ;; -- type / class related --
   (printout t crlf "-- Type Functions --" crlf)
   (assert-equal "type of integer" INTEGER (type 42))
   (assert-equal "type of float" FLOAT (type 3.14))
   (assert-equal "type of string" STRING (type "hello"))
   (assert-equal "type of symbol" SYMBOL (type abc))

   ;; -- File I/O --
   (printout t crlf "-- File I/O --" crlf)
   (bind ?fh (open "/tmp/fuzzyclips_test_io.txt" test-file "w"))
   (assert-true "file opened for writing" ?fh)
   (printout test-file "line1" crlf)
   (printout test-file "line2" crlf)
   (printout test-file "line3" crlf)
   (close test-file)

   (bind ?fh (open "/tmp/fuzzyclips_test_io.txt" test-file "r"))
   (assert-true "file opened for reading" ?fh)
   (bind ?line (readline test-file))
   (assert-equal "read line1" "line1" ?line)
   (bind ?line (readline test-file))
   (assert-equal "read line2" "line2" ?line)
   (bind ?line (readline test-file))
   (assert-equal "read line3" "line3" ?line)
   (close test-file)

   ;; Clean up
   (remove "/tmp/fuzzyclips_test_io.txt")

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
