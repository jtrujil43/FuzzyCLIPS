;;;======================================================
;;; Test 05: CLIPS Constructs
;;; Tests deftemplate, deffacts, defglobal, deffunction,
;;; defmodule, and other core constructs
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 05: CLIPS Constructs")

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

(deffunction save-counters ()
   (return (create$ ?*test-pass* ?*test-fail*)))
(deffunction restore-counters (?saved)
   (bind ?*test-pass* (nth$ 1 ?saved))
   (bind ?*test-fail* (nth$ 2 ?saved)))

;;; --- Deftemplate with constraints ---
(deftemplate person
   (slot name (type SYMBOL))
   (slot age (type INTEGER) (range 0 150))
   (slot height (type FLOAT) (default 0.0))
   (multislot hobbies (type SYMBOL)))

;;; --- Defglobal ---
(defglobal ?*PI* = 3.14159)
(defglobal ?*greeting* = "Hello")

;;; --- Deffunction ---
(deffunction circle-area (?r)
   (* ?*PI* ?r ?r))

(deffunction factorial (?n)
   (if (<= ?n 1) then 1
    else (* ?n (factorial (- ?n 1)))))

(deffunction fibonacci (?n)
   (if (<= ?n 1) then ?n
    else (+ (fibonacci (- ?n 1))
            (fibonacci (- ?n 2)))))

;;; --- Deffacts ---
(deffacts people
   (person (name alice) (age 30) (height 5.6) (hobbies reading coding))
   (person (name bob) (age 25) (height 5.9) (hobbies gaming)))

;;; --- Defrule with pattern matching ---
(defrule find-coder
   (person (name ?n) (hobbies $?h1 coding $?h2))
   =>
   (assert (coder ?n)))

(defrule find-young
   (person (name ?n) (age ?a&:(< ?a 30)))
   =>
   (assert (young-person ?n)))

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; Test: defglobals
   (printout t crlf "-- Defglobals --" crlf)
   (assert-true "PI approx 3.14" (and (> ?*PI* 3.14) (< ?*PI* 3.15)))
   (assert-equal "greeting is Hello" "Hello" ?*greeting*)

   ;; Test: deffunctions
   (printout t crlf "-- Deffunctions --" crlf)
   (assert-true "circle area r=1 ~ PI"
      (and (> (circle-area 1.0) 3.14) (< (circle-area 1.0) 3.15)))
   (assert-equal "factorial 5 = 120" 120 (factorial 5))
   (assert-equal "factorial 0 = 1" 1 (factorial 0))
   (assert-equal "fibonacci 0 = 0" 0 (fibonacci 0))
   (assert-equal "fibonacci 1 = 1" 1 (fibonacci 1))
   (assert-equal "fibonacci 7 = 13" 13 (fibonacci 7))

   ;; Test: deffacts and templates
   (printout t crlf "-- Deffacts & Templates --" crlf)
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   (assert-true "alice exists"
      (any-factp ((?p person)) (eq ?p:name alice)))
   (assert-true "bob exists"
      (any-factp ((?p person)) (eq ?p:name bob)))
   (assert-true "alice age 30"
      (any-factp ((?p person)) (and (eq ?p:name alice) (= ?p:age 30))))
   (assert-true "bob height 5.9"
      (any-factp ((?p person)) (and (eq ?p:name bob) (= ?p:height 5.9))))

   ;; Test: rules fire correctly
   (printout t crlf "-- Rules --" crlf)
   (run)
   (assert-true "alice identified as coder"
      (any-factp ((?c coder)) (eq (nth$ 1 ?c:implied) alice)))
   (assert-true "bob identified as young"
      (any-factp ((?y young-person)) (eq (nth$ 1 ?y:implied) bob)))

   ;; Test: dynamic assert/retract
   (printout t crlf "-- Dynamic Assert/Retract --" crlf)
   (bind ?f (assert (person (name charlie) (age 45) (height 6.0) (hobbies hiking))))
   (assert-true "charlie added"
      (any-factp ((?p person)) (eq ?p:name charlie)))
   (retract ?f)
   (assert-true "charlie retracted"
      (not (any-factp ((?p person)) (eq ?p:name charlie))))

   ;; Test: modify
   (printout t crlf "-- Modify --" crlf)
   (do-for-fact ((?p person)) (eq ?p:name alice)
      (modify ?p (age 31)))
   (assert-true "alice age modified to 31"
      (any-factp ((?p person)) (and (eq ?p:name alice) (= ?p:age 31))))

   ;; Test: find-all-facts
   (printout t crlf "-- Query Functions --" crlf)
   (bind ?all-people (find-all-facts ((?p person)) TRUE))
   (assert-true "find-all-facts returns >= 2"
      (>= (length$ ?all-people) 2))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
