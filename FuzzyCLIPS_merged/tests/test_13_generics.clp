;;;======================================================
;;; Test 13: Generic Functions & Defmethod
;;; Tests defgeneric, defmethod, type-based dispatch,
;;; method precedence, numeric vs string dispatch
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 13: Generic Functions")

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

;;; ==== Generic function: describe-value ====
;;; Dispatches differently based on argument type

(defgeneric describe-value)

(defmethod describe-value ((?x INTEGER))
   (str-cat "integer:" (format nil "%d" ?x)))

(defmethod describe-value ((?x FLOAT))
   (str-cat "float:" (format nil "%.2f" ?x)))

(defmethod describe-value ((?x STRING))
   (str-cat "string:" ?x))

(defmethod describe-value ((?x SYMBOL))
   (str-cat "symbol:" (sym-cat ?x)))

;;; ==== Generic function: compute with multiple types ====

(defgeneric compute-area)

;; Two-argument rectangle
(defmethod compute-area ((?w NUMBER) (?h NUMBER))
   (* ?w ?h))

;; Single-argument circle (radius)
(defmethod compute-area ((?r NUMBER))
   (* 3.14159 ?r ?r))

;;; ==== Generic function: format-output ====
;;; Tests method with different arity

(defgeneric format-pair)

(defmethod format-pair ((?key SYMBOL) (?val INTEGER))
   (format nil "%s=%d" (sym-cat ?key) ?val))

(defmethod format-pair ((?key SYMBOL) (?val FLOAT))
   (format nil "%s=%.2f" (sym-cat ?key) ?val))

(defmethod format-pair ((?key SYMBOL) (?val STRING))
   (format nil "%s=\"%s\"" (sym-cat ?key) ?val))

(defmethod format-pair ((?key SYMBOL) (?val SYMBOL))
   (format nil "%s=%s" (sym-cat ?key) (sym-cat ?val)))

;;; ==== Generic function: math operations with special cases ====

(defgeneric safe-divide)

(defmethod safe-divide ((?a NUMBER) (?b NUMBER (= ?b 0)))
   (str-cat "error:division-by-zero"))

(defmethod safe-divide ((?a NUMBER) (?b NUMBER (~= ?b 0)))
   (/ ?a ?b))

;;; ==== Generic function: recursive structure ====

(defgeneric to-string)

(defmethod to-string ((?x INTEGER))
   (format nil "%d" ?x))

(defmethod to-string ((?x FLOAT))
   (format nil "%.4f" ?x))

(defmethod to-string ((?x STRING))
   ?x)

(defmethod to-string ((?x SYMBOL))
   (sym-cat ?x))

;;; ==== Tests ====

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; -- Type-based dispatch --
   (printout t crlf "-- Type Dispatch --" crlf)
   (assert-equal "dispatch integer" "integer:42" (describe-value 42))
   (assert-equal "dispatch float" "float:3.14" (describe-value 3.14))
   (assert-equal "dispatch string" "string:hello" (describe-value "hello"))
   (assert-equal "dispatch symbol" "symbol:world" (describe-value world))

   ;; -- Arity-based dispatch --
   (printout t crlf "-- Arity Dispatch --" crlf)
   (bind ?rect (compute-area 5.0 3.0))
   (assert-true "compute-area(5,3) = 15" (approx-equal ?rect 15.0 0.01))
   (bind ?circ (compute-area 2.0))
   (assert-true "compute-area(2) ~ 12.566" (approx-equal ?circ 12.566 0.01))

   ;; -- Type-based second arg dispatch --
   (printout t crlf "-- Parameterized Dispatch --" crlf)
   (assert-equal "format int pair" "age=25" (format-pair age 25))
   (assert-equal "format float pair" "temp=98.60" (format-pair temp 98.6))
   (assert-equal "format string pair" "name=\"Alice\"" (format-pair name "Alice"))
   (assert-equal "format symbol pair" "status=active" (format-pair status active))

   ;; -- Restriction-based dispatch --
   (printout t crlf "-- Restriction Dispatch --" crlf)
   (bind ?r1 (safe-divide 10.0 3.0))
   (assert-true "safe-divide 10/3 ~ 3.333" (approx-equal ?r1 3.333 0.01))
   (bind ?r2 (safe-divide 10.0 0))
   (assert-equal "safe-divide by zero" "error:division-by-zero" ?r2)
   (bind ?r3 (safe-divide 10.0 0.0))
   (assert-equal "safe-divide by 0.0" "error:division-by-zero" ?r3)

   ;; -- Generic to-string --
   (printout t crlf "-- Generic to-string --" crlf)
   (assert-equal "to-string int" "42" (to-string 42))
   (assert-equal "to-string float" "3.1416" (to-string 3.14159))
   (assert-equal "to-string string" "hello" (to-string "hello"))

   ;; -- Method listing --
   (printout t crlf "-- Method Introspection --" crlf)
   (bind ?methods (get-method-list describe-value))
   (assert-true "describe-value has methods" (> (length$ ?methods) 0))
   (printout t "    INFO: describe-value methods: " (length$ ?methods) " entries" crlf)

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
