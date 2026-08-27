;;;======================================================
;;; Test 03: Fuzzy Extension Commands
;;; Tests the FuzzyCLIPS-specific UDF commands
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 03: Fuzzy Extension Commands")

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

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; ---- Fuzzy Inference Type ----
   (printout t crlf "-- Fuzzy Inference Type --" crlf)

   (bind ?itype (get-fuzzy-inference-type))
   (assert-equal "default inference type is max-min"
      max-min ?itype)

   (set-fuzzy-inference-type max-prod)
   (bind ?itype (get-fuzzy-inference-type))
   (assert-equal "set inference type to max-prod"
      max-prod ?itype)

   (set-fuzzy-inference-type max-min)
   (bind ?itype (get-fuzzy-inference-type))
   (assert-equal "set inference type back to max-min"
      max-min ?itype)

   ;; ---- Fuzzy Display Precision ----
   (printout t crlf "-- Fuzzy Display Precision --" crlf)

   (bind ?prec (get-fuzzy-display-precision))
   (assert-equal "default precision is 4" 4 ?prec)

   (set-fuzzy-display-precision 2)
   (bind ?prec (get-fuzzy-display-precision))
   (assert-equal "precision set to 2" 2 ?prec)

   (set-fuzzy-display-precision 6)
   (bind ?prec (get-fuzzy-display-precision))
   (assert-equal "precision set to 6" 6 ?prec)

   ;; Restore default
   (set-fuzzy-display-precision 4)

   ;; ---- Alpha Value ----
   (printout t crlf "-- Alpha Value --" crlf)

   (bind ?alpha (get-alpha-value))
   (assert-true "default alpha is ~0.0"
      (approx-equal ?alpha 0.0 0.001))

   (set-alpha-value 0.1)
   (bind ?alpha (get-alpha-value))
   (assert-true "alpha set to 0.1"
      (approx-equal ?alpha 0.1 0.001))

   (set-alpha-value 0.0)

   ;; ---- CF Threshold Commands ----
   (printout t crlf "-- CF Threshold --" crlf)

   (bind ?th (get-threshold))
   (assert-true "default threshold is 0.0"
      (approx-equal ?th 0.0 0.001))

   (set-threshold 0.5)
   (bind ?th (get-threshold))
   (assert-true "threshold set to 0.5"
      (approx-equal ?th 0.5 0.001))

   ;; unthreshold resets to 0.0
   (unthreshold)
   (bind ?th (get-threshold))
   (assert-true "unthreshold resets to 0.0"
      (approx-equal ?th 0.0 0.001))

   ;; ---- Fuzzy Set Manipulation UDFs exist ----
   (printout t crlf "-- UDF Registration --" crlf)
   ;; Just verify these don't error (they are stubs returning void/defaults)
   (assert-true "get-u-from registered" (numberp (get-u-from dummy)))
   (assert-true "get-u-to registered" (numberp (get-u-to dummy)))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
