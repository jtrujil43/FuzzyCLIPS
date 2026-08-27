;;;======================================================
;;; Test 04: Certainty Factors
;;; Tests CF assignment, retrieval, and combination
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 04: Certainty Factors")

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

;;; Templates for CF testing
(deftemplate measurement
   (slot source (type SYMBOL))
   (slot reading (type FLOAT)))

(deftemplate diagnosis
   (slot condition (type SYMBOL))
   (slot severity (type SYMBOL) (default low)))

;;; Rules that use pattern matching
(defrule detect-anomaly
   (measurement (source sensor-a) (reading ?r&:(> ?r 50.0)))
   =>
   (assert (diagnosis (condition anomaly) (severity high))))

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; Test: get-cf on a fact
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   (bind ?f1 (assert (measurement (source sensor-a) (reading 75.0))))

   ;; get-cf should return the certainty factor (default 1.0)
   (bind ?cf (get-cf ?f1))
   (assert-true "get-cf returns a number"
      (numberp ?cf))
   (assert-true "default CF is 1.0"
      (approx-equal ?cf 1.0 0.001))
   (printout t "    INFO: CF of asserted fact = " ?cf crlf)

   ;; Test: threshold operations
   (set-threshold 0.2)
   (bind ?th (get-threshold))
   (assert-true "threshold set to 0.2"
      (approx-equal ?th 0.2 0.001))

   ;; Test: unthreshold
   (unthreshold)
   (bind ?th (get-threshold))
   (assert-true "unthreshold resets to 0.0"
      (approx-equal ?th 0.0 0.001))

   ;; Restore threshold
   (set-threshold 0.0)

   ;; Test: rules still fire with facts
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   (assert (measurement (source sensor-a) (reading 75.0)))
   (run)
   (assert-true "anomaly diagnosis created"
      (any-factp ((?d diagnosis)) (eq ?d:condition anomaly)))

   ;; Test: fact without matching pattern (no rule fire)
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   (assert (measurement (source sensor-a) (reading 25.0)))
   (run)
   (assert-true "no anomaly for reading <= 50"
      (not (any-factp ((?d diagnosis)) (eq ?d:condition anomaly))))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
