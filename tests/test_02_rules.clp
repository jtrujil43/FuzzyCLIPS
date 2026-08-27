;;;======================================================
;;; Test 02: Rule Engine & Pattern Matching
;;; Verifies rules fire correctly, salience works, etc.
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 02: Rule Engine & Pattern Matching")

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
   (bind ?r (create$ ?*test-pass* ?*test-fail*))
   (return ?r))

(deffunction restore-counters (?saved)
   (bind ?*test-pass* (nth$ 1 ?saved))
   (bind ?*test-fail* (nth$ 2 ?saved)))

;;; --- Deftemplate & Rules ---

(deftemplate sensor
   (slot name (type SYMBOL))
   (slot value (type FLOAT))
   (slot status (type SYMBOL) (default unknown)))

(deftemplate alert
   (slot level (type SYMBOL))
   (slot message (type STRING)))

(defrule high-temp
   (declare (salience 10))
   (sensor (name temperature) (value ?v&:(> ?v 100.0)))
   =>
   (assert (alert (level critical) (message "Temperature too high"))))

(defrule normal-temp
   (declare (salience 5))
   (sensor (name temperature) (value ?v&:(<= ?v 100.0)&:(>= ?v 0.0)))
   =>
   (assert (alert (level info) (message "Temperature normal"))))

(defrule low-battery
   (sensor (name battery) (value ?v&:(< ?v 20.0)))
   =>
   (assert (alert (level warning) (message "Low battery"))))

;;; Test chain: one rule triggers another
(defrule escalate-critical
   (alert (level critical))
   =>
   (assert (escalated yes)))

;;; deffacts with BOTH sensors for first test
(deffacts test-sensors
   (sensor (name temperature) (value 150.0))
   (sensor (name battery) (value 15.0)))

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; Test: rules fire on matching facts (uses deffacts)
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   (run)

   (assert-true "critical alert created"
      (any-factp ((?a alert)) (eq ?a:level critical)))

   (assert-true "warning alert created"
      (any-factp ((?a alert)) (eq ?a:level warning)))

   (assert-true "escalation rule fired"
      (any-factp ((?e escalated)) TRUE))

   ;; Test: normal temp (override deffacts by adding different sensor)
   ;; After reset, deffacts re-asserts temp=150 and battery=15.
   ;; We need a clean slate, so reset then retract temp=150, assert temp=50
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   ;; Retract the deffacts temperature sensor
   (do-for-fact ((?f sensor)) (eq ?f:name temperature)
      (retract ?f))
   ;; Assert a normal temperature
   (assert (sensor (name temperature) (value 50.0)))
   (run)

   (assert-true "normal temp - info alert"
      (any-factp ((?a alert)) (eq ?a:level info)))

   ;; Note: battery=15.0 from deffacts still triggers low-battery warning
   ;; So we only check that no CRITICAL alert for normal temp
   (assert-true "no critical alert for normal temp"
      (not (any-factp ((?a alert)) (eq ?a:level critical))))

   ;; Test: high temp only, no low battery
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   ;; Retract battery sensor so no warning fires
   (do-for-fact ((?f sensor)) (eq ?f:name battery)
      (retract ?f))
   ;; Replace battery with high-value one
   (assert (sensor (name battery) (value 90.0)))
   (run)

   (assert-true "critical alert for 150, no battery warning for 90%"
      (and (any-factp ((?a alert)) (eq ?a:level critical))
           (not (any-factp ((?a alert)) (eq ?a:level warning)))))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
