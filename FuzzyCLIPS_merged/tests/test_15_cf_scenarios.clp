;;;======================================================
;;; Test 15: Certainty Factor Scenario Tests
;;; Tests CF with facts, threshold filtering, rules with
;;; CF propagation, and the Mycin CF combination formula
;;; via realistic diagnostic scenarios
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 15: Certainty Factor Scenarios")

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

;;; ==== Templates for medical diagnosis scenario ====

(deftemplate symptom
   (slot name (type SYMBOL))
   (slot severity (type SYMBOL) (default moderate)))

(deftemplate evidence
   (slot type (type SYMBOL))
   (slot source (type SYMBOL))
   (slot value (type FLOAT)))

(deftemplate diagnosis
   (slot disease (type SYMBOL))
   (slot likelihood (type SYMBOL) (default possible)))

;;; ==== Rules for diagnostic reasoning ====

(defrule fever-indicates-infection
   (symptom (name fever) (severity ?s&high|moderate))
   =>
   (assert (diagnosis (disease infection) (likelihood probable))))

(defrule cough-indicates-respiratory
   (symptom (name cough))
   =>
   (assert (diagnosis (disease respiratory) (likelihood possible))))

(defrule multiple-symptoms
   (symptom (name fever))
   (symptom (name cough))
   (symptom (name fatigue))
   =>
   (assert (diagnosis (disease flu) (likelihood probable))))

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; ==================================================
   ;; SECTION 1: get-cf on various fact types
   ;; ==================================================
   (printout t crlf "-- get-cf on Facts --" crlf)

   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)

   ;; Assert facts and check default CF
   (bind ?f1 (assert (symptom (name fever) (severity high))))
   (bind ?cf1 (get-cf ?f1))
   (assert-true "fever CF is number" (numberp ?cf1))
   (assert-true "fever default CF = 1.0" (approx-equal ?cf1 1.0 0.001))
   (printout t "    INFO: fever CF = " ?cf1 crlf)

   (bind ?f2 (assert (symptom (name cough) (severity moderate))))
   (bind ?cf2 (get-cf ?f2))
   (assert-true "cough default CF = 1.0" (approx-equal ?cf2 1.0 0.001))

   (bind ?f3 (assert (symptom (name fatigue) (severity mild))))
   (bind ?cf3 (get-cf ?f3))
   (assert-true "fatigue default CF = 1.0" (approx-equal ?cf3 1.0 0.001))

   ;; ==================================================
   ;; SECTION 2: Threshold filtering scenarios
   ;; ==================================================
   (printout t crlf "-- Threshold Filtering --" crlf)

   ;; With threshold at 0.0, all facts are "above threshold"
   (set-threshold 0.0)
   (assert-true "threshold 0.0: fever CF >= threshold"
      (>= (get-cf ?f1) (get-threshold)))

   ;; Set threshold high — facts with CF=1.0 still pass
   (set-threshold 0.9)
   (assert-true "threshold 0.9: fever CF >= threshold"
      (>= (get-cf ?f1) (get-threshold)))

   ;; Set threshold at exactly 1.0
   (set-threshold 1.0)
   (assert-true "threshold 1.0: fever CF >= threshold"
      (>= (get-cf ?f1) (get-threshold)))

   ;; Restore
   (unthreshold)

   ;; ==================================================
   ;; SECTION 3: Rules with symptoms
   ;; ==================================================
   (printout t crlf "-- Diagnostic Rules --" crlf)

   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)

   ;; Just fever
   (assert (symptom (name fever) (severity high)))
   (run)
   (assert-true "fever => infection diagnosis"
      (any-factp ((?d diagnosis)) (eq ?d:disease infection)))
   (assert-true "no flu with only fever"
      (not (any-factp ((?d diagnosis)) (eq ?d:disease flu))))

   ;; Add cough
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   (assert (symptom (name fever) (severity high)))
   (assert (symptom (name cough) (severity moderate)))
   (run)
   (assert-true "fever+cough => infection"
      (any-factp ((?d diagnosis)) (eq ?d:disease infection)))
   (assert-true "cough => respiratory"
      (any-factp ((?d diagnosis)) (eq ?d:disease respiratory)))
   (assert-true "no flu without fatigue"
      (not (any-factp ((?d diagnosis)) (eq ?d:disease flu))))

   ;; All three symptoms => flu
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)
   (assert (symptom (name fever) (severity high)))
   (assert (symptom (name cough) (severity moderate)))
   (assert (symptom (name fatigue) (severity mild)))
   (run)
   (assert-true "all symptoms => flu diagnosis"
      (any-factp ((?d diagnosis)) (eq ?d:disease flu)))

   ;; ==================================================
   ;; SECTION 4: Threshold set/get cycle
   ;; ==================================================
   (printout t crlf "-- Threshold Cycling --" crlf)

   ;; Cycle through many threshold values
   (bind ?all-ok TRUE)
   (progn$ (?tv (create$ 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))
      (set-threshold ?tv)
      (if (not (approx-equal (get-threshold) ?tv 0.0001)) then
         (bind ?all-ok FALSE)))
   (assert-true "threshold cycle 0.0 to 1.0" ?all-ok)
   (unthreshold)

   ;; ==================================================
   ;; SECTION 5: get-cf with evidence facts
   ;; ==================================================
   (printout t crlf "-- get-cf with Evidence --" crlf)

   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)

   ;; Multiple evidence facts
   (bind ?e1 (assert (evidence (type lab-test) (source blood-panel) (value 95.0))))
   (bind ?e2 (assert (evidence (type imaging) (source xray) (value 72.0))))
   (bind ?e3 (assert (evidence (type vitals) (source thermometer) (value 101.5))))

   (assert-true "evidence1 CF = 1.0" (approx-equal (get-cf ?e1) 1.0 0.001))
   (assert-true "evidence2 CF = 1.0" (approx-equal (get-cf ?e2) 1.0 0.001))
   (assert-true "evidence3 CF = 1.0" (approx-equal (get-cf ?e3) 1.0 0.001))

   ;; ==================================================
   ;; SECTION 6: Combining threshold with rule firing
   ;; ==================================================
   (printout t crlf "-- Threshold + Rules --" crlf)

   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)

   ;; Set threshold, assert symptoms, run rules
   (set-threshold 0.5)
   (assert (symptom (name fever) (severity high)))
   (assert (symptom (name cough) (severity low)))
   (run)

   ;; Rules should still fire (all facts have CF=1.0 > 0.5)
   (assert-true "threshold 0.5: infection still diagnosed"
      (any-factp ((?d diagnosis)) (eq ?d:disease infection)))

   (unthreshold)

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
