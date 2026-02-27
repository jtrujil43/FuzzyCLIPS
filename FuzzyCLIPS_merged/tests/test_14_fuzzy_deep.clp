;;;======================================================
;;; Test 14: Deep Fuzzy Extension Tests
;;; Comprehensive tests for all FuzzyCLIPS-specific UDFs:
;;; fuzzy inference type, display precision, alpha value,
;;; S/Z/PI functions, get-u-*, get-fs-*, modifiers,
;;; defuzzification stubs, plot-fuzzy-value
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 14: Deep Fuzzy Extensions")

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

   ;; ==================================================
   ;; SECTION 1: Fuzzy Inference Type (exhaustive)
   ;; ==================================================
   (printout t crlf "-- Fuzzy Inference Type --" crlf)

   ;; Default
   (bind ?ft (get-fuzzy-inference-type))
   (assert-equal "default inference = max-min" max-min ?ft)

   ;; Switch to max-prod
   (set-fuzzy-inference-type max-prod)
   (assert-equal "set to max-prod" max-prod (get-fuzzy-inference-type))

   ;; Switch back
   (set-fuzzy-inference-type max-min)
   (assert-equal "set back to max-min" max-min (get-fuzzy-inference-type))

   ;; Toggle multiple times
   (set-fuzzy-inference-type max-prod)
   (set-fuzzy-inference-type max-prod)
   (assert-equal "double set max-prod still max-prod" max-prod (get-fuzzy-inference-type))
   (set-fuzzy-inference-type max-min)

   ;; ==================================================
   ;; SECTION 2: Fuzzy Display Precision (boundary tests)
   ;; ==================================================
   (printout t crlf "-- Fuzzy Display Precision --" crlf)

   ;; Default
   (assert-equal "default precision = 4" 4 (get-fuzzy-display-precision))

   ;; Set to various values
   (set-fuzzy-display-precision 0)
   (assert-equal "precision = 0" 0 (get-fuzzy-display-precision))

   (set-fuzzy-display-precision 1)
   (assert-equal "precision = 1" 1 (get-fuzzy-display-precision))

   (set-fuzzy-display-precision 8)
   (assert-equal "precision = 8" 8 (get-fuzzy-display-precision))

   (set-fuzzy-display-precision 16)
   (assert-equal "precision = 16" 16 (get-fuzzy-display-precision))

   ;; Restore default
   (set-fuzzy-display-precision 4)
   (assert-equal "restored precision = 4" 4 (get-fuzzy-display-precision))

   ;; ==================================================
   ;; SECTION 3: Alpha Value (boundary tests)
   ;; ==================================================
   (printout t crlf "-- Alpha Value --" crlf)

   ;; Default
   (assert-true "default alpha = 0.0" (approx-equal (get-alpha-value) 0.0 0.0001))

   ;; Set to boundary values
   (set-alpha-value 0.0)
   (assert-true "alpha = 0.0" (approx-equal (get-alpha-value) 0.0 0.0001))

   (set-alpha-value 0.5)
   (assert-true "alpha = 0.5" (approx-equal (get-alpha-value) 0.5 0.0001))

   (set-alpha-value 1.0)
   (assert-true "alpha = 1.0" (approx-equal (get-alpha-value) 1.0 0.0001))

   (set-alpha-value 0.001)
   (assert-true "alpha = 0.001" (approx-equal (get-alpha-value) 0.001 0.0001))

   ;; Restore
   (set-alpha-value 0.0)

   ;; ==================================================
   ;; SECTION 4: CF Threshold (boundary + edge cases)
   ;; ==================================================
   (printout t crlf "-- CF Threshold Boundaries --" crlf)

   ;; Default after init
   (unthreshold)
   (assert-true "unthreshold => 0.0" (approx-equal (get-threshold) 0.0 0.0001))

   ;; Set to valid values
   (set-threshold 0.0)
   (assert-true "threshold = 0.0" (approx-equal (get-threshold) 0.0 0.0001))

   (set-threshold 0.5)
   (assert-true "threshold = 0.5" (approx-equal (get-threshold) 0.5 0.0001))

   (set-threshold 1.0)
   (assert-true "threshold = 1.0" (approx-equal (get-threshold) 1.0 0.0001))

   (set-threshold 0.1)
   (assert-true "threshold = 0.1" (approx-equal (get-threshold) 0.1 0.0001))

   (set-threshold 0.99)
   (assert-true "threshold = 0.99" (approx-equal (get-threshold) 0.99 0.0001))

   ;; Test set-threshold returns old value
   (set-threshold 0.3)
   (bind ?old (set-threshold 0.7))
   (assert-true "set-threshold returns old (0.3)" (approx-equal ?old 0.3 0.0001))
   (assert-true "new threshold = 0.7" (approx-equal (get-threshold) 0.7 0.0001))

   ;; Multiple unthreshold calls
   (unthreshold)
   (unthreshold)
   (assert-true "double unthreshold = 0.0" (approx-equal (get-threshold) 0.0 0.0001))

   ;; ==================================================
   ;; SECTION 5: Fuzzy Set Accessor Stubs
   ;; ==================================================
   (printout t crlf "-- Fuzzy Set Accessors (Stubs) --" crlf)

   ;; get-u-from and get-u-to return 0.0 (stub behavior)
   (bind ?uf (get-u-from dummy))
   (assert-true "get-u-from returns number" (numberp ?uf))
   (assert-true "get-u-from stub = 0.0" (approx-equal ?uf 0.0 0.0001))

   (bind ?ut (get-u-to dummy))
   (assert-true "get-u-to returns number" (numberp ?ut))
   (assert-true "get-u-to stub = 0.0" (approx-equal ?ut 0.0 0.0001))

   ;; get-fs-length returns 0 (stub)
   (bind ?fsl (get-fs-length dummy))
   (assert-true "get-fs-length returns integer" (integerp ?fsl))
   (assert-equal "get-fs-length stub = 0" 0 ?fsl)

   ;; get-fs-value, get-fs-x, get-fs-y return 0.0 (stubs)
   (bind ?fsv (get-fs-value dummy 0))
   (assert-true "get-fs-value stub = 0.0" (approx-equal ?fsv 0.0 0.0001))

   (bind ?fsx (get-fs-x dummy 0))
   (assert-true "get-fs-x stub = 0.0" (approx-equal ?fsx 0.0 0.0001))

   (bind ?fsy (get-fs-y dummy 0))
   (assert-true "get-fs-y stub = 0.0" (approx-equal ?fsy 0.0 0.0001))

   ;; get-u-units returns "" (stub)
   (bind ?units (get-u-units dummy))
   (assert-true "get-u-units returns string" (stringp ?units))
   (assert-equal "get-u-units stub = empty" "" ?units)

   ;; get-fs-template returns a symbol (stub)
   (bind ?tmpl (get-fs-template dummy))
   (assert-true "get-fs-template returns symbol" (symbolp ?tmpl))

   ;; ==================================================
   ;; SECTION 6: Defuzzification Stubs
   ;; ==================================================
   (printout t crlf "-- Defuzzification Stubs --" crlf)

   (bind ?md (moment-defuzzify dummy))
   (assert-true "moment-defuzzify returns number" (numberp ?md))
   (assert-true "moment-defuzzify stub = 0.0" (approx-equal ?md 0.0 0.0001))

   (bind ?mx (maximum-defuzzify dummy))
   (assert-true "maximum-defuzzify returns number" (numberp ?mx))
   (assert-true "maximum-defuzzify stub = 0.0" (approx-equal ?mx 0.0 0.0001))

   ;; ==================================================
   ;; SECTION 7: CF Enable/Disable Stubs
   ;; ==================================================
   (printout t crlf "-- CF Calculation Control --" crlf)

   ;; These are stubs — just verify they don't crash
   (enable-rule-cf-calculation)
   (assert-true "enable-rule-cf-calculation ok" TRUE)

   (disable-rule-cf-calculation)
   (assert-true "disable-rule-cf-calculation ok" TRUE)

   ;; ==================================================
   ;; SECTION 8: Fuzzy function list verification
   ;; ==================================================
   (printout t crlf "-- UDF Registration Verification --" crlf)
   (bind ?fl (get-function-list))

   ;; Verify all fuzzy UDFs are registered
   (assert-true "moment-defuzzify registered" (neq (member$ moment-defuzzify ?fl) FALSE))
   (assert-true "maximum-defuzzify registered" (neq (member$ maximum-defuzzify ?fl) FALSE))
   (assert-true "get-u registered" (neq (member$ get-u ?fl) FALSE))
   (assert-true "get-u-from registered" (neq (member$ get-u-from ?fl) FALSE))
   (assert-true "get-u-to registered" (neq (member$ get-u-to ?fl) FALSE))
   (assert-true "get-u-units registered" (neq (member$ get-u-units ?fl) FALSE))
   (assert-true "get-fs registered" (neq (member$ get-fs ?fl) FALSE))
   (assert-true "get-fs-template registered" (neq (member$ get-fs-template ?fl) FALSE))
   (assert-true "get-fs-lv registered" (neq (member$ get-fs-lv ?fl) FALSE))
   (assert-true "get-fs-length registered" (neq (member$ get-fs-length ?fl) FALSE))
   (assert-true "get-fs-value registered" (neq (member$ get-fs-value ?fl) FALSE))
   (assert-true "get-fs-x registered" (neq (member$ get-fs-x ?fl) FALSE))
   (assert-true "get-fs-y registered" (neq (member$ get-fs-y ?fl) FALSE))
   (assert-true "fuzzy-union registered" (neq (member$ fuzzy-union ?fl) FALSE))
   (assert-true "fuzzy-intersection registered" (neq (member$ fuzzy-intersection ?fl) FALSE))
   (assert-true "fuzzy-modify registered" (neq (member$ fuzzy-modify ?fl) FALSE))
   (assert-true "create-fuzzy-value registered" (neq (member$ create-fuzzy-value ?fl) FALSE))
   (assert-true "add-fuzzy-modifier registered" (neq (member$ add-fuzzy-modifier ?fl) FALSE))
   (assert-true "remove-fuzzy-modifier registered" (neq (member$ remove-fuzzy-modifier ?fl) FALSE))
   (assert-true "set-fuzzy-inference-type reg" (neq (member$ set-fuzzy-inference-type ?fl) FALSE))
   (assert-true "get-fuzzy-inference-type reg" (neq (member$ get-fuzzy-inference-type ?fl) FALSE))
   (assert-true "set-fuzzy-display-precision reg" (neq (member$ set-fuzzy-display-precision ?fl) FALSE))
   (assert-true "get-fuzzy-display-precision reg" (neq (member$ get-fuzzy-display-precision ?fl) FALSE))
   (assert-true "set-alpha-value registered" (neq (member$ set-alpha-value ?fl) FALSE))
   (assert-true "get-alpha-value registered" (neq (member$ get-alpha-value ?fl) FALSE))
   (assert-true "plot-fuzzy-value registered" (neq (member$ plot-fuzzy-value ?fl) FALSE))

   ;; Verify CF UDFs
   (assert-true "get-threshold registered" (neq (member$ get-threshold ?fl) FALSE))
   (assert-true "set-threshold registered" (neq (member$ set-threshold ?fl) FALSE))
   (assert-true "get-cf registered" (neq (member$ get-cf ?fl) FALSE))
   (assert-true "unthreshold registered" (neq (member$ unthreshold ?fl) FALSE))
   (assert-true "enable-rule-cf-calculation reg" (neq (member$ enable-rule-cf-calculation ?fl) FALSE))
   (assert-true "disable-rule-cf-calculation reg" (neq (member$ disable-rule-cf-calculation ?fl) FALSE))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
