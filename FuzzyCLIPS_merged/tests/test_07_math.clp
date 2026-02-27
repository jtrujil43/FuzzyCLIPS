;;;======================================================
;;; Test 07: Extended Math Functions
;;; Tests trig, hyperbolic, exponential, power, rounding,
;;; angle conversions, and math constants
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 07: Extended Math Functions")

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

   ;; -- pi constant --
   (printout t crlf "-- Constants --" crlf)
   (assert-true "pi ~ 3.14159" (approx-equal (pi) 3.14159265 0.0001))

   ;; -- Trigonometric functions --
   (printout t crlf "-- Trigonometric --" crlf)
   (assert-true "sin(0) = 0" (approx-equal (sin 0.0) 0.0 0.0001))
   (assert-true "sin(pi/2) = 1" (approx-equal (sin (/ (pi) 2.0)) 1.0 0.0001))
   (assert-true "cos(0) = 1" (approx-equal (cos 0.0) 1.0 0.0001))
   (assert-true "cos(pi) = -1" (approx-equal (cos (pi)) -1.0 0.0001))
   (assert-true "tan(0) = 0" (approx-equal (tan 0.0) 0.0 0.0001))
   (assert-true "tan(pi/4) = 1" (approx-equal (tan (/ (pi) 4.0)) 1.0 0.0001))

   ;; -- Inverse trigonometric --
   (printout t crlf "-- Inverse Trigonometric --" crlf)
   (assert-true "asin(0) = 0" (approx-equal (asin 0.0) 0.0 0.0001))
   (assert-true "asin(1) = pi/2" (approx-equal (asin 1.0) (/ (pi) 2.0) 0.0001))
   (assert-true "acos(1) = 0" (approx-equal (acos 1.0) 0.0 0.0001))
   (assert-true "acos(0) = pi/2" (approx-equal (acos 0.0) (/ (pi) 2.0) 0.0001))
   (assert-true "atan(0) = 0" (approx-equal (atan 0.0) 0.0 0.0001))
   (assert-true "atan(1) = pi/4" (approx-equal (atan 1.0) (/ (pi) 4.0) 0.0001))

   ;; -- Hyperbolic functions --
   (printout t crlf "-- Hyperbolic --" crlf)
   (assert-true "sinh(0) = 0" (approx-equal (sinh 0.0) 0.0 0.0001))
   (assert-true "cosh(0) = 1" (approx-equal (cosh 0.0) 1.0 0.0001))
   (assert-true "tanh(0) = 0" (approx-equal (tanh 0.0) 0.0 0.0001))
   ;; sinh(1) ~ 1.17520
   (assert-true "sinh(1) ~ 1.1752" (approx-equal (sinh 1.0) 1.17520 0.001))
   ;; cosh(1) ~ 1.54308
   (assert-true "cosh(1) ~ 1.5431" (approx-equal (cosh 1.0) 1.54308 0.001))

   ;; -- Exponential / Logarithmic --
   (printout t crlf "-- Exp / Log --" crlf)
   (assert-true "exp(0) = 1" (approx-equal (exp 0.0) 1.0 0.0001))
   (assert-true "exp(1) = e ~ 2.71828" (approx-equal (exp 1.0) 2.71828 0.001))
   (assert-true "log(1) = 0" (approx-equal (log 1.0) 0.0 0.0001))
   (assert-true "log(e) = 1" (approx-equal (log (exp 1.0)) 1.0 0.0001))
   (assert-true "log10(10) = 1" (approx-equal (log10 10.0) 1.0 0.0001))
   (assert-true "log10(100) = 2" (approx-equal (log10 100.0) 2.0 0.0001))

   ;; -- Power / Sqrt --
   (printout t crlf "-- Power / Sqrt --" crlf)
   (assert-true "** 2 3 = 8" (approx-equal (** 2.0 3.0) 8.0 0.0001))
   (assert-true "** 3 2 = 9" (approx-equal (** 3.0 2.0) 9.0 0.0001))
   (assert-true "** 2 10 = 1024" (approx-equal (** 2.0 10.0) 1024.0 0.0001))
   (assert-true "sqrt(4) = 2" (approx-equal (sqrt 4.0) 2.0 0.0001))
   (assert-true "sqrt(2) ~ 1.4142" (approx-equal (sqrt 2.0) 1.41421 0.001))
   (assert-true "sqrt(0) = 0" (approx-equal (sqrt 0.0) 0.0 0.0001))

   ;; -- Rounding --
   (printout t crlf "-- Rounding --" crlf)
   (assert-equal "round 2.3 = 2" 2 (round 2.3))
   (assert-equal "round 2.7 = 3" 3 (round 2.7))
   (assert-equal "round -2.3 = -2" -2 (round -2.3))
   (assert-equal "round -2.7 = -3" -3 (round -2.7))
   (assert-equal "integer 5.9 = 5" 5 (integer 5.9))
   (assert-equal "integer -5.9 = -5" -5 (integer -5.9))
   (assert-equal "float 5 = 5.0" 5.0 (float 5))

   ;; -- Angle conversions --
   (printout t crlf "-- Angle Conversions --" crlf)
   (assert-true "deg-rad 180 = pi" (approx-equal (deg-rad 180.0) (pi) 0.0001))
   (assert-true "rad-deg pi = 180" (approx-equal (rad-deg (pi)) 180.0 0.0001))
   (assert-true "deg-rad 90 = pi/2" (approx-equal (deg-rad 90.0) (/ (pi) 2.0) 0.0001))
   (assert-true "deg-grad 90 = 100" (approx-equal (deg-grad 90.0) 100.0 0.0001))
   (assert-true "grad-deg 100 = 90" (approx-equal (grad-deg 100.0) 90.0 0.0001))

   ;; -- Additional arithmetic --
   (printout t crlf "-- Arithmetic --" crlf)
   (assert-equal "div 10 3 = 3" 3 (div 10 3))
   (assert-equal "div -10 3 = -3" -3 (div -10 3))
   (assert-equal "mod 10 3 = 1" 1 (mod 10 3))
   (assert-true "min multi-arg" (= 1 (min 5 3 1 4 2)))
   (assert-true "max multi-arg" (= 5 (max 5 3 1 4 2)))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
