;;;======================================================
;;; Test 11: Fact & Instance Query Functions
;;; Tests any-factp, find-fact, find-all-facts,
;;; do-for-fact, do-for-all-facts, delayed-do-for-all,
;;; and corresponding instance query functions
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 11: Query Functions")

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

;;; --- Templates ---
(deftemplate student
   (slot name (type SYMBOL))
   (slot grade (type INTEGER) (range 0 100))
   (slot subject (type SYMBOL)))

(deftemplate course
   (slot title (type SYMBOL))
   (slot department (type SYMBOL))
   (slot credits (type INTEGER)))

;;; --- Class for instance queries ---
(defclass Product (is-a USER)
   (role concrete)
   (slot product-name (type STRING) (default ""))
   (slot price (type FLOAT) (default 0.0))
   (slot category (type SYMBOL) (default general)))

;;; --- Deffacts ---
(deffacts student-data
   (student (name alice) (grade 92) (subject math))
   (student (name bob) (grade 78) (subject science))
   (student (name carol) (grade 95) (subject math))
   (student (name dave) (grade 65) (subject science))
   (student (name eve) (grade 88) (subject art)))

(deffacts course-data
   (course (title algebra) (department math) (credits 3))
   (course (title calculus) (department math) (credits 4))
   (course (title physics) (department science) (credits 4))
   (course (title painting) (department art) (credits 2)))

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; Load facts
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)

   ;; -- any-factp --
   (printout t crlf "-- any-factp --" crlf)
   (assert-true "any student with grade > 90"
      (any-factp ((?s student)) (> ?s:grade 90)))
   (assert-true "any student in art"
      (any-factp ((?s student)) (eq ?s:subject art)))
   (assert-true "no student with grade > 100"
      (not (any-factp ((?s student)) (> ?s:grade 100))))
   (assert-true "no student named frank"
      (not (any-factp ((?s student)) (eq ?s:name frank))))

   ;; -- find-fact --
   (printout t crlf "-- find-fact --" crlf)
   (bind ?result (find-fact ((?s student)) (eq ?s:name alice)))
   (assert-equal "find-fact alice returns 1 result" 1 (length$ ?result))
   (bind ?alice-fact (nth$ 1 ?result))
   (assert-equal "alice grade = 92" 92 (fact-slot-value ?alice-fact grade))

   (bind ?result (find-fact ((?s student)) (eq ?s:name nonexistent)))
   (assert-equal "find-fact nonexistent returns 0" 0 (length$ ?result))

   ;; -- find-all-facts --
   (printout t crlf "-- find-all-facts --" crlf)
   (bind ?math-students (find-all-facts ((?s student)) (eq ?s:subject math)))
   (assert-equal "2 math students" 2 (length$ ?math-students))

   (bind ?high-grades (find-all-facts ((?s student)) (>= ?s:grade 88)))
   (assert-equal "3 students with grade >= 88" 3 (length$ ?high-grades))

   (bind ?all-students (find-all-facts ((?s student)) TRUE))
   (assert-equal "5 total students" 5 (length$ ?all-students))

   ;; -- do-for-fact --
   (printout t crlf "-- do-for-fact --" crlf)
   (bind ?found-name none)
   (do-for-fact ((?s student)) (eq ?s:name bob)
      (bind ?found-name ?s:name))
   (assert-equal "do-for-fact found bob" bob ?found-name)

   ;; -- do-for-all-facts (count high achievers) --
   (printout t crlf "-- do-for-all-facts --" crlf)
   (bind ?count 0)
   (do-for-all-facts ((?s student)) (> ?s:grade 80)
      (bind ?count (+ ?count 1)))
   (assert-equal "3 students with grade > 80" 3 ?count)

   ;; -- multi-template queries (join) --
   (printout t crlf "-- Multi-template Queries --" crlf)
   ;; Find student-course pairs where subjects match departments
   (bind ?matches (find-all-facts ((?s student) (?c course))
      (eq ?s:subject ?c:department)))
   ;; alice(math)+algebra, alice(math)+calculus, bob(science)+physics,
   ;; carol(math)+algebra, carol(math)+calculus, dave(science)+physics, eve(art)+painting = 7
   (assert-equal "7 student-course matches" 7 (length$ ?matches))

   ;; -- delayed-do-for-all-facts --
   (printout t crlf "-- delayed-do-for-all-facts --" crlf)
   (bind ?total-credits 0)
   (delayed-do-for-all-facts ((?c course)) TRUE
      (bind ?total-credits (+ ?total-credits ?c:credits)))
   (assert-equal "total credits = 13" 13 ?total-credits)

   ;; -- Instance queries --
   (printout t crlf "-- Instance Queries --" crlf)
   (make-instance laptop of Product (product-name "Laptop") (price 999.99) (category electronics))
   (make-instance phone of Product (product-name "Phone") (price 699.99) (category electronics))
   (make-instance book of Product (product-name "Book") (price 19.99) (category books))
   (make-instance shirt of Product (product-name "Shirt") (price 29.99) (category clothing))
   (make-instance headphones of Product (product-name "Headphones") (price 149.99) (category electronics))

   (assert-true "any electronics product"
      (any-instancep ((?p Product)) (eq ?p:category electronics)))

   (bind ?electronics (find-all-instances ((?p Product)) (eq ?p:category electronics)))
   (assert-equal "3 electronics products" 3 (length$ ?electronics))

   (bind ?expensive (find-all-instances ((?p Product)) (> ?p:price 100.0)))
   (assert-equal "3 products > $100" 3 (length$ ?expensive))

   (bind ?cheap-count 0)
   (do-for-all-instances ((?p Product)) (< ?p:price 50.0)
      (bind ?cheap-count (+ ?cheap-count 1)))
   (assert-equal "2 products < $50" 2 ?cheap-count)

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
