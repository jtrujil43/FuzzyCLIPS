;;;======================================================
;;; Test 10: COOL — CLIPS Object-Oriented Language
;;; Tests defclass, make-instance, send, message-handlers,
;;; inheritance, instance queries, slot access
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 10: COOL Object System")

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

;;; ==== CLASS DEFINITIONS ====

;; Base class: Shape
(defclass Shape (is-a USER)
   (role concrete)
   (slot color (type SYMBOL) (default red))
   (slot area (type FLOAT) (default 0.0)))

;; Rectangle inherits from Shape
(defclass Rectangle (is-a Shape)
   (slot width (type FLOAT) (default 1.0))
   (slot height (type FLOAT) (default 1.0)))

;; Circle inherits from Shape
(defclass Circle (is-a Shape)
   (slot radius (type FLOAT) (default 1.0)))

;; Triangle inherits from Shape
(defclass Triangle (is-a Shape)
   (slot base (type FLOAT) (default 1.0))
   (slot tri-height (type FLOAT) (default 1.0)))

;; Message handlers for computing area
(defmessage-handler Rectangle compute-area ()
   (bind ?self:area (* ?self:width ?self:height))
   ?self:area)

(defmessage-handler Circle compute-area ()
   (bind ?self:area (* 3.14159 ?self:radius ?self:radius))
   ?self:area)

(defmessage-handler Triangle compute-area ()
   (bind ?self:area (* 0.5 ?self:base ?self:tri-height))
   ?self:area)

;; Message handler for description
(defmessage-handler Shape describe ()
   (str-cat "Shape: color=" (sym-cat ?self:color) " area=" (format nil "%.2f" ?self:area)))

;; Animal hierarchy for deeper inheritance
(defclass Animal (is-a USER)
   (role concrete)
   (slot name (type STRING) (default "unknown"))
   (slot legs (type INTEGER) (default 0))
   (slot sound (type STRING) (default "")))

(defclass Dog (is-a Animal)
   (slot breed (type STRING) (default "mutt")))

(defclass Cat (is-a Animal)
   (slot indoor (type SYMBOL) (default yes)))

(defmessage-handler Dog init after ()
   (bind ?self:legs 4)
   (bind ?self:sound "woof"))

(defmessage-handler Cat init after ()
   (bind ?self:legs 4)
   (bind ?self:sound "meow"))

(defmessage-handler Animal speak ()
   (str-cat ?self:name " says " ?self:sound))

;;; ==== TESTS ====

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; -- Instance creation --
   (printout t crlf "-- Instance Creation --" crlf)
   (bind ?s (save-counters))
   (reset)
   (restore-counters ?s)

   (bind ?r1 (make-instance rect1 of Rectangle (width 5.0) (height 3.0) (color blue)))
   (assert-true "rect1 created" (instancep ?r1))
   (assert-true "rect1 is Rectangle" (eq (class ?r1) Rectangle))

   (bind ?c1 (make-instance circ1 of Circle (radius 2.0) (color green)))
   (assert-true "circ1 created" (instancep ?c1))

   (bind ?t1 (make-instance tri1 of Triangle (base 6.0) (tri-height 4.0)))
   (assert-true "tri1 created" (instancep ?t1))

   ;; -- Slot access --
   (printout t crlf "-- Slot Access --" crlf)
   (assert-true "rect1 width = 5.0" (= (send ?r1 get-width) 5.0))
   (assert-true "rect1 height = 3.0" (= (send ?r1 get-height) 3.0))
   (assert-equal "rect1 color = blue" blue (send ?r1 get-color))
   (assert-true "circ1 radius = 2.0" (= (send ?c1 get-radius) 2.0))

   ;; -- Compute area via message --
   (printout t crlf "-- Message Sending --" crlf)
   (bind ?rect-area (send ?r1 compute-area))
   (assert-true "rect area = 15.0" (approx-equal ?rect-area 15.0 0.01))

   (bind ?circ-area (send ?c1 compute-area))
   (assert-true "circle area ~ 12.566" (approx-equal ?circ-area 12.566 0.01))

   (bind ?tri-area (send ?t1 compute-area))
   (assert-true "triangle area = 12.0" (approx-equal ?tri-area 12.0 0.01))

   ;; -- Modify slot via put- --
   (printout t crlf "-- Slot Modification --" crlf)
   (send ?r1 put-color red)
   (assert-equal "rect1 color changed to red" red (send ?r1 get-color))
   (send ?r1 put-width 10.0)
   (bind ?new-area (send ?r1 compute-area))
   (assert-true "rect area after width=10 is 30.0" (approx-equal ?new-area 30.0 0.01))

   ;; -- Inheritance --
   (printout t crlf "-- Inheritance --" crlf)
   (assert-true "Rectangle is subclass of Shape"
      (subclassp Rectangle Shape))
   (assert-true "Circle is subclass of Shape"
      (subclassp Circle Shape))
   (assert-true "rect1 is-a Shape" (eq (type ?r1) Rectangle))

   ;; -- Animal hierarchy with init handlers --
   (printout t crlf "-- Animal Hierarchy --" crlf)
   (bind ?d1 (make-instance fido of Dog (name "Fido") (breed "Labrador")))
   (bind ?c2 (make-instance whiskers of Cat (name "Whiskers") (indoor yes)))

   (assert-equal "fido legs = 4" 4 (send ?d1 get-legs))
   (assert-equal "fido sound = woof" "woof" (send ?d1 get-sound))
   (assert-equal "fido breed = Labrador" "Labrador" (send ?d1 get-breed))
   (assert-equal "whiskers legs = 4" 4 (send ?c2 get-legs))
   (assert-equal "whiskers sound = meow" "meow" (send ?c2 get-sound))

   ;; -- speak message (inherited) --
   (assert-equal "fido speaks" "Fido says woof" (send ?d1 speak))
   (assert-equal "whiskers speaks" "Whiskers says meow" (send ?c2 speak))

   ;; -- Instance predicates --
   (printout t crlf "-- Instance Predicates --" crlf)
   (assert-true "instance-existp fido" (instance-existp [fido]))
   (assert-true "instancep fido" (instancep ?d1))

   ;; -- Unmake instance --
   (unmake-instance ?d1)
   (assert-true "fido unmade" (not (instance-existp [fido])))

   ;; -- Instance queries --
   (printout t crlf "-- Instance Queries --" crlf)
   (assert-true "find-all-instances Shape"
      (> (length$ (find-all-instances ((?s Shape)) TRUE)) 0))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
