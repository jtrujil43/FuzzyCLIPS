;;;======================================================
;;; Test 08: String Functions
;;; Tests str-cat, str-compare, sub-string, str-index,
;;; str-replace, eval, build, string-to-field, format
;;;======================================================

(defglobal ?*test-pass* = 0)
(defglobal ?*test-fail* = 0)
(defglobal ?*test-name* = "Test 08: String Functions")

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

(deffunction run-tests ()
   (printout t crlf "=== " ?*test-name* " ===" crlf)

   ;; -- str-cat --
   (printout t crlf "-- str-cat --" crlf)
   (assert-equal "str-cat two strings" "foobar" (str-cat "foo" "bar"))
   (assert-equal "str-cat mixed types" "abc123" (str-cat "abc" 123))
   (assert-equal "str-cat empty" "" (str-cat ""))
   (assert-equal "str-cat symbol+string" "helloworld" (str-cat hello "world"))

   ;; -- sym-cat --
   (printout t crlf "-- sym-cat --" crlf)
   (assert-equal "sym-cat two symbols" abc-def (sym-cat abc - def))
   (assert-true "sym-cat returns symbol" (symbolp (sym-cat a b c)))

   ;; -- str-length --
   (printout t crlf "-- str-length --" crlf)
   (assert-equal "str-length 0" 0 (str-length ""))
   (assert-equal "str-length 5" 5 (str-length "hello"))
   (assert-equal "str-length 11" 11 (str-length "hello world"))

   ;; -- upcase / lowcase --
   (printout t crlf "-- Case Conversion --" crlf)
   (assert-equal "upcase string" "HELLO" (upcase "hello"))
   (assert-equal "lowcase string" "hello" (lowcase "HELLO"))
   (assert-equal "upcase mixed" "HELLO WORLD" (upcase "Hello World"))
   (assert-equal "lowcase mixed" "hello world" (lowcase "Hello World"))
   ;; upcase/lowcase on symbols
   (assert-equal "upcase symbol" ABC (upcase abc))
   (assert-equal "lowcase symbol" abc (lowcase ABC))

   ;; -- str-compare --
   (printout t crlf "-- str-compare --" crlf)
   (assert-equal "str-compare equal" 0 (str-compare "abc" "abc"))
   (assert-true "str-compare less" (< (str-compare "abc" "def") 0))
   (assert-true "str-compare greater" (> (str-compare "def" "abc") 0))

   ;; -- sub-string --
   (printout t crlf "-- sub-string --" crlf)
   (assert-equal "sub-string 1 5" "hello" (sub-string 1 5 "hello world"))
   (assert-equal "sub-string 7 11" "world" (sub-string 7 11 "hello world"))
   (assert-equal "sub-string 1 1" "h" (sub-string 1 1 "hello"))
   (assert-equal "sub-string full" "hello" (sub-string 1 5 "hello"))

   ;; -- str-index --
   (printout t crlf "-- str-index --" crlf)
   (assert-equal "str-index found" 7 (str-index "world" "hello world"))
   (assert-equal "str-index at start" 1 (str-index "hello" "hello world"))
   (assert-equal "str-index not found" FALSE (str-index "xyz" "hello world"))

   ;; -- string-to-field --
   (printout t crlf "-- string-to-field --" crlf)
   (assert-equal "string-to-field int" 42 (string-to-field "42"))
   (assert-true "string-to-field float" (= 3.14 (string-to-field "3.14")))
   (assert-equal "string-to-field symbol" abc (string-to-field "abc"))

   ;; -- eval --
   (printout t crlf "-- eval --" crlf)
   (assert-equal "eval arithmetic" 10 (eval "(+ 3 7)"))
   (assert-equal "eval str-cat" "ab" (eval "(str-cat \"a\" \"b\")"))
   (assert-true "eval nested" (= 25 (eval "(* 5 5)")))

   ;; -- build --
   (printout t crlf "-- build --" crlf)
   (assert-true "build defglobal" (build "(defglobal ?*test-build-var* = 99)"))
   (assert-equal "built defglobal value" 99 ?*test-build-var*)

   ;; -- format --
   (printout t crlf "-- format --" crlf)
   (assert-equal "format integer" "Value: 42" (format nil "Value: %d" 42))
   (assert-equal "format float" "Pi: 3.14" (format nil "Pi: %.2f" 3.14159))
   (assert-equal "format string" "Hello World" (format nil "%s %s" "Hello" "World"))
   (assert-equal "format padded" "  42" (format nil "%4d" 42))

   ;; -- number-to-string conversions --
   (printout t crlf "-- Type Conversions --" crlf)
   (assert-true "numberp after eval" (numberp (eval "(+ 1 1)")))
   (assert-true "stringp str-cat result" (stringp (str-cat "a" "b")))

   ;; Summary
   (printout t crlf "--- Results: " ?*test-pass* " passed, " ?*test-fail* " failed ---" crlf)
   (if (> ?*test-fail* 0) then (printout t "SUITE FAILED" crlf) else (printout t "SUITE PASSED" crlf))
   (return ?*test-fail*))

(run-tests)
(exit)
