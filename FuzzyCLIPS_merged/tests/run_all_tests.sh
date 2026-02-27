#!/bin/bash
###############################################
# FuzzyCLIPS Unit Test Runner
###############################################

FUZZYCLIPS=""
TESTS_DIR="$(cd "$(dirname "$0")" && pwd)"
TOTAL_PASS=0
TOTAL_FAIL=0
SUITES_PASS=0
SUITES_FAIL=0
FAILED_SUITES=""

# Find the binary
for candidate in "$TESTS_DIR/../fuzzyclips" "../fuzzyclips" "./fuzzyclips"; do
    if [[ -x "$candidate" ]]; then
        FUZZYCLIPS="$candidate"
        break
    fi
done

if [[ -z "$FUZZYCLIPS" ]]; then
    echo "ERROR: fuzzyclips binary not found or not executable"
    exit 1
fi

echo "============================================"
echo " FuzzyCLIPS Unit Test Runner"
echo " $(date)"
echo "============================================"
echo ""

for test_file in "$TESTS_DIR"/test_*.clp; do
    test_name=$(basename "$test_file")
    echo "Running: $test_name"
    echo "--------------------------------------------"

    # Run the test, capture output
    output=$("$FUZZYCLIPS" -f "$test_file" 2>&1) || true

    # Only print lines that are actual test output (PASS/FAIL/Results/SUITE/===)
    echo "$output" | grep -E "^\s*(PASS|FAIL|---|\=\=\=|SUITE|INFO)" || true

    # Parse pass/fail from the "--- Results:" summary line only
    results_line=$(echo "$output" | grep "^--- Results:" | tail -1)
    pass=$(echo "$results_line" | grep -oP '(\d+) passed' | grep -oP '\d+')
    fail=$(echo "$results_line" | grep -oP '(\d+) failed' | grep -oP '\d+')

    if [[ -z "$pass" ]]; then pass=0; fi
    if [[ -z "$fail" ]]; then fail=0; fi

    TOTAL_PASS=$((TOTAL_PASS + pass))
    TOTAL_FAIL=$((TOTAL_FAIL + fail))

    if echo "$output" | grep -q "^SUITE PASSED"; then
        SUITES_PASS=$((SUITES_PASS + 1))
        echo "  => SUITE PASSED ($pass passed)"
    elif echo "$output" | grep -q "SUITE PASSED"; then
        SUITES_PASS=$((SUITES_PASS + 1))
        echo "  => SUITE PASSED ($pass passed)"
    else
        SUITES_FAIL=$((SUITES_FAIL + 1))
        FAILED_SUITES="$FAILED_SUITES  - $test_name\n"
        echo "  => SUITE FAILED ($pass passed, $fail failed)"
    fi
    echo ""
done

echo "============================================"
echo " OVERALL RESULTS"
echo "============================================"
echo " Test suites: $SUITES_PASS passed, $SUITES_FAIL failed"
echo " Test cases:  $TOTAL_PASS passed, $TOTAL_FAIL failed"
echo "============================================"

if [[ $SUITES_FAIL -gt 0 ]]; then
    echo ""
    echo "Failed suites:"
    echo -e "$FAILED_SUITES"
    exit 1
else
    echo ""
    echo "ALL SUITES PASSED"
    exit 0
fi
