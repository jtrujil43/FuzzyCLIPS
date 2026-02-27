# FuzzyCLIPS 6.42a — Merged Build

This project merges the **FuzzyCLIPS** fuzzy-logic extensions (originally based on
CLIPS 6.05 / 6.10, circa 1997–2004) into the modern **CLIPS 6.42** codebase
(released 2025-02-13).

## Merge Strategy

**CLIPS 6.42 as the base, FuzzyCLIPS ported on top.**

| Old API (FuzzyCLIPS / CLIPS 6.05)         | New API (CLIPS 6.42)                          |
|--------------------------------------------|------------------------------------------------|
| `globle` / `LOCALE` / `VOID` macros       | standard C `static` / `void`                  |
| No `Environment *` parameter               | Every function receives `Environment *`        |
| `DATA_OBJECT`                              | `CLIPSValue` / `UDFValue`                     |
| `DefineFunction2()`                        | `AddUDF()`                                     |
| `PrintRouter()`                            | `WriteString()`                                |
| `gm2()` / `rm()`                          | `genalloc()` / `genfree()`                    |
| `TRUE` / `FALSE`                           | `true` / `false` (`<stdbool.h>`)              |

### Valid AddUDF Return-Type Codes

    b d e f i l m n s y v * ;

> `"u"` and `"w"` are **NOT** valid in CLIPS 6.42. Use `"*"` (any type) or `"y"`
> (symbol) instead.

---

## Files Modified from CLIPS 6.42

| File            | Changes                                                                          |
|-----------------|----------------------------------------------------------------------------------|
| `setup.h`       | Banner → `"FuzzyCLIPS Version 6.42a"`, added `FUZZY_DEFTEMPLATES` and `CERTAINTY_FACTORS` feature flags |
| `constant.h`    | Added `FUZZY_VALUE_TYPE` (10), `FUZZY_VALUE_BIT`, and fuzzy function constants   |
| `entities.h`    | Added `CLIPSFuzzyValue` struct and `fuzzyValue` member in `CLIPSValue`/`UDFValue` unions |
| `symbol.h`      | Declared fuzzy value hash-table functions (`AddFuzzyValue`, `RetainFuzzyValue`, `ReleaseFuzzyValue`, etc.) |
| `symbol.c`      | Implemented the fuzzy value hash-table functions                                 |
| `tmpltdef.h`    | Added `fuzzyTemplate` bit-field, `fuzzyList` pointer, `#include "fuzzylv.h"`     |
| `tmpltdef.c`    | Calls `InitializeFuzzy()` and `InitializeCF()` after `InitializeFacts()`          |
| `factmngr.h`    | Added `double certaintyFactor` to the `Fact` struct (inside `#if CERTAINTY_FACTORS`) |

## New Files (Fuzzy Extensions)

### Headers
`cfdef.h` · `fuzzycom.h` · `fuzzydef.h` · `fuzzylhs.h` · `fuzzylv.h` ·
`fuzzymod.h` · `fuzzypsr.h` · `fuzzyrhs.h` · `fuzzyutl.h` · `fuzzyval.h`

### Implementations
`cfdef.c` · `fuzzycom.c` · `fuzzydef.c` · `fuzzylhs.c` · `fuzzymod.c` ·
`fuzzypsr.c` · `fuzzyrhs.c` · `fuzzyutl.c`

---

## Building

### Linux / WSL

```bash
cd FuzzyCLIPS_merged
make clean
make
```

Produces the `fuzzyclips` binary and `libclips.a`.

### Requirements
- GCC with C99 support
- GNU Make

### Windows (MSVC)

```
nmake -f makefile.win
```

---

## Running

```bash
./fuzzyclips          # interactive CLIPS REPL
./fuzzyclips -f2 file.clp   # batch-execute a CLIPS file
```

Inside the REPL you can verify the fuzzy extensions are loaded:

```
CLIPS> (get-threshold)
0.1
CLIPS> (set-threshold 0.3)
0.3
CLIPS> (pi-function 0.0 10.0)    ;; returns a fuzzy value
```

---

## Running the Unit Tests

```bash
cd tests
bash run_all_tests.sh
```

### Test Suites

| Suite | File | Tests | Description |
|-------|------|-------|-------------|
| 01 | `test_01_basic.clp` | 10 | Arithmetic, string, multifield operations |
| 02 | `test_02_rules.clp` | 15 | Rule firing, salience, conflict resolution |
| 03 | `test_03_fuzzy_commands.clp` | 13 | Fuzzy UDF commands (threshold, modifiers, S/Z/PI) |
| 04 | `test_04_certainty_factors.clp` | 12 | Certainty factors: get-cf, threshold filtering |
| 05 | `test_05_constructs.clp` | 14 | Deftemplates, deffacts, defglobals, modules |

**Total: 64 test cases**

### Expected Output

```
============================================
 FuzzyCLIPS Unit Test Runner
============================================
Running: test_01_basic.clp        ... PASSED (10/10)
Running: test_02_rules.clp        ... PASSED (15/15)
Running: test_03_fuzzy_commands.clp ... PASSED (13/13)
Running: test_04_certainty_factors.clp ... PASSED (12/12)
Running: test_05_constructs.clp   ... PASSED (14/14)
============================================
 TOTAL: 64 passed, 0 failed
 ALL TESTS PASSED
============================================
```

---

## Architecture Notes

- All fuzzy code is guarded by `#if FUZZY_DEFTEMPLATES` / `#if CERTAINTY_FACTORS`
  preprocessor flags (defined in `setup.h`). Setting either flag to `0` yields a
  standard CLIPS 6.42 build.
- Fuzzy values are stored in a dedicated hash table managed through `symbol.c`,
  following the same retain/release pattern used for symbols, floats, and integers.
- The certainty-factor subsystem adds a `double certaintyFactor` field to every
  `Fact` struct and provides threshold-based filtering via `get-threshold` /
  `set-threshold`.

---

## License

CLIPS is distributed under its own license (see the original CLIPS 6.42 distribution).
FuzzyCLIPS extensions were originally developed by the Integrated Reasoning Group
at the National Research Council of Canada.
