# FuzzyCLIPS 6.42a

**Fuzzy logic and certainty factor extensions for CLIPS 6.42**

FuzzyCLIPS is a fuzzy logic extension of the CLIPS (C Language Integrated
Production System) expert system shell from NASA. It was originally developed by
the Integrated Reasoning Group of the Institute for Information Technology of the
National Research Council of Canada. It enhances CLIPS by providing a fuzzy
reasoning capability that is fully integrated with CLIPS facts and the inference
engine, allowing one to represent and manipulate fuzzy facts and rules.

This version merges the original FuzzyCLIPS fuzzy extensions (based on
CLIPS 6.05/6.10, circa 1997–2004) onto the modern **CLIPS 6.42** codebase
(released 2025), bringing the fuzzy and certainty-factor capabilities forward
by roughly 20 years of CLIPS core improvements.

---

## Merge Strategy

The merge used **CLIPS 6.42 as the base** with the FuzzyCLIPS extensions ported
on top. This was necessary because the two codebases diverged enormously between
CLIPS 6.05 and 6.42 — essentially every API, data structure, and calling
convention changed.

### Key API Differences Bridged

| Aspect | Original FuzzyCLIPS (6.05) | Merged FuzzyCLIPS (6.42) |
|---|---|---|
| Function style | `globle` / `LOCALE` / `VOID` macros | Standard C99 types |
| Environment | No `Environment *` parameter | All functions take `Environment *` |
| UDF registration | `DefineFunction2()` | `AddUDF()` with type codes `bdefilmnsyv*;` |
| Data types | `DATA_OBJECT` | `CLIPSValue` / `UDFValue` / `UDFContext` |
| Output | `PrintRouter()` | `WriteString()` |
| Memory | `gm2()` / `rm()` | `genalloc()` / `genfree()` |
| Boolean | `int` with `TRUE`/`FALSE` | `<stdbool.h>` `bool` / `true` / `false` |

### Files Modified from CLIPS 6.42 Base

The following CLIPS 6.42 core files were modified to integrate fuzzy support:

- **setup.h** — Added `FUZZY_DEFTEMPLATES` (default 1) and `CERTAINTY_FACTORS`
  (default 1) compile-time flags. Updated banner to
  `"FuzzyCLIPS (6.42a 02/26/26)"`.
- **constant.h** — Added `FUZZY_VALUE_TYPE` (10), `FUZZY_VALUE_BIT`, and fuzzy
  function type constants (`S_FUNCTION`, `Z_FUNCTION`, `PI_FUNCTION`, etc.).
- **entities.h** — Added `CLIPSFuzzyValue` struct (with `TypeHeader`, reference
  count, and `struct fuzzy_value *contents`). Added `fuzzyValue` member to the
  `CLIPSValue` and `UDFValue` unions.
- **symbol.h** — Added fuzzy value hash table pointer to `symbolData`, plus
  declarations for `AddFuzzyValue`, `RetainFuzzyValue`, `ReleaseFuzzyValue`,
  `GetFuzzyValueTable`, and `SetFuzzyValueTable`.
- **symbol.c** — Implemented the fuzzy value hash table functions listed above,
  guarded by `#if FUZZY_DEFTEMPLATES`.
- **tmpltdef.h** — Added `fuzzyTemplate` bitfield and `fuzzyList` pointer to the
  `Deftemplate` struct, plus `#include "fuzzylv.h"`.
- **tmpltdef.c** — Added calls to `InitializeFuzzy(theEnv)` and
  `InitializeCF(theEnv)` in the deftemplate initialization path, guarded by
  the respective compile-time flags.
- **factmngr.h** — Added `double certaintyFactor` field to the `Fact` struct,
  guarded by `#if CERTAINTY_FACTORS`.

### New Fuzzy Extension Files

These files were created to implement the fuzzy/CF extensions:

| File | Purpose |
|---|---|
| `fuzzyval.h` | Core `struct fuzzy_value` definition (x/y arrays, name, etc.) |
| `fuzzylv.h` | `struct fuzzyLv` — fuzzy linguistic variable (universe of discourse, primary terms) |
| `fuzzydef.h` / `fuzzydef.c` | `InitializeFuzzy()` — bootstrap entry point; initializes modifiers, S/Z/PI curves |
| `fuzzycom.h` / `fuzzycom.c` | `DeffuzzyCommands()` — registers ~25 fuzzy UDF commands (defuzzify, get-u, get-fs, fuzzy-union/intersection/modify, inference type, display precision, alpha value, etc.) |
| `fuzzyutl.h` / `fuzzyutl.c` | Utility functions: `FZ_EQUAL`, `fcompliment`, `PrintFuzzySet`, `funion`, `fintersect` |
| `fuzzypsr.h` / `fuzzypsr.c` | Fuzzy parser: `Init_S_Z_PI_yvalues` (precomputed S/Z/PI curves), `sFunction`, `ParseFuzzyTemplate`, `InstallFuzzyValue`/`DeinstallFuzzyValue` |
| `fuzzylhs.h` / `fuzzylhs.c` | LHS fuzzy pattern matching: `GetFuzzyLHSPattern` |
| `fuzzyrhs.h` / `fuzzyrhs.c` | RHS fuzzy value handling: `CopyFuzzyValue`, `CompactFuzzyValue`, `ParseAssertFuzzyFact` |
| `fuzzymod.h` / `fuzzymod.c` | Fuzzy modifiers (hedges): `very`, `somewhat`, `more-or-less`, `slightly`, etc. via `concentrateFuzzyValue`, `dilateFuzzyValue`, `intensifyFuzzyValue`, `modifyFuzzyValue` |
| `cfdef.h` / `cfdef.c` | Certainty factors: `InitializeCF()`, `get-threshold`/`set-threshold`/`unthreshold`/`get-cf`, `combineCF` formula, `enable-rule-cf-calculation`/`disable-rule-cf-calculation` |

### Implementation Status

| Feature | Status |
|---|---|
| Core CLIPS 6.42 engine | Fully functional |
| Fuzzy UDF command registration | Fully functional (25+ commands) |
| Certainty factor commands | Fully functional (`get-cf`, `set-threshold`, `unthreshold`, etc.) |
| Fuzzy inference type get/set | Fully functional (`max-min`, `max-prod`) |
| Display precision / alpha value | Fully functional |
| Fuzzy modifier (hedge) framework | Framework implemented; hedge math complete |
| Fuzzy deftemplate parsing | Stub — needs full parser port |
| Fuzzy LHS/RHS pattern matching | Stub — needs full pattern matching port |
| Fuzzy set operations (union/intersection) | Stub — needs full algorithm port |
| Defuzzification (moment/maximum) | Stub — needs full algorithm port |

> **Note:** Many fuzzy operations are currently stubs that compile and register
> correctly but return placeholder values. The framework is complete and the
> porting of the remaining algorithm logic from the original FuzzyCLIPS source
> can proceed incrementally.

---

## Prerequisites

- **GCC** (or compatible C99 compiler)
- **GNU Make**
- **Linux / WSL** (tested on Ubuntu under WSL2)

## Building

```bash
cd /home/jovan/devel/Clips_code/FuzzyCLIPS_merged
make clean
make -j$(nproc)
```

This produces:
- `fuzzyclips` — the interactive console binary
- `libclips.a` — static library

### Build Options

The makefile uses these defaults:
```
CC = gcc
CFLAGS = -std=c99 -O3 -fno-strict-aliasing -Wall ...
```

To build in debug mode, edit the makefile or override:
```bash
make CFLAGS="-std=c99 -O0 -g -DLINUX"
```

To disable fuzzy or CF extensions at compile time, edit `setup.h`:
```c
#define FUZZY_DEFTEMPLATES 0   /* disable fuzzy support */
#define CERTAINTY_FACTORS  0   /* disable CF support */
```

---

## Running FuzzyCLIPS

### Interactive Mode

```bash
./fuzzyclips
```

You'll see the prompt:
```
     FuzzyCLIPS (6.42a 02/26/26)
FuzzyCLIPS>
```

### Batch Mode

```bash
./fuzzyclips -f myscript.clp
```

### Quick Fuzzy Command Examples

```clp
FuzzyCLIPS> (get-fuzzy-inference-type)
max-min
FuzzyCLIPS> (set-fuzzy-inference-type max-prod)
FuzzyCLIPS> (get-fuzzy-inference-type)
max-prod
FuzzyCLIPS> (get-fuzzy-display-precision)
4
FuzzyCLIPS> (get-threshold)
0.0
FuzzyCLIPS> (set-threshold 0.3)
FuzzyCLIPS> (get-threshold)
0.3
FuzzyCLIPS> (assert (my-fact 42))
<Fact-1>
FuzzyCLIPS> (get-cf 1)
1.0
```

---

## Unit Tests

The `tests/` directory contains 5 test suites with 64 test cases covering core
CLIPS functionality and the fuzzy/CF extensions.

### Test Suites

| File | Tests | Description |
|---|---|---|
| `test_01_basic.clp` | 21 | Math, strings, type predicates, lists, fact system |
| `test_02_rules.clp` | 6 | Rule firing, salience, pattern matching, rule chaining |
| `test_03_fuzzy_commands.clp` | 13 | Fuzzy inference type, display precision, alpha value, CF threshold, UDF registration |
| `test_04_certainty_factors.clp` | 6 | `get-cf`, `set-threshold`, `unthreshold`, rule-based CF |
| `test_05_constructs.clp` | 18 | Deftemplate, deffacts, defglobal, deffunction, defrule, modify, query functions |

### Running All Tests

```bash
bash tests/run_all_tests.sh
```

Expected output:
```
============================================
 FuzzyCLIPS Unit Test Runner
 ...
============================================

Running: test_01_basic.clp
--------------------------------------------
  PASS: addition 2+3=5
  ...
  => SUITE PASSED (21 passed)

...

============================================
 OVERALL RESULTS
============================================
 Test suites: 5 passed, 0 failed
 Test cases:  64 passed, 0 failed
============================================

ALL SUITES PASSED
```

### Running a Single Test

```bash
./fuzzyclips -f tests/test_03_fuzzy_commands.clp
```

---

## Directory Structure

```
FuzzyCLIPS_merged/
├── README.md              # This file
├── makefile               # Build system
├── main.c                 # Entry point
├── fuzzyclips             # Built binary
├── libclips.a             # Built static library
├── setup.h                # Compile-time config (FUZZY_DEFTEMPLATES, CERTAINTY_FACTORS)
├── *.c / *.h              # CLIPS 6.42 core source (~167 files)
├── fuzzyval.h             # Fuzzy value struct definition
├── fuzzylv.h              # Fuzzy linguistic variable struct
├── fuzzydef.c/h           # Fuzzy initialization
├── fuzzycom.c/h           # Fuzzy UDF commands (~25 registered)
├── fuzzyutl.c/h           # Fuzzy utility functions
├── fuzzypsr.c/h           # Fuzzy parser, S/Z/PI curves
├── fuzzylhs.c/h           # Fuzzy LHS pattern matching
├── fuzzyrhs.c/h           # Fuzzy RHS value handling
├── fuzzymod.c/h           # Fuzzy modifiers (hedges)
├── cfdef.c/h              # Certainty factor commands
└── tests/
    ├── run_all_tests.sh   # Test runner script
    ├── test_01_basic.clp
    ├── test_02_rules.clp
    ├── test_03_fuzzy_commands.clp
    ├── test_04_certainty_factors.clp
    └── test_05_constructs.clp
```

---

## Origins

- **CLIPS 6.42** — [https://clipsrules.net](https://clipsrules.net) — Gary Riley, released 2025
- **FuzzyCLIPS** — Bob Orchard, NRC Canada (National Research Council) — integrated fuzzy reasoning for CLIPS 6.05/6.10
- **This merge** — February 2026, porting FuzzyCLIPS extensions onto the CLIPS 6.42 codebase

## License

CLIPS is public domain software. The FuzzyCLIPS extensions follow the same terms.
