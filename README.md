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
| Fuzzy modifier (hedge) framework | Fully functional (not, very, somewhat, extremely, etc.) |
| Fuzzy deftemplate parsing | Fully functional (universe, primary terms, S/Z/PI, singletons) |
| Fuzzy LHS/RHS pattern matching | Fully functional (linguistic expressions, modifiers, AND/OR) |
| Fuzzy set operations (union/intersection) | Fully functional (funion, fintersect, maxmin_intersect) |
| Defuzzification (moment/maximum) | Fully functional (moment-defuzzify, maximum-defuzzify) |
| Fuzzy value UDF accessors | Fully functional (get-u, get-fs, create-fuzzy-value, etc.) |
| Fuzzy union/intersection/modify UDFs | Fully functional |
| Plot fuzzy value | Fully functional (text-based plot) |

> **Note:** The core fuzzy reasoning algorithms have been ported from the original
> FuzzyCLIPS source (CLIPS 6.05 era) to the modern CLIPS 6.42 API. The rule-engine
> integration functions (`computeFuzzyConsequence`, `changeValueOfFuzzySlots`) remain
> as documented stubs pending deeper integration with the 6.42 rule engine internals.

---

## Prerequisites

- **GCC** or Clang (any C99 compiler)
- **GNU Make**
- A **POSIX shell** (for `./configure` and the test runner)
- **Linux** or **macOS** (the platform is auto-detected; also builds under WSL)

## Building

FuzzyCLIPS uses a standard configure / make / test / install flow:

```bash
./configure          # detect the platform and write config.mk
make                 # build the fuzzyclips binary and libclips.a
make test            # build (if needed) and run the unit-test suite
sudo make install    # install under the prefix (default /usr/local)
```

`make` produces:
- `fuzzyclips` — the interactive console binary
- `libclips.a` — the static library

`make install` installs the binary to `$(bindir)`, the library to `$(libdir)`,
and the public headers to `$(includedir)` (`<prefix>/include/fuzzyclips`).
`make uninstall` removes them again, `make clean` deletes build products, and
`make distclean` additionally removes the generated `config.mk`.

### Configure Options

Run `./configure --help` for the full list. The most common options:

| Option | Effect |
|---|---|
| `--prefix=DIR` | Installation prefix (default `/usr/local`) |
| `--bindir=DIR` / `--libdir=DIR` / `--includedir=DIR` | Fine-grained install paths |
| `--enable-debug` | Build unoptimized with `-O0 -g` (default is `-O3` release) |
| `--enable-cpp` | Compile the sources as C++ (`g++ -std=c++11`) |
| `CC=...`, `CFLAGS=...`, `CPPFLAGS=...`, `LDFLAGS=...` | Override the compiler / flags |

Examples:
```bash
./configure --prefix=/opt/fuzzyclips     # install elsewhere
./configure --enable-debug               # debug build
./configure CC=clang                     # use a different compiler
make install DESTDIR=/tmp/stage          # staged install (for packaging)
```

The platform (`LINUX` / `DARWIN`) is detected automatically via `uname`.

To disable the fuzzy or certainty-factor extensions at compile time, set the
flags in `setup.h` and rebuild:
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

The `tests/` directory contains 15 test suites with 199 test cases covering core
CLIPS functionality and the fuzzy / certainty-factor extensions. The easiest way
to run them is `make test`, which builds the binary first if necessary.

### Test Suites

| File | Focus |
|---|---|
| `test_01_basic.clp` | Math, strings, type predicates, lists, fact system |
| `test_02_rules.clp` | Rule firing, salience, pattern matching, rule chaining |
| `test_03_fuzzy_commands.clp` | Fuzzy inference type, display precision, alpha value, UDF registration |
| `test_04_certainty_factors.clp` | `get-cf`, `set-threshold`, `unthreshold`, rule-based CF |
| `test_05_constructs.clp` | Deftemplate, deffacts, defglobal, deffunction, defrule, modify, queries |
| `test_06_procedural.clp` | Procedural functions, control flow, variable binding |
| `test_07_math.clp` | Arithmetic, trigonometric and numeric functions |
| `test_08_strings.clp` | String manipulation and predicates |
| `test_09_multifield.clp` | Multifield creation, slicing and functions |
| `test_10_cool.clp` | COOL — classes, instances, message passing |
| `test_11_queries.clp` | Fact- and instance-set query functions |
| `test_12_misc_io.clp` | I/O routers, formatting and miscellaneous commands |
| `test_13_generics.clp` | Generic functions and method dispatch |
| `test_14_fuzzy_deep.clp` | In-depth fuzzy deftemplates, sets and defuzzification |
| `test_15_cf_scenarios.clp` | End-to-end certainty-factor reasoning scenarios |

### Running All Tests

```bash
make test                    # preferred: builds first, then runs the suite
# or, if the binary is already built:
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
 Test suites: 15 passed, 0 failed
 Test cases:  199 passed, 0 failed
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
.
├── configure              # Generates config.mk (run this first)
├── Makefile               # Build system (includes config.mk)
├── makefile.win           # Native Windows (nmake) build
├── config.mk              # Generated by ./configure (git-ignored)
├── README.md              # This file
├── main.c                 # Entry point
├── setup.h                # Compile-time config (FUZZY_DEFTEMPLATES, CERTAINTY_FACTORS)
├── *.c / *.h              # CLIPS 6.42 core source
├── fuzzyval.h             # Fuzzy value struct definition
├── fuzzylv.h              # Fuzzy linguistic variable struct
├── fuzzydef.c/h           # Fuzzy initialization
├── fuzzycom.c/h           # Fuzzy UDF commands
├── fuzzyutl.c/h           # Fuzzy utility functions
├── fuzzypsr.c/h           # Fuzzy parser, S/Z/PI curves
├── fuzzylhs.c/h           # Fuzzy LHS pattern matching
├── fuzzyrhs.c/h           # Fuzzy RHS value handling
├── fuzzymod.c/h           # Fuzzy modifiers (hedges)
├── cfdef.c/h              # Certainty factor commands
├── Docs/                  # CLIPS and FuzzyCLIPS manuals (PDF/DOC)
└── tests/                 # Unit-test suites + run_all_tests.sh
    ├── run_all_tests.sh
    ├── test_01_basic.clp
    ├── ...
    └── test_15_cf_scenarios.clp
```

---

## Origins

- **CLIPS 6.42** — [https://clipsrules.net](https://clipsrules.net) — Gary Riley, released 2025
- **FuzzyCLIPS** — Bob Orchard, NRC Canada (National Research Council) — integrated fuzzy reasoning for CLIPS 6.05/6.10
- **This merge** — February 2026, porting FuzzyCLIPS extensions onto the CLIPS 6.42 codebase

## License

CLIPS is public domain software. The FuzzyCLIPS extensions follow the same terms.
