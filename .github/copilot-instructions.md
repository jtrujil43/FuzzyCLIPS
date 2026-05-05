# Copilot Instructions for FuzzyCLIPS

## Project Overview

FuzzyCLIPS 6.42a merges the original FuzzyCLIPS fuzzy-logic extensions (from CLIPS 6.05/6.10 era, ~1997–2004) onto the modern **CLIPS 6.42** codebase (2025). The active development directory is `FuzzyCLIPS_merged/`.

## Build Commands

```bash
cd FuzzyCLIPS_merged

make clean && make          # Release build (GCC, C99, -O3)
make debug                  # Debug build (-O0 -g)
make debug_cpp              # C++ debug build (g++, C++11)
make release_cpp            # C++ release build
```

Produces `fuzzyclips` binary and `libclips.a`. Requires GCC with C99 and GNU Make. Links `-lm`.

On Windows: `nmake -f makefile.win`

## Running Tests

```bash
cd FuzzyCLIPS_merged/tests
bash run_all_tests.sh               # Run all test suites
../fuzzyclips -f test_03_fuzzy_commands.clp  # Run a single test suite
```

Tests are `.clp` files that print PASS/FAIL lines and a `--- Results:` summary. The runner parses these to report overall status.

## Architecture

### Directory Layout

- **`FuzzyCLIPS_merged/`** — The canonical, buildable source tree (CLIPS 6.42 + fuzzy extensions)
- **`source/`** — Legacy FuzzyCLIPS source (CLIPS 6.05/6.10 era, reference only)
- **`merged_source/`** — Clean CLIPS 6.42 source without fuzzy extensions (for diffing)
- **`Docs/`** — PDF/DOC manuals for both CLIPS and FuzzyCLIPS

### Feature Flag Architecture

All fuzzy extensions are conditionally compiled via two flags in `setup.h`:

```c
#define FUZZY_DEFTEMPLATES  1   // Fuzzy deftemplate/value support
#define CERTAINTY_FACTORS   1   // Certainty factor on facts/rules
```

Setting either to `0` produces a standard CLIPS 6.42 build. All fuzzy code is guarded with `#if FUZZY_DEFTEMPLATES` / `#if CERTAINTY_FACTORS`.

### Core vs. Extension Files

**Modified CLIPS 6.42 files** (minimal, surgical changes):
`setup.h`, `constant.h`, `entities.h`, `symbol.h`, `symbol.c`, `tmpltdef.h`, `tmpltdef.c`, `factmngr.h`

**New fuzzy extension files:**
- `cfdef.c/h` — Certainty factor logic (possibility, necessity, similarity)
- `fuzzycom.c/h` — UDF commands (defuzzify, get-u, get-fs, plot, modifiers)
- `fuzzydef.c/h` — Fuzzy deftemplate definitions
- `fuzzylhs.c/h` — LHS fuzzy pattern parsing
- `fuzzyrhs.c/h` — RHS fuzzy value assertions
- `fuzzypsr.c/h` — Fuzzy value parser
- `fuzzymod.c/h` — Module support for fuzzy constructs
- `fuzzyutl.c/h` — Shared fuzzy utilities
- `fuzzylv.h` / `fuzzyval.h` — Data structure definitions

### Key Data Structures

- **`CLIPSFuzzyValue`** (in `entities.h`) — Hash-managed fuzzy value with `TypeHeader`, reference count, and `struct fuzzy_value *contents`
- **`struct fuzzy_value`** (in `fuzzylv.h`) — Universe of discourse (name, from, to) plus array of (x, y) points representing the membership function
- **Fact.certaintyFactor** — `double` field on every `Fact` struct (guarded by `#if CERTAINTY_FACTORS`)

## Key Conventions

### CLIPS 6.42 API Style (required for all new code)

| Pattern | Example |
|---------|---------|
| Every function takes `Environment *` as first param | `void InitializeFuzzy(Environment *theEnv)` |
| UDF signature | `void myFunc(Environment *env, UDFContext *ctx, UDFValue *ret)` |
| Register UDFs with `AddUDF()` | Valid return codes: `b d e f i l m n s y v * ;` |
| Output via `WriteString()` | Not `PrintRouter()` |
| Memory via `genalloc()` / `genfree()` | Not `gm2()` / `rm()` |
| Booleans | `bool` / `true` / `false` from `<stdbool.h>` |
| Data types | `CLIPSValue` / `UDFValue` / `UDFContext` (not `DATA_OBJECT`) |

**Important:** `"u"` and `"w"` are NOT valid `AddUDF` return-type codes in CLIPS 6.42. Use `"*"` (any type) or `"y"` (symbol).

### Fuzzy Value Memory Management

Fuzzy values use the same retain/release hash-table pattern as symbols, floats, and integers in CLIPS:
- `AddFuzzyValue()` — intern into hash table
- `RetainFuzzyValue()` / `ReleaseFuzzyValue()` — reference counting

### Header Guards

All headers use `#pragma once` plus traditional `#ifndef _H_name` / `#define _H_name` guards.

### Compiler Flags

The makefile passes `-D$(CLIPS_OS)` (either `LINUX` or `DARWIN`) at compile time. Platform detection is automatic via `uname -s`.
