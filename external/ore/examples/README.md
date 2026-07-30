# ORE Engine Examples

A verbatim, unmodified sync of the ORE Engine fork's own `Examples/`
directory -- real example trade portfolios, market data, and expected
outputs, used to smoke-test packaged `ore` engine binaries
(`external/ore/packages/`) and as reference input data across this repo
(e.g. `external/ore/packages/TA002_IR_Swap.tar.gz`).

## Quick Start

```bash
# Sync from a checkout of the ORE Engine fork (also syncs external/ore/xsd/
# and validates every example against it)
./external/ore/tools/update_ore_samples.sh ~/Development/ORE/Engine
```

## Data Files

| Path | Description |
|------|--------------|
| `Academy/`, `AmericanMonteCarlo/`, ..., `XvaRisk/` | One directory per ORE example, each with its own `Input/`, `ExpectedOutput/`, and (where upstream provides one) `run.py`/`Readme.md`. Synced verbatim -- never hand-edited. |
| `manifest.json` | Provenance: source engine commit/date, sync timestamp, file count, and a `tree_sha256` covering every vendored file -- lets anyone verify a re-sync against the same commit reproduces byte-identical content. |

## Regenerating

Never hand-edit any file under this directory. To pick up upstream
changes, follow `methodology.txt` in full -- in short:

```bash
./external/ore/tools/update_ore_samples.sh <path-to-ore-engine-checkout>
```

This syncs both `external/ore/examples/` and `external/ore/xsd/` from the
same engine commit, then validates every example XML against the freshly
synced schema (`scripts/validate_ore_examples.sh`) -- a stale schema
relative to the examples (or vice versa) is exactly the kind of drift
this two-directory sync avoids by construction.

## Data Source

ORE Engine, OreStudio's fork of OpenSourceRisk/Engine:
https://github.com/OreStudio/Engine (fork) /
https://github.com/OpenSourceRisk/Engine (upstream).

## Related Files

- Sync script: `external/ore/tools/update_ore_samples.sh`
- Validation: `scripts/validate_ore_examples.sh`
- Methodology: `external/ore/examples/methodology.txt`
- Schema: `external/ore/xsd/` (kept in lockstep, see its own README.md)
- Consuming package: `external/ore/packages/` (self-tests against `TA002_IR_Swap`, sourced from `Academy/TA002_IR_Swap/Input/`)
