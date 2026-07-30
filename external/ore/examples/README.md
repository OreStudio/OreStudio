# ORE Engine Examples

A sync of the ORE Engine fork's own `Examples/` directory -- real example
trade portfolios, market data, and expected outputs, used to smoke-test
packaged `ore` engine binaries (`external/ore/packages/`) and as
reference input data across this repo (e.g.
`external/ore/packages/TA002_IR_Swap.tar.gz`).

## Quick Start

```bash
# Sync from a checkout of the ORE Engine fork (also syncs external/ore/xsd/
# and validates every example against it)
./external/ore/tools/update_ore_samples.sh ~/Development/ORE/Engine
```

## Data Files

| Path | Description |
|------|--------------|
| `Academy/`, `AmericanMonteCarlo/`, ..., `XvaRisk/` | One directory per ORE example, each with its own `Input/`, `ExpectedOutput/`, and (where upstream provides one) `run.py`/`Readme.md`. Synced verbatim, except 3 files with a known-and-reapplied upstream FX-key bugfix -- see `methodology.txt`. |
| `manifest.json` | Provenance: source engine commit/date, sync timestamp, file count, and a `tree_sha256` covering every vendored file. |

## Regenerating

Never hand-edit any file under this directory (the 3 exceptions are
patched automatically by the sync script itself, not by hand -- see
`methodology.txt`). To pick up upstream changes, see
`external/ore/methodology.txt` for the full methodology -- in short:

```bash
./external/ore/tools/update_ore_samples.sh <path-to-ore-engine-checkout>
```

This syncs both `external/ore/examples/` and `external/ore/xsd/` from the
same engine commit, reapplies the 3 known hand-patches, then validates
every example XML against the freshly synced schema
(`scripts/validate_ore_examples.sh`).

## Data Source

ORE Engine, OreStudio's fork of OpenSourceRisk/Engine:
https://github.com/OreStudio/Engine (fork) /
https://github.com/OpenSourceRisk/Engine (upstream).

## Related Files

- Sync script: `external/ore/tools/update_ore_samples.sh`
- Validation: `scripts/validate_ore_examples.sh`
- Full methodology: `external/ore/methodology.txt`
- Schema: `external/ore/xsd/` (kept in lockstep, see its own README.md)
- Consuming package: `external/ore/packages/` (self-tests against `TA002_IR_Swap`, sourced from `Academy/TA002_IR_Swap/Input/`)
