# ORE XML Schemas

A verbatim, unmodified sync of the ORE Engine fork's own `xsd/`
directory -- the XSD schemas ORE's own input XML (and this repo's
vendored `external/ore/examples/`) validate against.

Kept in lockstep with `external/ore/examples/` (both synced from the
same engine commit in one invocation) -- a schema/example version
mismatch produces spurious validation failures unrelated to either side
actually being wrong. See `external/ore/examples/methodology.txt` for
the full sync methodology (this directory doesn't repeat it).

## Quick Start

```bash
./external/ore/tools/update_ore_samples.sh ~/Development/ORE/Engine
```

## Data Files

| Path | Description |
|------|--------------|
| `*.xsd` | Every ORE input schema, synced verbatim (`input.xsd` is the entry point `xmllint --schema` validates against). |
| `check.sh` / `check.cmd` | Upstream's own schema-validation helper scripts (kept for reference; this repo's own equivalent is `scripts/validate_ore_examples.sh`). |
| `manifest.json` | Provenance: source engine commit/date, sync timestamp, file count, and a `tree_sha256`. |

## Regenerating

Never hand-edit any `.xsd` file here. See
`external/ore/examples/README.md`/`methodology.txt` for the full sync +
validate methodology (one script call syncs both directories together).

## Related Files

- Sync script: `external/ore/tools/update_ore_samples.sh`
- Validation: `scripts/validate_ore_examples.sh`
- Examples (synced in lockstep): `external/ore/examples/`
