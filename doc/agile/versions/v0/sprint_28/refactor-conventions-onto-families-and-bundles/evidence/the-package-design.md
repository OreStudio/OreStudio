# Synthesis: conventions package

Base: candidate 3. Grafts from candidates 1 and 2. Judged by the cross-judge and by me.

## The shape

Continuously true across all three: the aggregate dies, a family is a registry key plus one payload per row, both obligations bracket the family loop inside the caller's unit of work, one apply path serves both imports, and PR 1 is a test-only baseline using byte equality on exported files.

- `refdata::domain::conventions_bundle` is generated from the registry. It holds `convention_family_rows { family, rows[] }`, where each row is one row's JSON as its own type. No member is a family's value type, and nothing reflects the set.
- `conventions_bundle_service::apply(bundle)` and `::read(party)` are the only writer and the only reader. `apply` does not open a unit of work, so the commit passes its transaction and the run store passes its own.
- The two tenor basis swap obligations are the prologue and epilogue of `apply`, in the caller's transaction. Prologue: resolve identity after stamping. Epilogue: close the omitted leg form.
- `ores::ore::domain::conventions_package` is ores.ore's. It holds one bundle and the FX projection, which is never stored and never crosses.
- `split_conventions` and `join_conventions` replace `conventions_mapper::map` and `reverse`. They are generated per family, one family per translation unit, with a dispatch unit that includes only declarations.
- The wire does not change.

## Grafts

1. From candidate 2: per-family translation units for the refdata reads and the ORE bindings. This is the only change that makes "no translation unit reflects every family" true rather than narrowed, and it addresses the MSVC cause directly.
2. From candidate 2: a translation-unit budget gate in codegen, so a unit that reflects more than one family fails a check instead of an MSVC build.
3. From candidate 1: `convention_family_key`, generated from the registry with a `static_assert` tying it to the table, so a family nobody handled fails the build.
4. From candidate 1: the behaviour change stated as a `[!shouldfail]` test, which goes green in the PR that fixes it and cannot be satisfied early.
5. From candidate 1's ordering answer: pin the family write order to registry key order. `convention` sorts before both `tenor_basis_swap_*` keys, and the part rows carry a live foreign key to the central row with `bump_parent_version`.

## Rejected

- Candidate 2's routing of the in-process run import through begin, stage and commit. It buys one small shared type and costs a staging row per convention row plus a transaction per stage call. A bundle is related families under one unit of work, not a thing that goes through the staging table.
- Candidate 1's preservation of the run store's non-transactional write. The story's acceptance demands one unit of work, and preserving the defect leaves the task open by design.
- The `#+ore_conventions_element:` frontmatter. It removes a collection name from hand code but leaves the per-family row mapping hand-written, so adding a family still needs hand code either way. It costs 23 model edits and buys a name. Dropped.
- A generated aggregate. It leaves the god type in place with a generator in front of it.
