# ORE instrument-convention extraction

This tool extracts the one canonical instrument-convention set from the ORE
example corpus. Extraction is mechanical and rerunnable. Curation is durable
and checked in.

An entry is a direct child of a `<Conventions>` root. The child's tag names the
kind. The entry has an `<Id>` child. The other children are its definition. An
Id sometimes carries more than one definition across the corpus. The tool
groups the definitions, applies the resolution rule, overlays the human
decisions, and writes the canonical set plus an audit trail.

Ids in one kind that share one identical definition but differ in spelling are
classified by the shape of the difference and handled by pattern:

- **A — `-CONVENTIONS` suffix rename.** Every Id strips to a single stem. The
  group is one convention. The `-CONVENTIONS` spelling is canonical and the
  other spelling becomes an alias.
- **B — vendor/exchange prefix twin.** Every Id is `PREFIX:STEM` and only the
  prefix differs. These are different feeds, so both stay canonical. The group
  is reported only.
- **C — anything else.** The tool does not guess. Every C group goes to the
  review file for a human.

## Run

```
./projects/ores.codegen/venv/bin/python tools/ore_conventions/extract.py
```

Options:

- `--examples DIR` — corpus root. Default `external/ore/examples`.
- `--decisions FILE` — curation overlay. Default this directory's
  `decisions.tsv`.
- `--out DIR` — output directory. Default `tmp/ore_conventions/`.

`--help` prints the same list. The tool exits 0 on success. It exits non-zero
only on a real failure: a bad examples path or an unreadable `decisions.tsv`.
Flagged Ids do not fail the run.

Run the tests with:

```
./projects/ores.codegen/venv/bin/python tools/ore_conventions/test_extract.py
```

## Output contract

The tool writes four files into `--out`.

### `conventions-canonical.tsv`

One row per canonical entry:

| column | meaning |
|---|---|
| `kind` | the entry's tag under `<Conventions>` |
| `id` | the canonical `<Id>` after any Pattern A rename |
| `signature` | the chosen normalised definition |
| `source_path` | a corpus file that carries it, relative to `--examples` |
| `variant_count` | number of distinct definitions seen for this `(kind, id)` |
| `decided_by` | `only-variant`, `rule-1` … `rule-4`, `pinned`, or `renamed` |
| `aliases` | the other Pattern A spellings, comma-separated, empty when none |

`decided_by` is `renamed` when the Id came from a Pattern A rename and no rule
or pin had to choose the definition. A rule or pin keeps its own value when
merged spellings disagree; the `aliases` column still records the rename.

### `conventions-aliases.tsv`

One row per resolved rename: `kind`, `alias_id`, `canonical_id`, `pattern`. The
only pattern today is `A`.

### `conventions-resolutions.md`

The audit trail. It starts with the stale pins and the flagged Ids in their own
sections. Then it gives a per-kind summary, the corpus defects, every
conflicting Id with all its variants and the deciding rule, and finally the
Id-spelling groups split into Pattern A (resolved), Pattern B (informational),
and Pattern C (with the shape of each group).

### `conventions-review.tsv`

The file a human works through. It holds the Ids that need a human: the flagged
ones, any stale pin, and every Pattern C group. Columns: `kind`, `id`,
`variants`, `winner_signature`, `runner_up_signature`, `why_flagged`. A Pattern
C row lists all the group's Ids in `id`, comma-separated.

## Resolution rule

In order:

1. most non-empty fields (children besides `Id` with non-empty content);
2. then most distinct files;
3. then the non-legacy path (`Legacy/`, `ORE-API/`, `ORE-Python/`,
   `ScriptedTrade/`, `MinimalSetup/`);
4. then lexicographic order of the normalised signature.

An Id is flagged for human review when the decision rests on rule 3 or rule 4,
or when the rule-1 winner (most non-empty fields) is not the plurality winner
(most files). A present-but-empty field does not count as stated. This stops a
one-file outlier from beating a well-supported definition.

The `signature` is a normalised definition: whitespace-collapsed text, fields
sorted by tag name, and a present-but-empty field kept as `Tag=`. Values are
never rewritten, and no Id is merged or renamed except by Pattern A.

## Id-spelling patterns

Ids in one kind that share one identical definition are grouped and classified
by the shape of the difference.

**Pattern A** requires every member to strip to a single common stem, with or
without a trailing `-CONVENTIONS`. The `-CONVENTIONS` form is canonical. The
alias's definitions are folded into the canonical Id before the rule runs, so
a conflicting old spelling still reaches the rule instead of being lost with
the rename. A group that strips to more than one stem is not a clean rename and
does not qualify: it becomes Pattern C.

**Pattern B** is `PREFIX:STEM` where only `PREFIX` differs. `ICE:ANO` and
`PLATTS:ANO` are different feeds, so both stay canonical.

**Pattern C** is everything else. The group is written to the review file with
its shape (the common spelling with `<X>` at each varying token) and the Ids.
The tool never merges a C group.

## How to add a decision

Add one row to `decisions.tsv`:

```
kind<TAB>id<TAB>decision<TAB>signature
```

- `kind` and `id` identify the entry.
- `decision` is the stable key: the 10-character signature hash printed in the
  review file and the audit. A full signature is also accepted.
- `signature` is a human-readable copy of the chosen definition. It is written
  next to the hash so a reader can see what the pin means.
- `TODO` in `decision` means "not yet decided". The tool then falls back to the
  rule.

A decision wins over the rule. It is reported as `pinned`. If the pinned
signature no longer matches any corpus variant, the tool reports it as
**STALE** on stdout and in the audit file, and it keeps the pin. A human must
then choose a replacement or withdraw the pin. A re-extraction never discards a
human decision.

A decision made against a Pattern A alias Id is moved onto the canonical Id, so
a pin on `EUR-6M-FRA` lands on `EUR-6M-FRA-CONVENTIONS` and is not reported
stale. If two decisions collapse onto one canonical Id, they must agree or the
run fails.

Keep the file sorted by `kind` then `id`. The header is fixed.

## Working through `conventions-review.tsv`

A row for one `kind` / `id` is a definition conflict: read the variants in
`conventions-resolutions.md`, pick the correct variant, put its signature hash
in `decisions.tsv`, and rerun the tool. A clean pin drops out of the review
file.

A row whose `why_flagged` starts with `pattern-C` is an Id-spelling group the
tool will not merge. Its `id` column lists every Id in the group. A human must
decide whether the group is a rename or several instruments that share a
definition. `decisions.tsv` cannot express that decision; the human records it
in the corpus or extends the tool.

## Determinism

The same corpus and the same `decisions.tsv` give byte-identical output. The
tool sorts all collections and writes UTF-8 with LF line endings. Check it by
running twice with two `--out` directories and comparing all four files.

## What comes next

The downstream step is TSV to DQ populate SQL. That generator is
`generate_dq_seed.py` (below).

## TSV to DQ seed SQL

`generate_dq_seed.py` turns `conventions-canonical.tsv` into the one
`ore.conventions` DQ dataset that ACME provisioning publishes:

```
./projects/ores.codegen/venv/bin/python tools/ore_conventions/generate_dq_seed.py
```

Options:

- `--tsv FILE` — the canonical TSV. Default
  `tmp/ore_conventions/conventions-canonical.tsv`, where `extract.py` writes it.
- `--out FILE` — the SQL output. Default
  `projects/ores.sql/populate/refdata/refdata_conventions_seed_populate.sql`.

It writes one artefact row per canonical entry, into the artefact table of the
entry's kind. The script holds the per-kind map from the ORE XML field name in
the `signature` to the live table's column, plus the value normalisers. Both
mirror `projects/ores.ore/core/src/domain/conventions_mapper.cpp`, the
authority for how an ORE convention reaches the store. A value the mapper
would not accept makes the script fail loudly; it never writes a row it cannot
map.

Run it after `extract.py`, and check the regenerated SQL in with the tool.

### The kinds

One artefact table per convention kind, generated by the entity model's
`:ores.sql.schema.domain_entity_artefact_create.enabled: true:` flag. The
publish function resolves the artefact table to the live table by name
transform, `ores_dq_<x>_conventions_artefact_tbl` to
`ores_refdata_<x>_conventions_tbl`, so the seed only has to name the kinds.

Two kinds in the corpus are not seeded:

- **`FX`** — FX conventions are world data, not party data. They are already
  carried by the `refdata.currency_pair_conventions` dataset and have no
  party-scoped FX convention table to publish into.
- **`Tenor`** — the corpus has no `Tenor` kind; `ores_refdata_tenor_conventions_tbl`
  is world data with its own seed.

### `oresmd_uri`

Every convention table carries a nullable `oresmd_uri` column, and the artefact
tables mirror it. The seed leaves it null on every row. There is no offline
tool that turns an ORE index name (`EUR-EURIBOR-6M`, the value the corpus
holds) into an oresmd URI. The two codecs that can do it,
`ore_index_codec::read` then `oresmd_uri_codec::write`, live in
`projects/ores.marketdata/core` and have no command-line entry point. Building
one is a task of its own. See `doc/knowledge/domain/market_data_urn.org` for
the format. The column is left explicit in the seed so the gap is visible.

### Publishing

The dataset rides in the `party_essentials` bundle at `display_order` 5, before
`ore.report_definitions` (10), because a run resolves a convention by id and
the report definitions run in the same bundle. Dependency rows in
`reporting_dataset_dependency_populate.sql` also order it before both reporting
datasets. The publish function is
`ores_refdata_publish_conventions_from_dq_fn`.

