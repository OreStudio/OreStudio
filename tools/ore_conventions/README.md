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
tables mirror it. The generator writes the oresmd **fixing** URI of the ORE index
the row names, read from `oresmd_index_map.tsv` (below). A row whose kind names
no index (a *requirement*, per the model doc string) carries `null`.

The URI comes from the codecs, never from a rule typed into the generator: an
ORE index name becomes a URI only through `ore_index_codec::read` then
`oresmd_uri_codec::write_index`, both in `projects/ores.marketdata/core`. The
product owner rejected a Python binding to the C++ codec, so the mapping is a
fixed table held in this tool. It is not hand-typed: it is generated once from
the codec, committed as data, and round-tripped by a marketdata test, so it
cannot silently drift. See
`doc/knowledge/domain/market_data_urn.org` for the URI format.

A name the map does not hold **fails generation and is named**. The generator
never writes a null for a name it looked up and never invents a URI.

### `oresmd_index_map.tsv`

`ore_name<TAB>oresmd_uri`, one row per ORE index name the canonical set
references, sorted by name, UTF-8 with LF. There is no comment header: the file
is parsed by both this tool and a C++ test, so a header would be data to one of
them. This README is its documentation.

The canonical set is every value of an index-name field the kind map writes
(`Index`, `BMAIndex`, `FlatIndex`, `SpreadIndex`, `IndexName`, `PayIndex`,
`ReceiveIndex`, `LongIndex`, `ShortIndex`) plus the id of every kind whose id
*is* the index name (`IborIndex`, `OvernightIndex`, `ZeroInflationIndex`,
`SwapIndex`). That is 622 names in the current corpus, and every one resolves.

Two kinds of value are deliberately excluded, and neither is an index name:

- A field that mentions "index" but holds a parameter: `IndexBased`,
  `IndexPaymentLag`, `IndexSettlementDays`, `IndexPaymentPeriod`,
  `FlatIndexIsResettable`, `OvernightIndexTenor`,
  `OvernightIndexFutureNettingType`. A naive scan for fields named like
  "index" picks these up, and their values (`true`, `0`, `2`, `1M`) are not
  index names.
- The peak and off-peak power indices nested in `OffPeakPowerIndexData`
  (`ICE:UNP`, `ICE:DPN`, ...). A power index needs a delivery date the
  convention does not state, so the codec would read the bare name as an
  inflation index; writing that URI would invent an address. They are written to
  the `off_peak_index` and `peak_index` columns as the corpus spells them, with
  no URI.

#### How it is generated, and how to regenerate it when ORE adds an index

The codec is the only thing that turns a name into a URI, and it has one
command-line entry point: the shell's offline `marketdata oresmd-index` verb.
The verb is pure — no NATS connection, no session, no database — and its only
job is the chain `ore_index_codec::read` then `oresmd_uri_codec::write_index`.
It reads names one per line from `--in` (or stdin) and writes
`ore_name<TAB>oresmd_uri` to `--out` (or stdout), sorted and de-duplicated. A
name the codec rejects makes the command fail, names the name and its line
number, and writes no output.

1. `extract.py` writes `tmp/ore_conventions/conventions-canonical.tsv`.
2. `generate_dq_seed.py --dump-index-names tmp/ore_conventions/index_names_to_resolve.txt`
   writes the sorted canonical index set the seeded rows reference.
3. `./compass.sh shell -f tools/ore_conventions/generate_oresmd_index_map.ores`
   runs `marketdata oresmd-index --in … --out oresmd_index_map.tsv`.
4. Regenerate the seed SQL and run the guard test.

The whole shell run exits non-zero when the verb rejects a name, so a stale or
hand-edited input cannot slip an empty URI into the table. The verb never writes
an empty URI and never invents one.

The guard test runs the same codecs over every committed row, so a codec change
that moved a URI fails the suite instead of the seed.

### Publishing

The dataset rides in the `party_essentials` bundle at `display_order` 5, before
`ore.report_definitions` (10), because a run resolves a convention by id and
the report definitions run in the same bundle. Dependency rows in
`reporting_dataset_dependency_populate.sql` also order it before both reporting
datasets. The publish function is
`ores_refdata_publish_conventions_from_dq_fn`.

