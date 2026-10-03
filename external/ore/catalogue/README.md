# ORE market datum catalogue

What ORE's own parser, `ore::data::parseMarketDatum`, makes of a market data
key: the datum class, the instrument type, the quote type and every inspector
the class declares, or ORE's error for a key it refuses. The oresmd key codec in
`ores.marketdata` is tested against it field by field.

| File | What it holds |
|------|---------------|
| `forms.jsonl` | One line per key in `tools/datum_catalogue/forms.txt`: every form ORE's parser documents, and keys it refuses |
| `corpus.jsonl.gz` | One line per distinct key in the example corpus under `examples/` |
| `quote_matrix.jsonl` | One line per accepted form in `forms.jsonl` with each quote token in turn: most of ORE's parser cases never check the quote type, so this records which quote types each form admits |
| `index_forms.jsonl` | What ORE's `parseIndex` makes of every index form `tools/datum_catalogue/index_forms.txt` lists, and of names it refuses: the index class and its family's inspectors |
| `index_corpus.jsonl` | The same for every distinct index name the corpus's fixing files carry |
| `instrument_types.txt` | ORE's `InstrumentType` enum, each member with the key tokens its parser reads as it |
| `quote_types.txt` | ORE's `QuoteType` enum, each member with the key tokens its parser reads as it; `HAZARD_RATE` has none, and `NULL` names `NONE` |
| `ore_version.txt` | The ORE tag and commit the catalogue came from, and the gzip that compressed it |

The index catalogue loads `examples/Products/Input/conventions.xml` first,
because ORE reads an index a conventions file defines, such as `CZK-CZEONIA`,
only once the convention is loaded. Its evaluation date is QuantLib's earliest,
so a name ORE completes from that date, such as a power index with no delivery
date, prints the same on every run.

Every value is a string. The as-of date is QuantLib's earliest date, so a key
ORE resolves to a date relative to it (an equity forward or dividend given as a
tenor) carries a date in 1901. `BOND_FUTURE_OPTION` prints its instrument type
as `?`, because ORE's enum printer has no entry for it.

These files are generated. From the same ORE commit the `.jsonl` content
regenerates byte for byte; the compressed bytes do so only with the same gzip. To rebuild them from a local ORE build, see the
recipe "How do I rebuild the ORE market datum catalogue?" under
`doc/recipes/ore/`, or run `tools/datum_catalogue/regenerate.sh`.
