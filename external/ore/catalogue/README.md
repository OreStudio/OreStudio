# ORE market datum catalogue

What ORE's own parser, `ore::data::parseMarketDatum`, makes of a market data
key: the datum class, the instrument type, the quote type and every inspector
the class declares, or ORE's error for a key it refuses. The oresmd key codec in
`ores.marketdata` is tested against it field by field.

| File | What it holds |
|------|---------------|
| `forms.jsonl` | One line per key in `tools/datum_catalogue/forms.txt`: every form ORE's parser documents, and keys it refuses |
| `corpus.jsonl.gz` | One line per distinct key in the example corpus under `examples/` |
| `instrument_types.txt` | ORE's `InstrumentType` enum, each member with the key tokens its parser reads as it |
| `ore_version.txt` | The ORE tag and commit the catalogue came from, and the gzip that compressed it |

Every value is a string. The as-of date is QuantLib's earliest date, so a key
ORE resolves to a date relative to it (an equity forward or dividend given as a
tenor) carries a date in 1901. `BOND_FUTURE_OPTION` prints its instrument type
as `?`, because ORE's enum printer has no entry for it.

These files are generated. From the same ORE commit the `.jsonl` content
regenerates byte for byte; the compressed bytes do so only with the same gzip. To rebuild them from a local ORE build, see the
recipe "How do I rebuild the ORE market datum catalogue?" under
`doc/recipes/ore/`, or run `tools/datum_catalogue/regenerate.sh`.
