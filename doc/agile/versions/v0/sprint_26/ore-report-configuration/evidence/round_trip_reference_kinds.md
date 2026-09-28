# Round trip status of the shared reference kinds

Measured on the branch that built the round-trip harness, at `77c286dcce`, with
the harness's own probe:

```
build/output/linux-clang-debug-make/publish/bin/ores.ore.core.tests "[.][measurement]"
```

The probe walks each kind over `external/ore/examples` and compares the exported
document with the imported one through that kind's comparison. It is hidden from
the default test run because two of the three kinds do not round trip, and their
failures are losses rather than comparisons that need stating.

| Kind | Glob | Files | Round trip |
|------|------|-------|------------|
| calendar adjustments | `calendaradjustment*.xml` | 5 | 5, registered as a test |
| currencies | `currencies*.xml` | 13 | 12 |
| conventions | `conventions*.xml` | 72 | 0 |

## Calendar adjustments round trip, with one stated normalisation

The kind is registered in `xml_roundtrip_reference_tests.cpp` and passes. Its
comparison declares the one normalisation the mapper makes: ORE writes a date
list it has no dates for as an empty element, and `calendar_adjustment_mapper`
omits it. An empty list is the same list either way, and a list with dates in it
is compared date by date, so the rule cannot hide a dropped date.

Before the comparison was written, the first difference was:

```
examples/Input/calendaradjustment.xml: imported "...</AdditionalHolidays>
  <AdditionalBusinessDays/>
</Calendar>..." exported "...</AdditionalHolidays>
</Calendar>..."
```

## Currencies lose ORE's currency type

Twelve files pass because they set no `CurrencyType`. The thirteenth is the only
one in the corpus that does, and it uses three values:

```
examples/Input/currencies.xml: first difference at byte 346 (line 11):
imported "...<CurrencyType>Major</CurrencyType>..."
exported "...<CurrencyType>fiat</CurrencyType>..."
```

`currency_mapper` translates ORE's type into the refdata `monetary_nature`:

| ORE `CurrencyType` | refdata `monetary_nature` |
|--------------------|---------------------------|
| `Metal` | `commodity` |
| `Crypto` | `synthetic` |
| `Major`, `Minor` | `fiat` |

The translation is not invertible. `Major` and `Minor` both become `fiat`, so
the export can never recover which one the document held. The two axes are also
not the same thing: `monetary_nature` says what a currency is, and ORE's
`CurrencyType` says how important it is, with `Metal` and `Crypto` naming a kind
that neither vocabulary quite captures.

`market_tier` is not a home for it either: it is a soft foreign key to
`ores_refdata_currency_market_tiers_tbl`, whose values are `g10` and `emerging`.
The mapper leaves it empty on import and does not write it on export.

*Fix required.* A column on the refdata currency that carries ORE's currency
type, seeded as a lookup with the values the corpus uses, mapped one to one in
both directions. This is a model change with the usual blast radius, and it
belongs to the reference round-trip task rather than to the harness.

## Conventions lose every type the mapper does not model

None of the 72 files round trips. The first difference is usually a field or a
type the mapper never reads:

```
examples/Academy/FC003_Reporting_Currency/Input/conventions.xml:
imported "<Id>EUR-ZERO-TENOR-BASED</Id>
  <TenorBased>true</TenorBased>..."
exported "..."
```

`conventions_mapper` says so in its own header: it models nine convention
categories and "ORE conventions.xml contains additional types (AverageOIS,
TenorBasisSwap, CrossCurrencyBasis, InflationSwap, etc.) that are not yet
modelled and are silently skipped during import."

*Fix required.* Model the missing convention categories and the missing fields
on the categories that are modelled, then register the kind. The mapper must
stop skipping silently: a type it does not recognise should be counted and
reported, because a silent skip is the reason 72 files passed for as long as
nobody measured them.

## What this means for the plan of record

The story's plan assumed each document kind was one mapper away from a round
trip. For the three shared reference kinds that is false. Calendar adjustments
were one comparison away and are done. Currencies need a column. Conventions
need the categories and fields the mapper skips, which is the largest of the
three and the one most likely to be needed by real documents, since inflation
and cross-currency basis conventions appear in the corpus.

The harness did what it was built for: it turned three "probably fine"
assumptions into three measurements, each with the byte and the line that
disagrees.
