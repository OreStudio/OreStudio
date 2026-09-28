# What the conventions kind is missing

Measured through the harness's kind walk and two hidden probes, at `8215ab6253`:

```
build/output/linux-clang-debug-make/publish/bin/ores.ore.core.tests "[.][conventions]" -s
```

The document carries **twenty-six** convention categories. `conventions_mapper`
models **nine**: Zero, Deposit, Swap, OIS, FRA, IborIndex, OvernightIndex, FX and
CDS. It used to skip the rest without saying so, which is why seventy-two files
passed a round-trip test that only compared element counts. It now counts what
it skips into `mapped_conventions::unmodelled` and logs a warning per category.

## The categories the corpus uses and the mapper does not model

Fourteen of the seventeen unmodelled categories appear in the corpus.
`TenorBasisTwoSwap`, `FxOptionTimeWeighting` and `ZeroInflationIndex` do not.

| Category | Files | Elements |
|----------|-------|----------|
| CrossCurrencyBasis | 49 | 522 |
| SwapIndex | 47 | 678 |
| Future | 39 | 102 |
| AverageOIS | 36 | 43 |
| FxOption | 36 | 420 |
| TenorBasisSwap | 29 | 142 |
| InflationSwap | 22 | 101 |
| CrossCurrencyFixFloat | 18 | 41 |
| BMABasisSwap | 16 | 20 |
| CmsSpreadOption | 12 | 12 |
| CommodityFuture | 5 | 808 |
| CommodityForward | 2 | 5 |
| BondYield | 1 | 5 |
| IntradayPowerLoad | 1 | 2 |

Sixty-three of the seventy-two files carry at least one of them.

## What the nine files that use only modelled categories fail on

Nine files carry no unmodelled category, and none of them round trips either.
That is the half a field mapping can fix, and its causes are two.

**A dropped element.** `<IndexBased>` is in the document and not in the export.
`deposit_convention` already has the column and the mapper never reads it:
`zero_convention` has no `index_based` field at all, although ORE writes an
index-based zero convention, which is what `EUR-EONIA-CONVENTIONS` is.

```
examples/CurveBuilding/Input/conventions_centralbank.xml:
imported "<Id>GBP-DEPOSIT</Id>
  <IndexBased>true</IndexBased>
  <Index>GBP-SONIA</Index>"
exported "<Id>GBP-DEPOSIT</Id>
  <Index>GBP-SONIA</Index>"
```

**Two normalisations the mapper performs deliberately.** A boolean comes back
with ORE's enum spelling rather than the document's, and a day counter alias
comes back as the canonical code the mapper normalises to.

```
examples/Academy/TA001_Equity_Option/Input/conventions.xml:
imported "<TenorBased>true</TenorBased>
  <DayCounter>A365</DayCounter>"
exported "<TenorBased>True</TenorBased>
  <DayCounter>A365F</DayCounter>"
```

The second is the mapper's stated design: it collapses ORE's enum aliases to
canonical FpML/CDM codes before storing them, because the refdata columns are
soft foreign keys to code tables. `A365` and `A365F` are two spellings of one
code, and `true` and `True` are one boolean, so both carry the same information
and the comparison may state them.

## What this means for the work

Three kinds of change, in order of how much they buy:

1. **The dropped fields on the modelled categories.** `IndexBased` on Deposit is
   a mapper line. On Zero it is a column. These are the cheapest and they are
   the reason nine files fail for a reason unrelated to the unmodelled
   categories.
2. **The two declared normalisations**, stated in the kind's comparison rather
   than fixed, because the mapper normalises on purpose and the information
   survives it.
3. **The fourteen categories**, each of which needs a refdata entity, a mapper
   in both directions, and its own round trip. This is the bulk of the work and
   it is why conventions is not one round's job.

Until all three land, the kind stays measured by a hidden probe rather than
asserted green, because a test that passes while a document loses a category is
worse than a measurement a person reads.
