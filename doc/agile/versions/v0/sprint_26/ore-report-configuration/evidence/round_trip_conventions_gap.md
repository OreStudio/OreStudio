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

Sixteen of the seventeen unmodelled categories appear in the corpus; only
`FxOptionTimeWeighting` does not.

**Correction.** An earlier version of this sentence named `TenorBasisTwoSwap`
and `ZeroInflationIndex` as absent. Both appear, in forty-six and six files, and
the table below has always shown them.

| Category | Files | Elements |
|----------|-------|----------|
| CrossCurrencyBasis | 49 | 522 |
| SwapIndex | 47 | 678 |
| TenorBasisTwoSwap | 46 | 65 |
| Future | 39 | 102 |
| AverageOIS | 36 | 43 |
| FxOption | 36 | 420 |
| TenorBasisSwap | 29 | 142 |
| InflationSwap | 22 | 101 |
| CrossCurrencyFixFloat | 18 | 41 |
| BMABasisSwap | 16 | 20 |
| CmsSpreadOption | 12 | 12 |
| ZeroInflationIndex | 6 | 54 |
| CommodityFuture | 5 | 808 |
| CommodityForward | 2 | 5 |
| BondYield | 1 | 5 |
| IntradayPowerLoad | 1 | 2 |

**Correction.** An earlier version of this table had fourteen rows and said that
`TenorBasisTwoSwap` and `ZeroInflationIndex` do not appear in the corpus. Both
do, in forty-six and six files. The fourteen rows were the whole list only
because the command that produced them ended in `head -40`, and the measurement
prints two lines per category, so the last two were cut off. A truncated list
read as a complete one, which is the third time in this work that a formatted
output has been trusted over the artefact.

Seventeen categories were unmodelled between them. Sixteen appear in the corpus;
only `FxOptionTimeWeighting` does not.

Sixty-three of the seventy-two files carry at least one of them, and the list
above is as it stood before any category was modelled. Ten of the sixteen have
since landed -- SwapIndex, Future, FxOption, AverageOIS, CrossCurrencyBasis,
TenorBasisTwoSwap, TenorBasisSwap, ZeroInflationIndex, BMABasisSwap and
InflationSwap -- so the live numbers are on the task. At `3c775b17d9` the mapper
models nineteen of the twenty-six categories, six remain unmodelled,
twenty-two files still carry one, and fifty round trip outright.

## What the nine files that use only modelled categories fail on

Nine files carry no unmodelled category, and none of them round tripped under a
plain text comparison. Nothing is dropped on this half: every difference is a
value the mapper writes in its own canonical spelling.

**Correction.** An earlier version of this page said the export dropped
`<IndexBased>`. It does not. The comparison message quotes a window that starts
forty bytes before the difference, so the element printed at the top of the
imported side is context, not the difference. Dumping the export settles it: the
element is there, written as `True` where the document wrote `true`. Nothing is
dropped on this half, and the correction matters because the fix it implied was
a mapper line and a new column, and neither is needed.

**Two normalisations the mapper performs deliberately.** A boolean comes back
with ORE's enum spelling rather than the document's, and a day counter alias
comes back as the canonical code the mapper normalises to.

```
examples/CurveBuilding/Input/conventions_centralbank.xml:
imported "<IndexBased>true</IndexBased>"
exported "<IndexBased>True</IndexBased>"

examples/Academy/TA001_Equity_Option/Input/conventions.xml:
imported "<TenorBased>true</TenorBased>
  <DayCounter>A365</DayCounter>"
exported "<TenorBased>True</TenorBased>
  <DayCounter>A365F</DayCounter>"
```

This is the mapper's stated design: it collapses ORE's boolean spellings and its
enum aliases to the canonical codes the refdata columns hold, because those
columns are soft foreign keys to code tables. `A365` and `A365F` are two
spellings of one code, and `true` and `True` are one boolean, so both carry the
same information and the comparison may state them.

Conventions are therefore the case where a plain text comparison cannot work at
all, and the kind's comparison (`conventions_diff.cpp`) answers it in two parts:
no element may appear fewer times in the export than in the document, and the
same mapper must read the same conventions out of both documents. The first
catches a category the mapper reads and does not model; the second tolerates the
canonicalisation while still catching a value the mapper got wrong, because a
value the mapper got wrong survives neither direction.

With that in place the files that carry only modelled categories round trip,
and they are asserted green rather than measured. The count was nine when this
was written and is fifty once the tenth category landed; the per-category case
asserts each one as it lands, and the whole-kind case asserts the count.

## What this means for the work

Two kinds of change remain, in order of how much they buy:

1. **The sixteen categories**, each of which needs a refdata entity, a mapper in
   both directions, and its own round trip. This is the bulk of the work and it
   is why conventions is not one round's job.
2. **The mapper's report of what it skips**, already delivered: it counts and
   warns, and the task's acceptance asks for a test that asserts the count is
   zero, which cannot pass until the categories land.

Until the categories land the kind stays measured by a hidden probe rather than
asserted green, because a test that passes while a document loses a category is
worse than a measurement a person reads. The files that do not lose one are
asserted, and the count of them is asserted with them so that it cannot grow
quietly.
