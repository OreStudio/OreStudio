#+title: Today's market corpus census

What the 98 shipped =todaysmarket*.xml= documents actually contain, measured at
commit =79eefb38ad= before any entity or mapper exists for the kind.

* The population

#+begin_src sh
find external/ore -iname 'todaysmarket*.xml' | wc -l
#+end_src

=98= files under sixteen filename spellings, of which =todaysmarket.xml= is one.
Unlike the SIMM kind, a single stem prefix =todaysmarket= selects all 98 and
nothing else, so =files_of_kind("todaysmarket", ...)= is enough.

Every file parses, and every one has a =TodaysMarket= root. There is no stray
for the kind to filter.

* Twenty-four collections

The document is a set of named top-level collections, each optional. Counted by
how many files carry each:

| Collection | Files | Collection | Files |
|------------+-------+------------+-------|
| =DiscountingCurves= | 98 | =ZeroInflationIndexCurves= | 32 |
| =IndexForwardingCurves= | 98 | =YYInflationIndexCurves= | 31 |
| =FxSpots= | 89 | =ZeroInflationCapFloorVolatilities= | 28 |
| =Configuration= | 88 | =CommodityCurves= | 24 |
| =YieldCurves= | 76 | =CommodityVolatilities= | 23 |
| =SwapIndexCurves= | 63 | =YYInflationCapFloorVolatilities= | 22 |
| =DefaultCurves= | 58 | =BaseCorrelations= | 18 |
| =SwaptionVolatilities= | 54 | =CDSVolatilities= | 18 |
| =FxVolatilities= | 51 | =Correlations= | 10 |
| =EquityCurves= | 36 | =YieldVolatilities= | 3 |
| =CapFloorVolatilities= | 35 | =IntradayPowerPriceCurves= | 1 |
| =EquityVolatilities= | 33 | | |
| =Securities= | 33 | | |

Only two collections are in every file: =DiscountingCurves= and
=IndexForwardingCurves=. Six are in ten files or fewer, and
=IntradayPowerPriceCurves= is in exactly one. A mapper that handles the common
collections and skips the rare ones would pass on most of the corpus and lose
data on the rest, so the rare ones are where the fidelity work is, not the
common ones.

* The Configuration block is a bundle of references

=Configuration= appears 206 times across 88 files, between 0 and 6 per file. It
is keyed by a *required* =id= attribute — not =name=, which is worth stating
because an early pass of this census read =name= and reported 63 files with
duplicate keys. That was the census being wrong: read the right attribute and
the count is 0, so the id is unique within a file.

Each block holds up to 23 fields, every one a reference by name into one of the
other collections:

#+begin_src xml
<Configuration id="default">
  <DiscountingCurvesId>xois_eur</DiscountingCurvesId>
  <YieldCurvesId>xois_eur</YieldCurvesId>
  <IndexForwardingCurvesId>default</IndexForwardingCurvesId>
</Configuration>
#+end_src

So a configuration names one entry in each collection; it does not hold curve
data itself. =DiscountingCurvesId= appears in all 206 blocks, =YieldCurvesId= in
146, and the rarest, =BondFutureVolatilitiesId= and
=IntradayPowerPriceCurvesId=, once each.

* The collections are mostly attribute-and-text pairs

The common shape is an entry with one identifying attribute and a target as its
text, which is close to the pricing engine parameter shape:

#+begin_src xml
<DiscountingCurves>
  <DiscountingCurve currency="EUR">Yield/EUR/EUR1D</DiscountingCurve>
</DiscountingCurves>
<IndexForwardingCurves>
  <Index name="EUR-EURIBOR-3M">Yield/EUR/EUR3M</Index>
</IndexForwardingCurves>
#+end_src

The attribute is not always the same word — =currency= here, =name= there — and
the collection that is a list is not always a list of the same thing.

*Exactly one collection nests.* Measured over the corpus, every collection has
depth 1 except =SwapIndexCurves=, whose =SwapIndex= entries hold a
=Discounting= child:

#+begin_src xml
<SwapIndexCurves>
  <SwapIndex name="EUR-CMS-1Y">
    <Discounting>EUR-EONIA</Discounting>
  </SwapIndex>
</SwapIndexCurves>
#+end_src

That is depth 2 and nothing deeper. It is not a rare corner either: 51 of the 98
files nest this way, so it is the majority case for that collection and cannot
be deferred.

A note for whoever reads the schema rather than the corpus: =curveconfig.xsd=
does declare deeply nested volatility configurations with quotes, interpolation
and extrapolation, and it is easy to carry that shape across to this kind. It
does not belong here. Today's market *references* volatility configurations by
name; it does not contain them. Reading the schema for today's market and
assuming the nesting carries over is the mistake this census was written to
prevent, and the first pass of this very file made it.

* Separation

No =Configuration= field text contains =;=, =|=, =~= or a newline, so the
reference fields are safe to store as they are. The separator question has not
been asked of the collection entry text yet, and it has to be: the same probe
against the pricing engine parameters found 255 values holding a comma and 14
holding a pipe.

* The design question this census leaves open

Whether the 24 collections become 24 tables, one table with a collection
discriminator, or a generic entry table with the nested family broken out.

Twenty-three of the twenty-four collections are =entry(collection, key, target,
position)=: one identifying attribute, a reference as the text, and an order.
One table with a collection discriminator holds all of them, and that is the
same shape the pricing engine parameters already use.

=SwapIndexCurves= does not fit it, because its entries carry a second value. It
is one extra column on the entry table or one table of its own, and the choice
is small enough to make when the model is written rather than here.

The corpus also does not say whether the identifying attribute name must
survive. =currency= and =name= both appear, and they may be the same column
under two spellings or two different concepts. Nothing in the corpus
distinguishes them, so the mapper has to either keep the attribute name or the
model has to state which collections use which; silently normalising them would
round trip and would lose the distinction if one exists.
