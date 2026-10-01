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

* The mapper's vocabulary, read from the schema

Each collection owns an entry element, and every entry identifies itself by an
attribute. Read out of =todaysmarket.xsd= rather than inferred from the corpus,
because the corpus only shows the attribute each file happened to write:

| Collection | Entry element | Key attribute |
|------------+---------------+---------------|
| =YieldCurves= | =YieldCurve= | =name= |
| =DiscountingCurves= | =DiscountingCurve= | =currency= |
| =IndexForwardingCurves= | =Index= | =name= |
| =SwapIndexCurves= | =SwapIndex= | =name= |
| =ZeroInflationIndexCurves= | =ZeroInflationIndexCurve= | =name= |
| =YYInflationIndexCurves= | =YYInflationIndexCurve= | =name= |
| =FxSpots= | =FxSpot= | =pair= |
| =FxVolatilities= | =FxVolatility= | =pair= |
| =SwaptionVolatilities= | =SwaptionVolatility= | =key=, =currency= |
| =YieldVolatilities= | =YieldVolatility= | =name= |
| =CapFloorVolatilities= | =CapFloorVolatility= | =key=, =currency= |
| =CDSVolatilities= | =CDSVolatility= | =name= |
| =DefaultCurves= | =DefaultCurve= | =name= |
| =YYInflationCapFloorVolatilities= | =YYInflationCapFloorVolatility= | =name= |
| =ZeroInflationCapFloorVolatilities= | =ZeroInflationCapFloorVolatility= | =name= |
| =EquityCurves= | =EquityCurve= | =name= |
| =EquityVolatilities= | =EquityVolatility= | =name= |
| =Securities= | =Security= | =name= |
| =BaseCorrelations= | =BaseCorrelation= | =name= |
| =CommodityCurves= | =CommodityCurve= | =name= |
| =CommodityVolatilities= | =CommodityVolatility= | =name= |
| =Correlations= | =Correlation= | =name= |
| =BondFutureVolatilities= | =BondFutureVolatility= | =name= |
| =IntradayPowerPriceCurves= | =IntradayPowerPriceCurve= | =name= |

Two things fall out of this table that the prose above could not settle.

**The key attribute is four different words, not one.** It is =name= for
nineteen of the twenty-four collections, =currency= for =DiscountingCurves=,
=pair= for the two FX collections, and =key= — the literal string =key= — for
=SwaptionVolatilities= and =CapFloorVolatilities=. So a single =entry_key=
column would be storing a different attribute name depending on the row, and the
column name would collide with one of the values it stores. Either the entry
table carries the attribute name beside the value, or it carries the four
attributes as four columns. The plan's lookup does the former.

**=SwaptionVolatilities= and =CapFloorVolatilities= identify by two attributes at
once.** Every other entry has exactly one key. Those two carry =key= and
=currency= together, so a row keyed on one attribute alone would collapse two
distinct entries into one on any document that used both.

**The =id= in the table above belongs to the collection, not to the entry.**
The vocabulary extraction walked the collection type and so reported the
wrapper's own =id= attribute alongside each entry's key. Reading the generated
entry structs shows entries declare no =id= at all:

#+begin_src cpp
struct discountCurvesType_DiscountingCurve_t : xsd::string {
    xsd::string currency{};
};
#+end_src

So the earlier claim on this page that "every entry type also declares an
optional =id=" was wrong, and the =todays_market_entry.entry_id= column it
justified was dead. The column is removed. The collection's =id= is real and is
now =todays_market_collection.collection_id= — one attribute, in one place, and
the first pass of this census put it in the wrong one.

**The entry attributes are not all strings, which is what the mapper has to
absorb.** Three shapes appear:

#+begin_src cpp
struct indexForwardingCurvesType_Index_t : xsd::string {
    domain::indexNameType name{};              // a typedef for xsd::string
};
struct fxSpotsType_FxSpot_t : xsd::string {
    domain::currencyPair pair{};               // also a typedef
};
struct swaptionVolatilitiesType_SwaptionVolatility_t : xsd::string {
    xsd::optional<xsd::string> key;            // optional
    xsd::optional<domain::currencyCode> currency;  // optional, and an enum
};
#+end_src

=currencyPair= and =indexNameType= are =typedef=s for =xsd::string=, so those
copy straight across. =currencyCode= is an enum, so it needs a conversion rather
than an assignment, and both members of the two-key entry are optional, so
either may be absent.

**And one entry does not derive from the reference text at all.**

#+begin_src cpp
struct swapIndexCurvesType_SwapIndex_t {
    domain::indexNameType name{};
    domain::indexNameType Discounting{};
};
#+end_src

=SwapIndex= has no base string: its key is =name= and its child is
=Discounting=, and there is no reference text. So the mapper cannot treat "the
base string is the target" as a rule for all twenty-four; =SwapIndex= is the
exception and the only one.

* Every entry shape, read from the binding

The complete set, so the mapper is mechanical rather than exploratory:

| Collection | Entry member | Shape |
|------------+--------------+-------|
| =YieldCurves= | =YieldCurve= | =xsd::string name= |
| =DiscountingCurves= | =DiscountingCurve= | =xsd::string currency= |
| =IndexForwardingCurves= | =Index= | =indexNameType name= |
| =SwapIndexCurves= | =SwapIndex= | =name= + =Discounting=, no base |
| =ZeroInflationIndexCurves= | =ZeroInflationIndexCurve= | =xsd::string name= |
| =YYInflationIndexCurves= | =YYInflationIndexCurve= | =xsd::string name= |
| =FxSpots= | =FxSpot= | =currencyPair pair= |
| =FxVolatilities= | =FxVolatility= | =currencyPair pair= |
| =SwaptionVolatilities= | =SwaptionVolatility= | optional =key= + optional =currencyCode= |
| =YieldVolatilities= | =YieldVolatility= | =xsd::string name= |
| =CapFloorVolatilities= | =CapFloorVolatility= | optional =key= + optional =currencyCode= |
| =CDSVolatilities= | =CDSVolatility= | =xsd::string name= |
| =DefaultCurves= | =DefaultCurve= | =xsd::string name= |
| =YYInflationCapFloorVolatilities= | =YYInflationCapFloorVolatility= | =xsd::string name= |
| =ZeroInflationCapFloorVolatilities= | =ZeroInflationCapFloorVolatility= | =xsd::string name= |
| =EquityCurves= | =EquityCurve= | =xsd::string name= |
| =EquityVolatilities= | =EquityVolatility= | =xsd::string name= |
| =Securities= | =Security= | =xsd::string name= |
| =BaseCorrelations= | =BaseCorrelation= | =xsd::string name= |
| =CommodityCurves= | =CommodityCurve= | =xsd::string name= |
| =CommodityVolatilities= | =CommodityVolatility= | =xsd::string name= |
| =Correlations= | =Correlation= | =xsd::string name= |
| =BondFutureVolatilities= | =BondFutureVolatility= | =xsd::string name= |
| =IntradayPowerPriceCurves= | =IntradayPowerPriceCurve= | =xsd::string name= |

=indexNameType= and =currencyPair= are =typedef=s for =xsd::string=, so nineteen
entries copy a plain =name= straight across and four copy their own attribute
name. =currencyCode= is an enum, so the two volatility collections need a
conversion rather than an assignment. And one entry has no base string. Four
cases for the mapper, not twenty-four.

Reproduce it by dumping each entry struct, and note the anchoring:

#+begin_src sh
H=projects/ores.ore/core/include/ores.ore.core/domain/domain.hpp
awk "/^struct discountCurvesType_DiscountingCurve_t /,/^};/" $H
#+end_src

The trailing space matters. Without it the pattern also matches the forward
declaration =struct X;= and walks forward into an unrelated struct's body — the
first pass of this extraction reported all twenty-four entries as holding
=std::vector<domain::trade>=, which is a struct belonging to a different schema
entirely.



* How the binding represents this, which is what the mapper encodes

The generated binding is regular enough that the mapper can be written against
one pattern rather than twenty-four.

=todaysmarket= holds one vector per collection:

#+begin_src cpp
struct todaysmarket {
    xsd::vector<domain::configurationType> Configuration;
    xsd::vector<domain::discountCurvesType> DiscountingCurves;
    xsd::vector<domain::indexForwardingCurvesType> IndexForwardingCurves;
    // ... one member per collection, twenty-four of them
};
#+end_src

A collection is a wrapper holding the entries. It also declares its own optional
=id=, which the corpus does not write and the mapper must still carry:

#+begin_src cpp
struct discountCurvesType {
    xsd::optional<xsd::string> id;
    xsd::vector<domain::discountCurvesType_DiscountingCurve_t> DiscountingCurve;
};
#+end_src

An entry is named =<collectionType>_<EntryElement>_t=, derives from =xsd::string=
— which is where the reference text lives — and declares one member per key
attribute:

#+begin_src cpp
struct discountCurvesType_DiscountingCurve_t : xsd::string {
    xsd::string currency{};
};
#+end_src

That is the whole shape. =DiscountingCurve= takes its key from =currency= and its
reference from the base; =Index= does the same from =name=; =SwaptionVolatility=
declares both =key= and =currency=, which is the two-key case; and
=swapIndexCurvesType_SwapIndex_t= is the only one whose entry has a child.

So the mapper is twenty-four small cases over one pattern, not twenty-four
shapes. That part is mechanical.

**But reading the binding found a gap the model does not yet cover.** Every
collection *wrapper* declares its own optional =id= — =discountCurvesType::id=,
=indexForwardingCurvesType::id= and so on — separate from the =id= that each
*entry* declares. The entry's id has a column, =todays_market_entry.entry_id=.
The collection's does not, because a collection is a discriminator on the entry
table rather than a table of its own, so there is nowhere to put it.

No corpus file writes it, so nothing fails today. A document that did would lose
it, and the loss would be invisible because the corpus cannot show it — the same
trap =ParConversion= set for the stress mapper. Either the entry table carries
=collection_id= as a denormalised copy, or the collection earns a table of its
own holding =collection=, its =id= and its position. That decision belongs in
the task's Plan and has not been made.



