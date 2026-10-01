#+title: Pricing engine corpus census

The measurements this task rests its design on. Taken on the corpus at commit
=8f8789e0a3=, which this branch does not modify: the file set, the element
counts and the separator survey below depend on =external/ore/= alone.

* How many files, and what shape they are

#+begin_src sh
find external/ore -iname 'pricingengine*.xml' | wc -l
#+end_src

=125= files, matching the census count already recorded in
[[file:round_trip_corpus.md][round_trip_corpus.md]]. All 125 parse, all have a
=PricingEngines= root, and every one names its file with the lowercase stem
=pricingengine= — there is no =PricingEngine.xml= variant for the kind's prefix
match to miss.

The document shape is uniform across the corpus. Every =Product= element carries
exactly the four children the schema names, in some order:

#+begin_src
PricingEngines: ['Product']                          x70
PricingEngines: ['GlobalParameters', 'Product']      x55
Product: ['Engine', 'EngineParameters', 'Model', 'ModelParameters']  x2131
#+end_src

=2131= products in total, between 1 and 82 per file. No element outside the
schema appears anywhere, so the mapping has no unnamed case to survive.

* Global parameters

=55= files carry a =GlobalParameters= element. Every one of them holds at least
one =Parameter=; none is written empty.

#+begin_src
files with GlobalParameters: 55
with zero Parameter children: 0
#+end_src

This is why the mapper may treat "the document wrote =GlobalParameters=" and "a
global-scope row exists" as the same statement. Had any file written an empty
one, the presence would have needed a column of its own, because no row would
have carried it.

* Why order is stored rather than recovered

Two fields that look like they could identify a row do not.

#+begin_src
files with duplicate Product type: 34
duplicate parameter names within one scope: 24
#+end_src

=34= of the 125 files write the same =Product/@type= more than once. The
repeated types are ordinary ones — =CMS_DEACTIVATED=, =CommodityForward=,
=FxOption= — not a malformed file. =24= files write the same =Parameter/@name=
twice within one scope; =ScriptedTrade= repeats =Interactive= under
=EngineParameters=, and =CBO= repeats =LossDistributionPeriods=.

Neither the engine type nor the parameter name therefore identifies its row, and
neither can be sorted back into the order the document wrote. The =
position= column on =pricing_model_product= and on =
pricing_model_product_parameter= carries that order instead. This follows
=ores.reporting.configuration_parameter=, whose model states the same reason for
the same column.

* Separators inside values

The parameters hold free text, and some of it contains the punctuation an
encoding scheme would reach for as a separator:

| Separator | Values containing it | Example                                              |
|-----------+----------------------+------------------------------------------------------|
| =,=       | 255                  | =GridCoarsening=: =3M(1W),1Y(1M),5Y(3M),10Y(1Y),50Y(5Y)= |
| =|=       | 14                   | =Rule_0=: =(...|_AA$|_A$...),_AAA=                    |
| newline   | 8                    | =BucketTimes=: a comma-separated tenor list on two lines |
| tab       | 8                    | the same =BucketTimes= values                        |

This is the reason the mapper stores one row per =Parameter= with the name and
the value in columns of their own. Nothing is joined into a composite string, so
no value has to be escaped and no separator can collide with one. It also means
the =BucketTimes= values keep their newlines, which a scheme that normalised
whitespace would have destroyed.

* Reproducing

The census scripts are throwaway and live outside the tree in the session
scratch directory; each prints the tables quoted above. The file counts are
reproducible with the =find= command at the top of this page, and the shape
tables with any XML reader over the same glob.
