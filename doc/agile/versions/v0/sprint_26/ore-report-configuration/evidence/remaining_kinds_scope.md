#+title: What remains, and what it costs

A measured scope for the configuration kinds the story still owes, so the next
unit is chosen on evidence rather than on which task happens to be listed first.
Measured on the vendored corpus at commit =e6f80a03bd=.

* The measurement

Three columns decide the cost, and the third one changed the recommendation.

- *Corpus files* says whether there is anything to prove against.
- *Complex types* in the XSD is the proxy for modelling effort: each one is a
  shape the entity model has to hold.
- *Bound* says whether the kind can be parsed at all. =ores.ore.core= holds a
  generated ORE binding, and a schema that was never fed to it has no type, no
  loader and no saver — so no mapper can be written for it until that is fixed,
  whatever the schema size says.

| Kind                | Corpus files | XSD lines | Complex types | Bound |
|---------------------+--------------+-----------+---------------+-------|
| historical return   | 0            | 28        | 2             | no    |
| Basel traffic light | 0            | 24        | 3             | no    |
| today's market      | 98           | 429       | 50            | yes   |
| sensitivity         | 50           | 671       | 66            | no    |
| SIMM calibration    | 4            | 743       | 80            | no    |
| curve configuration | 89           | 1486      | 126           | no    |
| simulation          | 172          | 1777      | 187           | yes   |
| pricing engines     | 125          | 30        | 4             | yes   |
| stress test         | 18           | –         | –             | yes   |
| credit simulation   | 14           | –         | –             | yes   |
| run document        | 416          | –         | –             | yes   |

Reproduce the first three columns with:

#+begin_src sh
for f in historicalreturnconfig baselTrafficLightconfig todaysmarket \
         sensitivity simmcalibration curveconfig simulation; do
    printf "%-24s %3s files  %4s lines  %3s types\n" "$f" \
        "$(find external/ore -iname "$f*.xml" | wc -l)" \
        "$(wc -l < external/ore/xsd/$f.xsd)" \
        "$(grep -c '<xs:complexType' external/ore/xsd/$f.xsd)"
done
#+end_src

The =Bound= column comes from the generated binding itself:

#+begin_src sh
grep -o 'void load_file(const std::string& file, [a-zA-Z_]*' \
    projects/ores.ore/core/include/ores.ore.core/domain/domain.hpp \
    | sed 's/.*, //' | sort -u
#+end_src

which prints =conventions creditsimulation crossAssetModel currencyConfig
currencyDefinition ore portfolio pricingengines simulation stresstesting
todaysmarket=. Nothing else is bound.

* What the table says

The pricing engine kind was the cheap one and it is done: a four-type schema and
125 files. Nothing else is that shape.

**Most of what remains is not bound, and that is the real cost.** =curveconfig=,
=sensitivity=, =simmcalibration=, =historicalreturnconfig= and
=baselTrafficLightconfig= have no type, no loader and no saver in
=ores.ore.core=, so each of those units begins by putting its schema into the
binding generation. Their type counts measure what comes after that step, not
the whole unit. Anyone costing them from the corpus alone will be wrong by a
step they cannot see from the corpus.

**Two kinds are bound and have files: today's market and simulation.**
=todaysmarket= is 98 files against a 50-type schema; =simulation= is 172 files
against 187 types. These are the only remaining kinds whose cost the table
states in full.

**Two kinds are trivial but prove nothing about shipped files.**
=historicalreturnconfig= and =baselTrafficLightconfig= are 28 and 24 lines with
two and three types, and the corpus ships no file of either. Their round trip
can be completed, but the fixture is built from the schema, so it demonstrates
the mapper against a document we wrote. That is worth having and is not evidence
about the corpus. Neither is bound either, so they are not cheap in practice.

**The two big ones interlock.** =curveconfig= (89 files) and =todaysmarket= (98
files) are 187 files between them, and today's market names curves that the
curve configuration defines. The curve task's own description says today's
market "is largely curve specifications", so it is worth checking first whether
today's market can be mapped against curve *names* alone, which would let it
land before the curve entity model does.

* Recommended order

1. =todaysmarket= — 98 files, 50 types, already bound. Cheapest remaining kind
   whose whole cost is visible, and it settles whether it really waits on
   =curveconfig=. It is already tasked as 0D7AA741.
2. =simulation= — 172 files, 187 types, already bound. The largest corpus among
   the kinds that need no binding work.
3. =curveconfig= — 89 files and 126 types, the largest modelling unit, and it
   needs binding first. Prerequisite for the curve half of today's market.
4. =sensitivity= — 50 files, 66 types, needs binding.
5. =simmcalibration= — 4 files but 80 types and needs binding, so its file count
   is the most misleading number in this table.
6. =historicalreturnconfig= and =baselTrafficLightconfig= — smallest schemas,
   no corpus files, need binding.

* What this table is not

It is not a schedule, and the =Bound= column is the reason it needed a second
pass. Type count measures the modelling surface, not the work: a kind whose
segments and quotes nest deeply costs more per type than one that is a flat
list. The ordering is about which unit is cheapest to finish, not about which is
most valuable. Both of the top two are also the two largest corpora left, so the
ordering happens to agree with value here — but only by luck.
