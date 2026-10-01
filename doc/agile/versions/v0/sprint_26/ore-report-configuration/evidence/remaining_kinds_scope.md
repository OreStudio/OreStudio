#+title: What remains, and what it costs

A measured scope for the configuration kinds the story still owes, so the next
unit is chosen on evidence rather than on which task happens to be listed first.
Measured on the vendored corpus at commit =e6f80a03bd=.

* The measurement

Schema size is the proxy for effort: the number of complex types in the XSD is
the number of shapes an entity model has to hold. The corpus count says whether
there is anything to prove against.

| Kind                    | Corpus files | XSD lines | Complex types | Entities exist |
|-------------------------+--------------+-----------+---------------+----------------|
| historical return       | 0            | 28        | 2             | no             |
| Basel traffic light     | 0            | 24        | 3             | no             |
| today's market          | 98           | 429       | 50            | no             |
| sensitivity             | 50           | 671       | 66            | no             |
| SIMM calibration        | 4            | 743       | 80            | no             |
| curve configuration     | 89           | 1486      | 126           | no             |
| simulation              | 172          | 1777      | 187           | no             |
| pricing engines         | 125          | 30        | 4             | yes            |
| stress test             | 18           | –         | –             | yes            |
| credit simulation       | 14           | –         | –             | yes            |

Reproduce the two numeric columns with:

#+begin_src sh
for f in historicalreturnconfig baselTrafficLightconfig todaysmarket \
         sensitivity simmcalibration curveconfig simulation; do
    printf "%-24s %3s files  %4s lines  %3s types\n" "$f" \
        "$(find external/ore -iname "$f*.xml" | wc -l)" \
        "$(wc -l < external/ore/xsd/$f.xsd)" \
        "$(grep -c '<xs:complexType' external/ore/xsd/$f.xsd)"
done
#+end_src

* What the table says

The pricing engine kind was the cheap one and it is done: a four-type schema and
125 files. Nothing else is that shape.

**Two kinds are trivial but prove nothing about shipped files.**
=historicalreturnconfig= and =baselTrafficLightconfig= are 28 and 24 lines with
two and three types, and the corpus ships no file of either. Their round trip
can be completed quickly, but the fixture is built from the schema, so it
demonstrates the mapper against a document we wrote. That is worth having and is
not evidence about the corpus.

**One kind is small and does have files.** =simmcalibration= ships 4 files
against an 80-type schema. It is the cheapest kind that advances the thing the
story actually claims — that shipped files round trip.

**The two big ones interlock.** =curveconfig= (89 files) and =todaysmarket=
(98 files) are 187 files between them, and today's market names curves that the
curve configuration defines. The curve task's own description says today's
market "is largely curve specifications", so curve configuration comes first.
The curve schema is 126 types, so that is the largest single modelling unit
left.

**Nothing is blocked on modelling only in principle.** All six kinds with a
schema and no entity model are covered by task F17348AD, which is BACKLOG and
whose description already names them: simulation and cross-asset model,
sensitivity, SIMM calibration, historical return, Basel traffic light.

* Recommended order

1. =simmcalibration= — 4 files, 80 types. Smallest kind with real corpus files,
   so it is the next proven thing rather than the next built thing.
2. =historicalreturnconfig= and =baselTrafficLightconfig= — trivial, and they
   close the kind list even though they prove nothing about the corpus.
3. =curveconfig= — 89 files, 126 types, the largest modelling unit, and the
   prerequisite for the next item.
4. =todaysmarket= — 98 files, and it cannot round trip until curves do.
5. =simulation= and =sensitivity= — 222 files between them, 187 and 66 types.

* What this table is not

It is not a schedule. Type count measures the modelling surface, not the work:
a kind whose segments and quotes nest deeply costs more per type than one that
is a flat list, and the curve schema mixes both. The ordering above is about
which unit is cheapest to finish, not about which is most valuable.
