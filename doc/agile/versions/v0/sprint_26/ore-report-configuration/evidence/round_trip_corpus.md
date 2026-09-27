# Round-trip corpus census

Every ORE configuration document in the vendored tree, counted by exact
file name. A prefix glob over-counts, because it also matches the
variants of other documents (`ore_classic.xml`, `simulation_*.xml`), so
the table below uses `-name "<type>.xml"` only.

Command, run from the repository root:

```
for n in ore sensitivity stresstest simulation simmcalibration \
         creditsimulation historicalreturnconfig baselTrafficLightconfig \
         curveconfig conventions currencies; do
  printf "%-24s %s\n" "$n" "$(find external/ore/examples -name "$n.xml" | wc -l)"
done
```

Measured at `adbea845a4`, on `feature/ore-report-configuration`:

| Document | Exact name count | Prefix glob count |
|----------|------------------|-------------------|
| ore.xml | 82 | 416 |
| simulation.xml | 91 | 172 |
| curveconfig.xml | 76 | 89 |
| conventions.xml | 67 | 72 |
| sensitivity.xml | 40 | 50 |
| stresstest.xml | 16 | 18 |
| currencies.xml | 13 | 13 |
| simmcalibration.xml | 2 | 2 |
| creditsimulation.xml | 2 | 14 |
| historicalreturnconfig.xml | 0 | 0 |
| baselTrafficLightconfig.xml | 0 | 0 |

The prefix column is what a careless glob reports. It is recorded here
because the task descriptions written at `2a16e709eb` quoted it: those
numbers are the prefix counts, not the file counts. The exact counts are
the ones a round trip must cover.

Two document types have no file in the corpus at all:
`historicalreturnconfig.xml` and `baselTrafficLightconfig.xml`. Their
tasks build a fixture from the schema and record the absence as a
measurement.
