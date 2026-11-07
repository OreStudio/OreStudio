# Stage 3: historical book and portfolio NPV summaries

Version 0.1.4 adds `summarize_scope_results` to the shared agent/MCP executor.
The NPV slice is deployed locally and validated with 68 tests on Windows and
Linux, identical release artifacts and independent live MCP/GPU checks. Greek
aggregation remains an open part of Stage 3; this is not full Greek delivery.
See the [release evidence](validation.md) for the exact source and artifact.

## Scope and selection

Supply `scopeType` (`book` or `portfolio`), a literal scope name/UUID and an
explicit `runId`. Omitting the run asks for a selection instead of guessing one.
In chat, a pending run or ambiguous-scope choice lasts five minutes in that
authenticated session only. A model cannot select an unrequested run or replace
a requested Greek with an NPV figure.
The missing-run prompt is deterministic and makes no inference request. On a
UUID reply, only that session's bounded original scope intent is resumed.

The tool reconstructs **booking membership at the selected run's `started_at`**
through existing bitemporal NATS reads for books, portfolios, trades and trade
bookings. This timestamp is distinct from the run's actual pricing date.
The original UTC timestamp, including microseconds, is preserved in the NATS
request and the response. Chronology checks retain submillisecond precision.
Portfolio membership is the booking portfolio's subtree; direct trade-to-
portfolio assignments are a separate relationship and are not mixed in.
Names resolve against that historical catalogue. Missing/cyclic parents,
duplicate pages and records newer than the requested timestamp fail closed.

Only the selected party's population is considered. For every expected member,
the tool requires one result for that exact run, definition, date and measure.
A missing result is not zero and does not establish a per-trade failure status.
Output states expected, valued, missing and duplicate counts; the unavailable
per-trade failure count is `null`. Empty scopes have no zero-valued total.

## Amounts, units and provenance

Only canonical `npv.csv` NPV rows are enabled. Default `npv_base` gives a
common-base result; native `npv` is also available when every member's stated
currency agrees. Mixed/missing currencies, or a different requested currency,
withhold the total. Currency filtering never drops members to manufacture a
partial total. There is no FX conversion.

The current flat `npv_base` projection has blank currency fields. The source
CSV's explicit `BaseCurrency` is retained in the corresponding typed
`report_metrics` qualifiers. The tool joins **each** `NPV(Base)` metric by
trade, run, definition, pricing date, source, family and numerical value. It
requires one metric per member, no scenario/factor, valid `BaseCurrency`, and
agreement with any already populated currency fields. Missing/conflicting
metadata yields no total. Today's book/configuration currency is never inferred
as a substitute, and no stored backend rows are modified.

Totals use BigInt decimal arithmetic over the canonical decimal renderings of
the stored backend double values. This avoids binary accumulation error but
cannot recover precision already lost in backend double storage. No monetary
rounding is applied. Contributor amounts and the total are decimal strings.
Contributors sort by absolute amount, then trade UUID; `offset`/`limit` page
the drill-down while the total continues to cover the whole verified scope.

The output names the scope, run, health, membership timestamp, actual pricing
date, completion time, cut (or its absence), pricing adjustment and source.
Run health/provenance is rechecked before returning figures. Reads across
mutable backend tables are not claimed to be one atomic database snapshot.
Only clean completed runs with coherent dates are accepted. Partial/failed/
running runs have no total. Undated requests flag stored pricing dates older
than three days as stale; an explicit historical date must match the run.

## Greeks and bounds

The 0.1.4 release did not enable Greeks. Version 0.1.5 adds
[stored raw sensitivity buckets](greeks.md) through the same tool for `greeks`,
`delta`, `gamma`, `vega` and `dv01`, preserving factor/bump/scenario/currency
dimensions. Incomplete or unverified buckets have no full-scope total. No
sensitivity or zero value is inferred from an NPV or an absent row.

Each historical/result scan is bounded at 2,000 rows. Booking requests use
500-trade batches; a result scope is limited to 1,000 expected trades and a
portfolio chain to 50 parents. Contributor pages contain at most 25 rows.
The existing 30-second tool deadline, eight-second in-flight NATS bound,
session/tool budgets and 32 KiB output limit apply. A catalogue beyond its
bound cannot establish a complete population, even for a small named book.

## Live reference and reproduction

The clean Acme run `884d0bc9-932d-4c4c-9b66-ee97d2b810bb` priced at 2026-11-06.
Its membership timestamp is `2026-11-06T21:42:03.608662Z`. Raw historical NATS
records and typed CSV metrics were independently summed with Python `Decimal`:

| Scope | Verified members | Base NPV, GBP |
| --- | ---: | ---: |
| Group FX Hedges | 63 | 206318954.123122 |
| Group Intercompany Loans | 10 | 50969977.080832 |
| Group Treasury Portfolio | 73 | 257288931.203954 |

The two books reconcile to their parent. Native FX-book `npv` is correctly
withheld because it mixes GBP and USD; the base values have explicit GBP
qualifiers. The independent reference scripts import no agent arithmetic.

```text
node scripts/inspect-scope-reference.mjs <config> <private-credentials.json> <party-id> <run-id> <private-raw.json>
python scripts/reconcile-scope-reference.py <private-raw.json> <private-reference.json>
node scripts/smoke-scope-reporting.mjs <agent-url> <private-credentials.json> <party-id> <private-reference.json> <optional-other-party-id> <optional-partial-run-id>
```

The deployed smoke checks every paged contributor against the independent
reference, whole-scope totals, second-party denial, partial-run withholding,
real model tool selection and session-bound run clarification. It is read-only.
Raw local evidence belongs in ignored `build/output/`, never source control.

Only the agent allowlist gains existing read subjects
`trading.v1.trade_bookings.list` and `reporting.v1.report_metrics.list`. No new
server handler/build, broker ACL/configuration, report execution or feed import
is needed by this release. The agent remains separately built and deployed.
Role-level/cross-tenant proof, hosted authentication, broker restrictions and
native Linux NVIDIA execution remain distinct open gates.
