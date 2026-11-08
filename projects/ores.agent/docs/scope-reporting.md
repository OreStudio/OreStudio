# Stored book and portfolio totals

`summarize_scope_results` calculates client-derived totals from existing NATS
reads. It does not start a report or provide a server summary API.

Example: “What is the base NPV of book Group FX Hedges from run <RUN_UUID>?”

## Scope and membership

Supply `scopeType` (`book` or `portfolio`), literal name/UUID and `runId`.
A missing run or ambiguous scope requires a choice. Pending choices last five
minutes in that session. A run-ID reply resumes the original typed request.

Membership uses the selected run's `started_at`, not its pricing date.
Historical reads preserve microseconds. Portfolio membership follows the booking
portfolio subtree. Direct trade-to-portfolio assignments are separate.
Missing/cyclic parents, foreign records and inconsistent pages are rejected.

Each expected trade needs exactly one result for the run, definition, date and
measure. Output shows expected, valued, missing and duplicate counts.
Per-trade failure count is unknown (`null`). Missing rows and empty scopes are
not zero-valued totals.

## Amounts and checks

Default `npv_base` requires a common base currency. Native `npv` requires one
stated currency across all members. Different or missing currencies withhold
the total. Currency filtering cannot remove uncovered trades. There is no FX conversion.

The flat base-NPV rows can have blank currency. The tool joins each `NPV(Base)`
metric's explicit `BaseCurrency` using trade, run, definition, date, source,
family and value. Missing/conflicting metadata withholds the total. Current
book currency is not a substitute.

Totals use exact decimal sums of the stored double values' decimal renderings.
They cannot recover precision already lost in storage. Totals and contributors
are decimal strings. Contributors sort by absolute amount, then trade ID.
Paging contributors does not change the whole-scope total.

Only clean completed runs with coherent dates and canonical `npv.csv` rows
qualify. Health is checked again. Replies show run, membership time, pricing
date, completion, cut, source and stale status. The reads are not one atomic
server snapshot.

Limits: 2,000 rows per scan, 1,000 trades per scope, 500-trade booking batches,
50 portfolio parents, 25 contributors per page, 30-second deadline and 32 KiB output.
For sensitivity measures, see [Greeks](greeks.md).

## Independent check

```text
node scripts/inspect-scope-reference.mjs <CONFIG> <CREDENTIALS> <PARTY> <RUN> <PRIVATE_RAW_JSON>
python scripts/reconcile-scope-reference.py <PRIVATE_RAW_JSON> <PRIVATE_REFERENCE_JSON>
node scripts/smoke-scope-reporting.mjs <AGENT_URL> <CREDENTIALS> <PARTY> <PRIVATE_REFERENCE_JSON> <OPTIONAL_OTHER_PARTY> <OPTIONAL_PARTIAL_RUN>
```

The reference uses Python `Decimal`, not agent arithmetic. Store raw evidence
outside Git. Historical totals are in [validation](validation.md).
