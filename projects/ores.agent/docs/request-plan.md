# Typed requests and review-driven cleanup

Delivered evidence: [0.1.8 review validation](review-validation.md).

Release 0.1.8 keeps the same standalone application, shared MCP/tool executor,
NATS transport, read-only operations and local model. No reporting/main-server,
ORE, database/business-data or broker change is part of this cleanup.

Chat now follows five explicit steps: parse a `RequestPlan`, resolve a pending
choice, validate proposed arguments, execute, and render the matching result.
The plan holds literal trade/run IDs, scope, measure, date, provider, currency
and conservative partial-result consent. A model cannot replace a selected
trade or run; omitted explicit fields are filled and contradictory selectors
are rejected. The requested measure is set from the plan, including the
common-base default for scope NPV. The model cannot independently enable partial
results. Explicit inspection is supported; negative/absent consent blocks it.
Provider phrases are parsed once, including `from provider ore`.

Complete scope requests execute deterministically through the shared executor.
A missing run creates a five-minute, identity-bound pending choice. A UUID reply
fills the existing plan's run field; it does not reconstruct prose or call the
model again. This preserves the selected base/native measure, date and factors.
One `Conversations` store replaces the separate market/discovery/scope maps and
context map. It also resumes exact candidate choices without allowing state to
cross tenant, party or backend-session identity changes. Unrelated messages
consume pending state as before; general conversation history is not retained.

`presentation.ts` validates the small result envelope and chooses one domain
formatter. Existing domain adapters retain financial validation and response
formatting. `catalogue.ts` builds schema descriptions once and uses explicit
route tables. The agent depends on a small tool-execution interface. Busy-slot
cleanup encloses planning as well as execution. Repeated-call, session, deadline,
tool-count, context and output-size safeguards remain in place.

`read-pages.ts` shares bounded pagination, duplicate detection, total stability
and complete-page checks across discovery and reporting adapters. Ownership,
snapshot timestamps, microsecond membership, currency/coverage validation and
domain limits stay in the adapters. Market observation selection keeps its
specialised early-stop/date rules. This does not introduce a generic financial
query service or move semantics into the main server.

## Versioned evaluation changes

The frozen `cases.json`, `holdout-stage5.json` and grader are unchanged.
`fixtures-v1.mjs` preserves their previous fixture behavior byte for byte; the
runner explicitly selects it for those two corpus versions. This historical
fixture can conceal a native/base-measure mistake and must not be treated as
new evidence of correct measure selection.

The separate `review-v2.json` corpus (`review-2.0.0`, 11 cases) uses immutable
fixtures in `fixtures.mjs`. Native NPV rows are EUR 10 and USD 20; base rows are
GBP 0.1 and GBP 0.2. Requests filter these stored rows instead of relabelling
their measure. The new cases assert measure, currency and value/basis explicitly,
including native/base separation and withholding mixed-currency totals. Negative
model proposals test identity substitution and partial-result consent. Unit
tests also bypass chat normalization to prove that a wrong measure fails the
stronger oracle.

`npm run verify` gates all three replay corpora. To run the new model/fixture
suite separately after building:

```text
node evals/run.mjs --profile model --corpus evals/review-v2.json --config <PRIVATE_CONFIG> --inference-root <EXISTING_INFERENCE_ROOT> --repeat 2 --out <NEW_DIRECTORY>
```

New review cases do not create reports or data and have no live profile. Use the
existing explicit live-read corpus against previously stored records. Reports
record the actual fixture version/hash. Comparison checks that hash, deriving
the legacy value from older reports when needed; different fixture versions
require an explicitly incompatible descriptive comparison. Different case-ID
sets remain separate. The new corpus cannot be presented as the original
baseline or used to overwrite it.

Options parsing and Markdown reporting now live in separate modules, and the
evaluation modules have expanded readable blocks. Duplicate CLI options are
rejected. Runtime and evaluation response shapes remain compatible. Structured
requests may use no inference; application handling scores are not isolated
model accuracy. Existing base-NPV unit/data and positive live IR/vega/command
capability gaps remain explicit; no result is repaired to increase a score.
