# Typed request design

Current chat steps: parse the request, resume a pending choice, validate proposed
arguments, execute the tool, then format its result.

| Module | Responsibility |
| --- | --- |
| `intent.ts` | `RequestPlan`: literal IDs, scope, measure, date, provider, currency and partial-result consent. |
| `conversation.ts` | One expiring, identity-bound store for pending choices. |
| `catalogue.ts` | Cached schemas and explicit route-to-tool lists. |
| `presentation.ts` | Validate the conversation envelope and select a formatter. |
| `read-pages.ts` | Bounded paging, duplicate detection and stable totals. |
| Domain adapters | Financial units, coverage, ownership and result formatting. |
| `markdown.ts` | Render reply Markdown with raw HTML and images disabled. |

The model cannot replace explicit trade/run IDs, change the requested measure
or grant partial-result consent. Scope NPV defaults to common-base values.
Complete scope requests execute directly. A run-ID continuation fills the typed
plan without another model call.

Pending choices last five minutes. Tenant, party or backend-session changes
clear them. Page mechanics are shared; domain rules remain in their adapters.
Market observations retain their own date/early-stop rules.

This is a limited cleanup, not complete typing of every domain output. Agent
financial analysis remains client-derived. See the
[ownership rationale](architecture-future-prs.md).

## Evaluation fixtures

Original `cases.json` and `holdout-stage5.json` use unchanged `fixtures-v1.mjs`.
Those legacy fixtures can conceal native/base measure mistakes.

The separate eleven-case `review-v2.json` uses immutable rows in `fixtures.mjs`:
native values are EUR 10 and USD 20; base values are GBP 0.1 and GBP 0.2.
Checks assert measure, currency and value. Wrong-measure executor/grader tests
must fail. The original corpus, grader and historical scores stay unchanged.

`npm run verify` checks all three replay corpora. To test the review corpus with
the real model after building:

```text
node evals/run.mjs --profile model --corpus evals/review-v2.json --config <PRIVATE_CONFIG> --inference-root <INFERENCE_ROOT> --repeat 2 --out <NEW_DIRECTORY>
```

The review corpus has no live profile. Reports record the actual fixture hash.
Different fixtures/cases require separate or explicitly incompatible comparisons.
Application handling scores include deterministic paths; they are not isolated
model accuracy. See [0.1.8 evidence](review-validation.md).
