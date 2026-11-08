# Review cleanup validation — 0.1.8

Implemented and validated 2026-11-08. Final code:
`055af005e05aa5bdc59c3993609a4937baa3bf85`; structural checkpoint:
`80b04ac08b2b7dc6b2f9fc04635ebbe4866dff53`. Installed independent release:
`0.1.8-c1371bb8918e`. See [request-plan design and versioned fixtures](request-plan.md)
and the [reviewed aggregate](../evals/baselines/review-2026-11-08.json).

The four review findings have targeted coverage: a model cannot substitute an
explicit trade/run identity; absent/negative partial-result consent blocks
`allowPartial`; `from provider ore` preserves ore; immutable fixtures and stronger
measure/value/currency assertions expose native/base confusion. Direct executor/
grader tests independently prove that wrong native results fail the new oracle.

One typed request plan replaces scattered selector policy, and one identity-bound
pending-choice store replaces separate clarification maps. Complete scope requests
and run continuation use the shared executor without another model call. One
presentation dispatcher replaces formatter probing, tool schemas are cached,
and bounded page mechanics are shared across discovery/reporting adapters. Domain
financial checks/formatters remain in place. Request, conversation and presentation
metadata are typed; this is not a complete rewrite of every domain output type.
The evaluation CLI now separates options and report rendering into readable modules.

## Validation and preserved baseline

All **107 tests pass on Windows and pinned Linux**, and all three complete replay
suites pass every handling check. Windows/Linux manifests and source fingerprints
match. Linux used the same pinned Node v24.22.2 image as Stage 6; Windows used
v24.16.0. The Linux archive report records Git revision unknown; the archive
revision, manifest and matching fingerprints identify its source. Native Linux
NVIDIA, local Node 22 and remote CI execution are not claimed.

Original corpus/grader bytes are unchanged. The original and Stage 5 corpus
versions explicitly select byte-for-byte preserved `fixtures-v1.mjs`, retaining
their historical native/base blind spot. The new eleven-case `review-2.0.0`
corpus uses distinct immutable native/base rows, with a separate hash/version and
denominator. New reports record the actual fixture hash; comparison checks it,
including compatible legacy provenance for older reports. Scores from the new
corpus must not be substituted for the old baseline or called broader accuracy.

| Profile | Cases/repetitions | Handling passed | Overall passed | Failed | Blocked data/backend |
| --- | ---: | ---: | ---: | ---: | ---: |
| model | 42 | 42 | 34 | 0 | 8 |
| live | 36 | 34 | 26 | 2 | 8 |
| v2-model | 22 | 22 | 22 | 0 | 0 |
| holdout-model | 8 | 8 | 6 | 0 | 2 |
| holdout-live | 8 | 8 | 6 | 0 | 2 |
| replay | 26 | 26 | 22 | 0 | 4 |
| holdout-replay | 8 | 8 | 6 | 0 | 2 |
| v2-replay | 11 | 11 | 11 | 0 | 0 |

Original model/live profiles and the new review/model profile ran twice; the
paraphrase profiles ran once. Strict comparison with Stage 6 reports matching
recorded provenance and no per-case rate regressions for the original replay,
model and live selection. The live run-continuation case now passes 2/2 instead
of 0/2, taking handling from 32/36 to 34/36. Two existing positive trade base-NPV
checks still fail `invalid_result_units`; values remain withheld. IR/vega data
gaps and report/booking capabilities remain blocked. No value, currency or
backend record was repaired to raise a score.

The model/runtime artifacts, sampling, 262144 context and 2048 output allowance
are unchanged. Structured requests and continuation can bypass inference, so
these are application-handling scores, not isolated model accuracy. The model
profile's actual inference/proposal traces and denominators remain explicit.
Rendering now measures one selected formatter instead of several probes; mixed
timing samples must not be advertised as a before/after latency benchmark.

## Installed release and isolation

Installed authenticated chat passes seven smoke checks: stored scope value,
typed run continuation, sentence-ending factor punctuation, provider/date FX,
read-only command refusal, existing unit-error withholding and private correlated
timing. Main web app, agent health and readiness return HTTP 200. The previous
immutable 0.1.7 release remains available for rollback. All 19 inspected native,
broker and inference PIDs and private agent configuration hash are unchanged.
Only the standalone agent lifecycle changed. Reporting/main-server/ORE source,
database/business data and NATS configuration/deployment are unchanged; the
previous backend addition remains reverted and the net feature scope stays
agent/workflow/agile only. Normal existing login/party-selection/logout is the
session lifecycle used by real checks.

Private evidence remains under ignored `build/output/`:

- `agent-review-final-{model,live,v2-model,holdout-model,holdout-live}/`: complete
  raw reports/traces; `agent-review-final-replays/` contains the three replay reports.
- `agent-review-final-compare-{replay,model,live}/`: controlled-provenance comparisons.
- `agent-review-final-linux/`: three matching Linux reports and artifact manifest;
  `agent-review-final-linux-check.log`, `agent-review-final-windows.log` and
  `agent-review-final-package.log` record verification/packaging.
- `agent-review-installed-smoke.json`, `agent-review-rollback.json` and
  `agent-review-scope-*`: installed checks, release paths and process/config checks.

The earlier structural-checkpoint reports `agent-review-{model,live}/` are retained,
not overwritten. A final selector-punctuation correction was separately committed,
tested and evaluated before release. Only reviewed aggregates are committed.
Future full result-type migration and broader natural-language coverage can be
agent-only iterations; server/query/security proposals remain separate project-lead
work. No production-readiness or full business-fulfillment claim follows.
