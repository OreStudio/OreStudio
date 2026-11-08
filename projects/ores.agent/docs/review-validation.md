# Historical 0.1.8 review validation

Recorded 2026-11-08. Final code: `055af005e05aa5bdc59c3993609a4937baa3bf85`.
Structural checkpoint: `80b04ac08b2b7dc6b2f9fc04635ebbe4866dff53`.
Release: `0.1.8-c1371bb8918e`. See [current validation](validation.md),
[request design](request-plan.md) and the
[reviewed aggregate](../evals/baselines/review-2026-11-08.json).

## Changes and results

Typed requests preserve explicit IDs, providers and partial-result consent.
One session-bound store handles clarification. Complete scope requests and
run continuation can bypass inference. Schemas and page mechanics are shared;
domain checks remain in their adapters. Domain output typing is not complete.

107 tests passed on Windows/pinned Linux. All three replay handling suites
passed; source fingerprints and manifests matched. Windows used Node v24.16.0;
Linux used v24.22.2. Native Linux NVIDIA, local Node 22 and remote CI were not claimed.

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

Original model/live and review/model cases ran twice. Paraphrases ran once.
Strict comparisons found no per-case rate regressions against Stage 6.
Run continuation improved from 0/2 to 2/2. Two live trade base-NPV checks still
failed `invalid_result_units`. IR/vega and report/booking gaps remained blocked.

Original cases, grader and `fixtures-v1.mjs` stayed unchanged, including their
native/base blind spot. The new eleven-case corpus used distinct immutable
native/base rows and a separate version/hash. New scores cannot replace the
original baseline. Model/runtime, context 262144 and output 2048 were unchanged.
These are application handling scores, not isolated model accuracy.

## Installed checks and evidence

Seven checks passed: scope value, typed continuation, factor punctuation, dated
FX/provider, command refusal, unit-error withholding and safe correlated timing.
Main/agent health returned 200. Config and all 19 native/broker/inference PIDs
stayed unchanged. Only the agent lifecycle changed; 0.1.7 remained for rollback.
The earlier reporting addition stayed reverted. No server/data/broker repair occurred.

Private paths under `build/output/`:

- `agent-review-final-{model,live,v2-model,holdout-model,holdout-live}/`
- `agent-review-final-replays/`, `agent-review-final-compare-{replay,model,live}/`
- `agent-review-final-linux/` and `agent-review-final-{linux-check,windows,package}.log`
- `agent-review-installed-smoke.json`, `agent-review-rollback.json`, `agent-review-scope-*`

Earlier `agent-review-{model,live}/` reports remain unchanged. Only reviewed
aggregates are committed. See [validation](validation.md) for the later Markdown release.
