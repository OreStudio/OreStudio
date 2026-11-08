# Historical Stage 6 validation

Recorded 2026-11-07. Code: `5004b5e7c8e36f7c26e70788227ca92fdfa1675f`.
Release: `0.1.7-0a1d2f580ce4`. See [current validation](validation.md),
[comparison/metric definitions](reproducible-evaluation.md) and the
[reviewed aggregate](../evals/baselines/stage6-2026-11-07.json).

101 tests passed on Windows and pinned Linux. Replay fingerprints and package
manifests matched. Windows used Node v24.16.0; Linux used v24.22.2. Native Linux
NVIDIA, local Node 22 and remote CI execution were not claimed.

## Recorded results and metrics

| Profile | Cases/repetitions | Handling passed | Overall passed | Failed | Blocked backend | Blocked data |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| replay | 26 | 26 | 22 | 0 | 2 | 2 |
| model | 42 | 42 | 34 | 0 | 4 | 4 |
| live | 36 | 32 | 24 | 4 | 4 | 4 |
| holdout-replay | 8 | 8 | 6 | 0 | 2 | 0 |
| holdout-model | 8 | 8 | 6 | 0 | 2 | 0 |
| holdout-live | 8 | 8 | 6 | 0 | 2 | 0 |


| Metric | Model/fixtures | Live reads |
| --- | ---: | ---: |
| executedToolSelection | 48/48 | 40/40 |
| proposedToolSelection | 38/38 | 32/32 |
| executedArguments | 64/64 | 60/60 |
| proposedArguments | 56/60 | 53/58 |
| clarification | 6/6 | 4/4 |
| refusal | 4/4 | 4/4 |
| evidenceAttribution | 62/62 | 56/62 |
| numericAttribution | 18/18 | 14/18 |

The first table counts selected case repetitions. The second counts applicable
oracle checks; it is not an all-task accuracy denominator. Numeric attribution
is the figure/value subset of evidence attribution. Handling includes deterministic
preservation and continuation, not model accuracy alone.

Original corpus/grader, paraphrases, model files and settings were unchanged.
Model/live default cases ran twice; paraphrases ran once. Context was 262144 and
output allowance 2048. All profiles wrote complete reports without unexpected
execution errors.

## Retained regression

Strict live comparison returned 2. `scope-run-clarification` fell from 1/2 to 0/2;
live handling fell from 33/36 to 32/36. Both continuations proposed native NPV,
and mixed-currency validation withheld totals. The other two failures were the
existing `invalid_result_units` case. No result was repaired or oracle weakened.

This small sample records a regression, not its cause. Original replay/model
and paraphrase comparisons had no per-case rate regressions. IR/vega data and
command capability gaps remained. Later 0.1.8 results are separate evidence.

## Instrumentation and deployment

Doctor checked model readiness and NATS TLS without login or business operations.
Comparison tests rejected incompatible profiles/cases, duplicate results and
output reuse. Timing spans overlap and cannot be summed. Missing samples are
not zero. Percentiles need 20+ samples and remain descriptive.

Only the agent moved from 0.1.6 to 0.1.7. Config and the older release were kept.
Installed checks included 62 payload-free timing events across six phases.
Main/agent health returned 200. The 19 inspected native/broker/inference PIDs
and private config hash stayed unchanged. No server, schema, data or broker change occurred.

Private paths under `build/output/`:

- `agent-stage6-{replay,model,live,holdout-replay,holdout-model,holdout-live}/`
- `agent-stage6-compare-{replay,model,live,holdout-model,holdout-live}/`
- `agent-stage6-compare-live-strict/` and `agent-stage6-linux/`
- `agent-stage6-{windows-check-final,linux-check,package}.log`
- `agent-stage6-installed-smoke.json`, `agent-stage6-rollback.json`, `agent-stage6-scope-*`
