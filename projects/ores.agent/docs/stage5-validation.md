# Historical Stage 5 validation

Recorded 2026-11-07. Code: `17cb7bc4a07cb9108ab81fe7abf69d8afb7303be`.
Release: `0.1.6-4317a6cd1870`. See [current validation](validation.md),
[intent behavior](intent-workflows.md) and the
[reviewed aggregate](../evals/baselines/stage5-2026-11-07.json).

94 tests passed on Windows and pinned Linux. Replay fingerprints and release
manifests matched. Windows used Node v24.16.0; Linux used v24.22.2.
Native Linux NVIDIA and remote CI were not tested.

## Recorded results

| Profile | Cases/repetitions | Handling passed | Overall passed | Failed | Blocked backend | Blocked data | Errors |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| replay | 26 | 26 | 22 | 0 | 2 | 2 | 0 |
| model | 42 | 42 | 34 | 0 | 4 | 4 | 0 |
| live | 36 | 33 | 25 | 3 | 4 | 4 | 0 |
| holdout-replay | 8 | 8 | 6 | 0 | 2 | 0 | 0 |
| holdout-model | 8 | 8 | 6 | 0 | 2 | 0 | 0 |
| holdout-live | 8 | 8 | 6 | 0 | 2 | 0 | 0 |

Original Stage 4 handling was 14/21 model/fixture and 13/18 live, each one
repetition. Stage 5 repeated original model/live cases twice; paraphrases ran
once. The original corpus/grader and model settings were unchanged: context
262144 and output allowance 2048. Scores include deterministic agent behavior;
they are not isolated model accuracy.

The eight paraphrase cases were fixed before first model execution and later
rerun as regressions. They are not an independent statistical benchmark.
Correct refusals did not make report execution or booking available.

## Retained findings

- Live `trade-npv` failed `invalid_result_units`; its figure stayed withheld.
- `scope-run-clarification` passed once and failed once. Native NPV was proposed
  instead of common-base NPV; mixed-currency validation withheld the total.
- Earlier code `0ca93f8aa3` scored 32/36 live handling. It also showed a Delta
  question answered with run status and a generation-limit response. The full
  report remains in `build/output/agent-stage5-live/`.

No expected value, unit rule, generation allowance or original grading keyword
was changed to hide a failure. IR/vega data and report/booking capabilities
remained blocked. Later continuation fixes are recorded separately.

## Deployment and evidence

Only the agent changed. Private config and the previous 0.1.5 release were kept.
Installed scope, FX, refusal and unit-error checks passed. Main/agent health
returned 200. The 19 inspected native/broker/inference PIDs and config hash
stayed unchanged. No backend, broker, schema or business record changed.

Private paths under `build/output/`:

- `agent-stage5-final-{replay,model,live,holdout-replay,holdout-model,holdout-live}/`
- `agent-stage5-final-linux/` and `agent-stage5-final-{windows-test,linux-check,package}.log`
- `agent-stage5-installed-smoke.json` and `agent-stage5-rollback.json`
