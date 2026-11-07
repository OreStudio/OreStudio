# Stage 5 validation — bounded agent workflows

Implemented and validated on 2026-11-07. Code revision:
`17cb7bc4a07cb9108ab81fe7abf69d8afb7303be`. Installed separate release:
`0.1.6-4317a6cd1870`. See [behavior and boundaries](intent-workflows.md),
[original Stage 4 baseline](evaluation-baseline.md) and the reviewed
[machine-readable comparison](../evals/baselines/stage5-2026-11-07.json).

All **94 tests pass on Windows and pinned Linux**, including the existing
independent MCP, artifact, lifecycle, reporting/unit/coverage safeguards and
new intent/clarification/catalogue checks. Both replay corpora match across
platforms with identical source fingerprints. Windows/Linux package manifests
are identical. Linux ran Node v24.22.2 in the same pinned image as Stage 4;
Windows used v24.16.0. Native Linux NVIDIA execution and remote CI are not claimed.
The Linux extracted archive records Git revision `unknown`; the external archive
revision and matching fingerprints provide source provenance.

## Frozen-corpus comparison

The original 26-case corpus and grader are unchanged. Stage 4 model/fixture
handling was **14/21**, and live handling **13/18**, each one repetition. This
stage repeats the original model and live profiles twice, preserving every
failed/blocked case in the denominator. Sampling settings, model/runtime
artifacts, configured 262144 context and 2048 output allowance are unchanged.
These are assistant/application handling scores, including deterministic
refusal/clarification paths, not isolated model accuracy or broad benchmarks.

| Profile | Cases/repetitions | Handling passed | Overall passed | Failed | Blocked backend | Blocked data | Errors |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| replay | 26 | 26 | 22 | 0 | 2 | 2 | 0 |
| model | 42 | 42 | 34 | 0 | 4 | 4 | 0 |
| live | 36 | 33 | 25 | 3 | 4 | 4 | 0 |
| holdout-replay | 8 | 8 | 6 | 0 | 2 | 0 | 0 |
| holdout-model | 8 | 8 | 6 | 0 | 2 | 0 | 0 |
| holdout-live | 8 | 8 | 6 | 0 | 2 | 0 | 0 |

Final model/fixture handling: **42/42**. Final live handling:
**33/36**. Live failed-check case IDs: `scope-run-clarification`, `trade-npv`.
Missing IR/vega remain data gaps; reporting/booking remain blocked capabilities
even when the refusal passes. No denominator, expected value or grading keyword
was changed to manufacture an improvement. The Stage 4 booking-oracle false
negative remains in its historical baseline; the new deterministic response
passes that same bounded check.

The eight additional paraphrase/clarification cases were frozen before initial
model execution. They reuse fixtures/existing records, have separate denominators
and were not used to tune the implementation. They were rerun on the final
revision as regression checks; they are not an independent statistical test set.

## Retained findings

The existing live trade base-NPV read fails `invalid_result_units`. It now stops
with an explicit withheld answer instead of returning an unrelated run list.
The positive business test deliberately remains failed: no currency is inferred,
stored result altered or backend repair made. Review this with the project lead.

One final run-selection continuation proposed native `npv` instead of the
common-base default. The tool withheld mixed-currency totals, so the handling
check remains failed; its second repetition passed. This is a remaining agent
measure-selection gap, not permission to invent a total or modify the backend.

An earlier Stage 5 live comparison at `0ca93f8aa3` passed 32/36 handling checks.
It retained two unit failures, a Delta request answered with run status and one
scope-value response hitting `generation_limit`. The Delta trace motivated
removing run-status discovery from an already selected scope-valuation catalogue;
run selection remains a separate clarification. The full earlier report remains
under `build/output/agent-stage5-live/`; it is not overwritten by final evidence.
The generation allowance was not increased. Inspect final traces for remaining
model/continuation/limit failures; these small repeated samples do not establish
perfect routing or conversation behavior. No financial-data or safety check was
weakened to pass the suite.

## Independent rollout and private evidence

Only the standalone agent was installed/switched. Existing private configuration
was preserved and the previous immutable 0.1.5 release remains available for
rollback. Installed authenticated chat smoke checks passed scope-value phrasing,
natural-date FX, command refusal without reads and explicit unit-error withholding.
Main web app, agent health and readiness returned HTTP 200. All 19 inspected
native/broker/inference PIDs and the private agent-config hash stayed unchanged;
only the agent application lifecycle changed. No reporting/server/ORE source,
broker configuration, schema or business record was changed. Normal existing
login/logout is the only session lifecycle used by evaluations/smoke checks.

Private artifacts are ignored under `build/output/`:

- `agent-stage5-final-{replay,model,live,holdout-replay,holdout-model,holdout-live}/`
  contains complete JSON, readable summaries and redacted raw traces.
- `agent-stage5-final-linux/` contains both replay reports and the matching manifest.
- `agent-stage5-final-windows-test.log`, `agent-stage5-final-linux-check.log`
  and `agent-stage5-final-package.log` record component checks and packaging.
- `agent-stage5-installed-smoke.json` records safe installed check names/statuses;
  `agent-stage5-rollback.json` records the previous/new release directories.

Only reviewed aggregates are committed. Future backend architecture remains
[separate proposals](architecture-future-prs.md). Next is Stage 6 reproducible
comparison/release work within the agent/evaluation scope, retaining these gaps.
