# Stage 6 validation — reproducible evaluation and independent release

Delivered 2026-11-07. Implementation:
`5004b5e7c8e36f7c26e70788227ca92fdfa1675f`. Installed independent artifact:
`0.1.7-0a1d2f580ce4`. See [commands and measurement definitions](reproducible-evaluation.md)
and [reviewed machine-readable evidence](../evals/baselines/stage6-2026-11-07.json).

All **101 tests pass on Windows and pinned Linux**, including telemetry privacy,
async-context isolation, observer failure isolation, matching/mismatched report
comparison, strict regression exits, prerequisite checks and offline verification.
Both complete replay corpora pass every handling check while retaining blocked
business outcomes. Source fingerprints and package manifests match across OSes.
Linux Node v24.22.2 used the same pinned image as Stage 5; Windows used v24.16.0.
The extracted Linux report records Git revision `unknown`; its archive revision
and matching fingerprints provide provenance. Remote manual CI is configured for
Windows/Linux Node 22/24 but was not dispatched; native Linux NVIDIA execution
and a local Node 22 run are not claimed.

## Repeat and compare without changing the backend

Original Stage 4 corpus/grader and Stage 5 paraphrases are unchanged. The model,
sampling settings, artifact hashes, 262144 configured context and 2048 output
allowance remain pinned. Model/live default profiles ran twice; paraphrase
profiles ran once. Every selected repetition stays in the task denominators.

| Profile | Cases/repetitions | Handling passed | Overall passed | Failed | Blocked backend | Blocked data |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| replay | 26 | 26 | 22 | 0 | 2 | 2 |
| model | 42 | 42 | 34 | 0 | 4 | 4 |
| live | 36 | 32 | 24 | 4 | 4 | 4 |
| holdout-replay | 8 | 8 | 6 | 0 | 2 | 0 |
| holdout-model | 8 | 8 | 6 | 0 | 2 | 0 |
| holdout-live | 8 | 8 | 6 | 0 | 2 | 0 |

All profiles wrote complete reports with no unexpected execution errors. The
replay/model and both real-model paraphrase comparisons match recorded provenance
and have no per-case rate regressions against Stage 5. Strict live comparison
returns **2**, correctly flagging `scope-run-clarification`: handling changed from
33/36 to 32/36 overall, with this continuation passing 1/2 previously and 0/2 now.
Both new continuations proposed native `npv` and the tool withheld mixed-currency
totals. The other two failed repetitions are the existing trade base-NPV
`invalid_result_units` rejection. Values remain withheld; no result/unit/data
repair was made. These small stochastic samples show an observed regression,
not its cause. The report and strict finding are retained rather than rerunning
until a better score appears or weakening the oracle.

The release gate is deterministic component/harness integrity; initial model
quality and business fulfillment remain observations as agreed in the plan.
IR/vega data gaps and report/booking command capabilities stay open. Future
agent-only work can strengthen common-base intent after continuation. Backend
unit/query/security proposals still require project-lead discussion/separate PRs.

## Metrics and correlated timing evidence

The new measurements distinguish proposals from validated execution:

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

These are applicable oracle sample counts, not all-task accuracy denominators.
For example model-proposed arguments pass 56/60 while executed arguments pass
64/64; deterministic preservation/continuation contributes to application success.
No claim of perfect model accuracy follows from 42/42 handling checks. Missing
turn records, truncation turns, turn errors and timing sample counts are explicit
in each report. Numeric attribution is the figure/value subset of evidence checks.
Timing outcomes describe execution success, not business fulfillment or answer
correctness. Overlapping spans must not be summed; render samples are formatter
attempts. Percentiles appear only with 20+ samples and remain descriptive, not
latency benchmarks. Older reports' missing phase timing is not zero latency.

Real prerequisite checks returned ready for the configured model endpoint,
existing identity inputs and NATS TLS connectivity, with no login, generation or
business operations. Tests also show missing model prerequisites as unavailable.
The comparator refuses incompatible profiles/case selections and, by default,
corpus/grader/configuration provenance. Explicit incompatible comparisons remain
labelled; strict mode cannot silently accept them. Non-overwrite and summary/
duplicate-case validation are covered by tests.

## Independent rollout and retained evidence

Only the standalone agent was switched from 0.1.6 to 0.1.7. Private configuration
was preserved; the previous immutable release remains available for rollback.
Installed authenticated chat checks pass scope-value phrasing, natural-date FX,
refusal without reads and explicit unit-error withholding. Five smoke checks
also verify 62 payload-free timing events linked to HTTP request IDs across all
six phases. Main web app, agent health and readiness returned HTTP 200. All 19
inspected native/broker/inference PIDs and private agent-config hash stayed
unchanged. No main/reporting/ORE source, schema, business record, broker
configuration or native deployment changed in Stage 6. Normal existing
login/party-selection/logout established evaluation/smoke sessions only.

Private artifacts remain under ignored `build/output/`:

- `agent-stage6-{replay,model,live,holdout-replay,holdout-model,holdout-live}/`:
  full JSON/Markdown, metrics and redacted raw traces.
- `agent-stage6-compare-{replay,model,live,holdout-model,holdout-live}/`:
  matched-provenance comparisons; `agent-stage6-compare-live-strict/` separately
  confirms exit 2 for the retained regression.
- `agent-stage6-linux/`: Linux replay reports and matching artifact manifest.
- `agent-stage6-windows-check-final.log`, `agent-stage6-linux-check.log`,
  `agent-stage6-package.log`: verifier/package evidence.
- `agent-stage6-installed-smoke.json`, `agent-stage6-rollback.json` and
  `agent-stage6-scope-*`: installed checks, release paths and process/config checks.

Only reviewed aggregates are committed. Stages 0–6 of the active local
agent/evaluation sequence are delivered with these explicit limitations. The
broader feature, hosted/least-privilege security, native Linux GPU validation and
[future backend architecture](architecture-future-prs.md) remain separate work;
this checkpoint does not claim full business fulfillment or production readiness.
