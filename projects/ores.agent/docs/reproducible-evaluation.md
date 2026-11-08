# Stage 6: reproduce evaluations, compare and release the agent

Delivered evidence and retained failures: [Stage 6 validation](stage6-validation.md).

Use Node 22.11+ and the locked component dependencies. From `projects/ores.agent`:

```text
npm ci --ignore-scripts
npm run verify
npm run package
```

`verify` runs all component tests and three complete offline replay corpora:
the original, Stage 5 paraphrases and [versioned review regressions](request-plan.md).
Every handling check must pass, with no execution errors. Correct refusals and
missing-data answers still retain their blocked business outcomes; the gate
does not relabel them as fulfilled. Reports go into new ignored directories
under `build/output/agent-checks/`. To select an explicit new report directory
after running the tests: `node evals/check.mjs --out <NEW_DIRECTORY>`.
The manual Windows/Linux Node 22/24 CI workflow runs the same verifier and
uploads fixture-only reports separately from the checksummed package. It uses
no live credentials/model/backend. Local validation is not a claim that remote
CI or native Linux NVIDIA execution has run.

## Explicit real-model and live-read profiles

```text
npm run eval:doctor -- --profile model --config <PRIVATE_CONFIG>
npm run eval:doctor -- --profile live --config <PRIVATE_CONFIG> --credentials <PRIVATE_CREDENTIALS> --party <PARTY_UUID>
npm run eval -- --profile model --config <PRIVATE_CONFIG> --inference-root <EXISTING_INFERENCE_ROOT> --repeat 2 --out <NEW_MODEL_REPORT>
npm run eval -- --profile live --live-read --config <PRIVATE_CONFIG> --credentials <PRIVATE_CREDENTIALS> --party <PARTY_UUID> --other-party <OTHER_PARTY_UUID> --inference-root <EXISTING_INFERENCE_ROOT> --repeat 2 --out <NEW_LIVE_REPORT>
```

Doctor checks input presence, the configured model's models endpoint and (for
live) a TLS NATS connection. It performs no login, inference generation or
business reads/writes. Exit 0 means those checks passed; 1 means prerequisites
are unavailable/invalid. It is not permission, data, GPU or capability proof.
The live evaluator separately requires `--live-read`; unavailable identities/
backend/data remain explicit case outcomes. Fatal setup/report-write errors
return 1. Normal complete reports return 0 even with quality failures; optional
strict mode returns 2 at the selected threshold. `--require-handling` with
`--strict --min-pass 0` enforces handling without hiding blocked capabilities.

Choose the same corpus and case IDs for before/after runs. `--cases id,id`
selects cases; `--corpus evals/holdout-stage5.json` selects the separate eight-case
paraphrase corpus. Repeat accepts 1–5; each repeated case has fresh agent state,
with multi-turn continuation within the case. Pin private configuration, model
artifacts and existing live records/run IDs; do not create prerequisites to
make a score pass. Run model/live evaluations sequentially while inference is
otherwise idle. Historical data remains historical, and staleness changes as
time passes.

## Compare complete reports

```text
npm run eval:compare -- <BEFORE/results.json> <AFTER/results.json> --out <NEW_COMPARISON_DIRECTORY>
npm run eval:compare -- <BEFORE/results.json> <AFTER/results.json> --out <NEW_COMPARISON_DIRECTORY> --strict
```

The comparator validates unique case/repetition IDs and summary totals, and
requires the same profile and case-ID selection. By default it also requires
matching corpus, grader, selected fixture hash, tool schemas, backend contract, deadline and (for real
models) model artifacts/settings; live party selection must match. Agent source,
prompt/catalogue changes are the experiment, not a reason to discard the
comparison. Unequal repeat counts are labelled and rates use each full
denominator. There is no silently selected intersection or significance claim.

`--allow-incompatible` permits explicitly labelled descriptive comparisons
when provenance differs. It never permits mixing profiles or dropping case IDs.
Strict comparison returns 2 for any per-case handling/overall-rate regression
or an incompatible comparison; normal valid comparisons return 0, invalid input/
overwrite/write errors return 1. Existing result directories are never reused.
The comparator reads full private reports, not committed aggregate snapshots.
Live data can change despite identical configuration, so inspect traces before
attributing a regression to code. Artifact hashes describe configured files,
not attestation of an already-running process.

## Metrics and private timing

Reports retain the original task/handling/business scores and add versioned
measurement definitions. Tool selection distinguishes model proposals from
validated execution attempts. Argument denominators count expected selectors;
numeric attribution counts figure/value checks, with provenance/evidence checks
reported separately as the wider attribution measure. Clarification and command
refusal have their own denominators. Missing turn records are explicitly
unmeasured, never passed. These are bounded corpus oracles, not a semantic judge.
Generation/reply-size failures, including recovered attempts, are counted per turn.

Chat timing links `agent`, intent/catalogue `selection`, `inference`, `tool`,
NATS-adapter `adapter` and formatter `render` spans. HTTP chat uses the response
`x-request-id` as its trace ID; evaluation traces retain these IDs per turn.
Nested spans have parent IDs. Render samples are formatter attempts, including
formatters which do not handle a result. Span durations overlap and must not be
summed. Older reports have no phase telemetry; absence is not zero latency.
Every distribution reports its sample count; descriptive nearest-rank p50/p95
are emitted only at 20+ samples. Mixed case timings and individual smoke runs
are not latency benchmarks.

Runtime timing events contain IDs, phase, duration, outcome and an allowlisted
error code only. They exclude prompt/arguments, identities/tokens, data/replies
and exception text; unknown errors use `operation_failed`. Sink failures cannot
change the request. This is agent-side instrumentation, with no server/broker
instrumentation or header/protocol changes to NATS. Raw evaluation reports still
contain private business data and belong outside Git. Commit reviewed aggregate
evidence only; protect configuration/log directories with Windows ACLs or POSIX
permissions. See [evaluation privacy and exit codes](evaluation.md).

## Package and deploy separately

Packaging starts no service and downloads no model. Follow the existing
[independent deployment/rollback commands](deployment.md): verify the manifest,
install outside the checkout, stop only the installed agent controller, configure
the selected immutable release (preserving private config), start and check
health/readiness. Keep the previous installed release for rollback. Do not
restart native services, NATS or inference, migrate a schema or prepare business
data as part of this process. Runtime code changes need a new agent artifact;
report-only evaluation runs need no application deployment.

Known data/unit/model-handling gaps remain in [Stage 5 findings](stage5-validation.md).
Future architecture is still a [separate project-lead backlog](architecture-future-prs.md).
Completing this local process does not establish hosted security, least-privilege
broker access, full business fulfillment or production readiness.
