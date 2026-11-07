# Stage 4: agent evaluation runner

Delivered baseline: [Stage 4 results and oracle limitation](evaluation-baseline.md).
Follow-up: [Stage 5 comparisons and remaining failures](stage5-validation.md).
Use `--corpus evals/holdout-stage5.json` for the separate paraphrase/clarification
corpus; the default Stage 4 corpus and grader remain unchanged.

Stage 6 adds [verification, preflight, comparison, metrics and release commands](reproducible-evaluation.md).

The evaluator runs the actual `Agent` and shared `ToolExecutor` from this
checkout. It is checkout tooling, not a new server or a change to the installed
agent bundle. The current runtime remains separately deployed. No reporting,
OreStudio/ORE, database, broker or main-UI changes are needed.

From `projects/ores.agent` with the existing locked dependencies installed:

```text
npm run eval
```

This builds the TypeScript component and runs the offline replay profile.
It does not connect to inference or NATS. The versioned corpus currently has
26 cases; profile membership is explicit in `evals/cases.json`.

## Profiles

| Profile | Inference | Business data |
| --- | --- | --- |
| replay | Labelled scripted tool-call/response replay | Labelled fixture backend, real executor/validation |
| model | Actual configured local LFM3 server | Same labelled fixture backend; no NATS connection |
| live | Actual configured local model | Existing records through authenticated read-only NATS operations |

Replay scores verify the harness and deterministic agent/tool paths. They are
**not model-quality scores**. The model profile measures actual tool selection
and conversation handling against known fixture results. Live checks exercise
existing data/API integration; they neither create nor repair prerequisites.

```text
npm run eval -- --profile model --config <private-agent-config.json> --inference-root <existing-inference-root>
npm run eval -- --profile live --live-read --config <private-agent-config.json> --credentials <private-demo-credentials.json> --party <existing-party-uuid> --other-party <existing-other-party-uuid> --inference-root <existing-inference-root>
```

The credentials JSON uses the existing `tenantUsername`/`tenantPassword` fields.
Normal login, party selection and logout establish/release evaluation sessions.
The live backend wrapper has an explicit operation allowlist for existing
reads plus that session lifecycle. `exportPortfolio` is the existing in-message
trade export guarded by `trading::trades:read`; storage exports, report triggers,
copy-scope, writes, arbitrary subjects and unknown operations are not allowed.
The fixture backend cannot perform real operations. Command/booking cases test
refusal and remain blocked business capabilities, even if handling is correct.

Run real-model profiles sequentially while the inference server is otherwise
idle. They use its existing slot and settings; the runner starts/stops/deploys
no service and downloads no model. Missing alternate-party credentials yield
`blocked_identity`, not a successful isolation test. Live capability/data
assumptions are explicitly recorded cases and may need review if records change.

## Selection and output

```text
npm run eval -- --profile replay --cases books,scope-npv,missing-vega --repeat 2 --out <new-report-directory>
npm run eval -- --profile replay --strict --min-pass 0.9
```

`--repeat` accepts 1–5 repetitions. `--timeout-ms` bounds each conversation turn
(default 60000, allowed 100–120000); a timeout is recorded and later cases
continue. Each case gets a fresh agent/clarification state; turns within it
share that state. Live sessions retain the application's normal tool budgets.
The real model's sampling is not forced to match scripted replay.

By default, new private reports go under ignored
`build/output/agent-evals/<UTC-time>-<profile>-<unique-id>/`. An explicit output
directory must not already exist; existing reports are never overwritten.
Files are created with restrictive modes where supported; Windows inherits the
parent directory's ACLs. Keep these directories local and out of Git.

- `results.json`: expected/actual turn results, failures, proposed and executed
  tool arguments, backend-operation/inference timings and provenance.
- `summary.md`: outcome table, separate handling/capability counts and findings.
- `progress.json`: completed case count for a running evaluation.
- `run-reservation.json`: profile/start marker, also present after a fatal setup
  failure; it is not proof of a completed evaluation.

Credential fields, known credential values, bearer/JWT strings and secret keys
inside proposed JSON arguments are redacted. Private error messages/stacks are
not included. Reports still contain scoped business facts and model replies;
do not commit them wholesale. Commit only reviewed safe aggregate evidence.

## Scores and exit codes

Every case has a handling result and a separate business-capability result.
For example, a correct “no volatility sensitivity available” answer can pass
handling while positive live vega stays `blocked_data`. Report execution and
booking refusal can be correct while those business requests remain
`blocked_backend`. Labelled fixtures do not close live data/capability gates.

Overall outcomes are `pass`, `fail`, `blocked_backend`, `blocked_data`,
`blocked_identity` and `error`. All selected repetitions, including blocked or
errored ones, remain in both headline rate denominators. No skipped/blocked case
is silently counted as an overall pass. Handling failures take precedence over
declared capability gaps, so a bad answer is not hidden by a known backend gap.
Failure causes remain unclassified until the trace is reviewed.

Normal mode returns **0** after writing a complete valid report, even with poor
scores or per-case errors. Fatal harness setup/report-writing failure returns
**1**. Optional `--strict` returns **2** if the all-case pass rate is below
`--min-pass` (default 1) or unexpected execution errors occur. `--min-pass`
requires `--strict`. Strict thresholds are optional, not a gate for establishing
the initial baseline. Deterministic unit/contract/harness integrity tests still
need to pass; eval quality failures are findings for Stage 5.

## Provenance and limitations

Reports record corpus version/hash, agent Git revision and dirty status, source/
lockfile fingerprints, prompt and tool-catalogue hashes, model request settings,
Node/platform, backend contract revision and live party/run/data identifiers.
When `--inference-root` is supplied, the runner reads the existing service/lock
files, streams the GGUF hash, checks it against the pinned hash, and records
runtime-binary hash, quantization, release and configured context/GPU/thread
flags. It does not change those files. These are configured-artifact records;
the evaluator does not attest which bytes the already-running process loaded.
Without that root, runtime provenance is explicitly incomplete.

The corpus covers discovery, provider/book/run clarification, historical FX,
missing IR/vega, trade/scope NPV, raw Delta/Gamma/DV01, alternate-party denial,
untrusted text, refusals, malformed calls and labelled failure/cancellation
paths. Several prompts are paraphrases rather than the previous smoke wording.
Numerical/reference and critical argument checks are deterministic; prose
checks are deliberately bounded lexical oracles and can miss semantic errors.
They are not an LLM judge, broad accuracy benchmark or financial certification.
Case results describe this corpus/model/data profile, not all user questions.

Positive live IR/vega and future command capabilities stay open where data/APIs
are absent. Use reports to discuss [future PR proposals](architecture-future-prs.md)
with the project lead, not to authorise server changes. The evaluator itself
does not make production-readiness claims or weaken existing safeguards.
