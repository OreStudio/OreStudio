# Run agent evaluations

The runner uses this checkout's `Agent` and `ToolExecutor`. It does not deploy
code or change the installed agent. Use Node 22.11+ and locked dependencies.
From `projects/ores.agent`:

```text
npm ci --ignore-scripts
npm run eval
```

The default builds the agent and runs offline replay. It needs no model or NATS.

## Profiles and datasets

| Profile | Inference | Business data |
| --- | --- | --- |
| `replay` | Scripted responses | Labelled fixtures and the real executor |
| `model` | Configured local model | Labelled fixtures; no NATS |
| `live` | Configured local model | Existing records through read-only NATS |

| Corpus | Cases | Purpose |
| --- | ---: | --- |
| `evals/cases.json` | 26 | Original baseline; profile membership is explicit. |
| `evals/holdout-stage5.json` | 8 | Separate paraphrase/clarification checks. |
| `evals/review-v2.json` | 11 | Stronger measure/identity checks; no live profile. |

Fixtures are labelled test data. Replay checks program behavior, not model quality. Model/live scores include
deterministic agent paths. Small corpora do not establish general accuracy.
The original and paraphrase corpora use unchanged `fixtures-v1.mjs`, which can
conceal native/base mistakes. The review corpus uses distinct immutable rows:
native EUR 10/USD 20; base GBP 0.1/GBP 0.2. It has a separate version and hash. Keep both fixture files: `fixtures-v1.mjs`
reproduces the original results; `fixtures.mjs` supports the stronger review
checks. They are not interchangeable. Do not format the frozen fixtures,
corpora, grader or baseline records.

```text
npm run eval -- --profile model --config <PRIVATE_CONFIG> --inference-root <INFERENCE_ROOT>
npm run eval -- --profile live --live-read --config <PRIVATE_CONFIG> --credentials <PRIVATE_CREDENTIALS> --party <PARTY_UUID> --other-party <OTHER_PARTY_UUID> --inference-root <INFERENCE_ROOT>
```

Credentials use `tenantUsername` and `tenantPassword`. Live mode allows existing
reads and login/party-selection/logout only. It cannot create reports, trades,
feeds or data. Missing alternate-party identity is `blocked_identity`, not a
passed isolation check. Refusal cases do not execute commands.

Run model/live profiles sequentially while inference is otherwise idle.
The runner does not start services or download models.

## Select cases and store reports

```text
npm run eval -- --profile replay --cases books,scope-npv,missing-vega --repeat 2 --out <NEW_DIRECTORY>
npm run eval -- --profile replay --corpus evals/review-v2.json
```

`--repeat` allows 1–5. `--timeout-ms` allows 100–120000 (default 60000) per turn.
Each case gets fresh agent state; its conversation turns share that state.
A case error is recorded and later cases continue.

Default output is ignored `build/output/agent-evals/<UTC-time>-<profile>-<id>/`.
An explicit directory must not exist. Reports contain:

- `results.json`: checks, replies, traces, timings and source/model identifiers.
- `summary.md`: counts and findings.
- `progress.json`: completed case count.
- `run-reservation.json`: start marker; not proof of completion.

Known secrets are redacted, but reports still contain private business data.
Keep raw reports out of Git. Commit only reviewed safe aggregates.
The JSON files in `evals/baselines/` are reviewed historical evidence. Review
the scores, failures and source identifiers. They are not inputs to the
comparison command; that command needs complete private `results.json` files.

## Scores and exit codes

Handling and business fulfillment are separate. Correctly reporting missing
vega can pass handling while live vega remains `blocked_data`. Correct booking
refusal does not mean booking works.

Outcomes: `pass`, `fail`, `blocked_backend`, `blocked_data`, `blocked_identity`
and `error`. Every selected repetition remains in the denominator. Handling
failures take priority over known capability gaps. Do not hide failures by
removing cases or changing expected values.

| Exit | Meaning |
| --- | --- |
| 0 | Complete valid report; normal mode permits quality failures and case errors. |
| 1 | Fatal setup or report-write error. |
| 2 | Strict mode failed its threshold or handling/error rules. |

`--strict --min-pass 0.9` sets an overall threshold. `--min-pass` needs strict
mode. To require handling while retaining blocked business outcomes, use
`--strict --min-pass 0 --require-handling`.

## Reproduce a result

Reports record corpus/grader/fixture hashes, source and lockfile fingerprints,
prompt/tool schemas, model settings, backend contract, platform and live IDs.
`--inference-root` also checks configured model/runtime files and hashes.
It does not prove which bytes the running inference process loaded.

An oracle is a rule that checks an expected answer. Lexical checks can miss
correct wording or semantic errors. Keep failures and known oracle limits.
[Validation](validation.md) links the frozen records. A poor score does not
approve a server or data change.

## Verify and check prerequisites

```text
npm run verify
npm run eval:doctor -- --profile model --config <PRIVATE_CONFIG>
npm run eval:doctor -- --profile live --config <PRIVATE_CONFIG> --credentials <PRIVATE_CREDENTIALS> --party <PARTY_UUID>
```

`verify` checks source format and backend contract sources, runs tests and runs
all three replay corpora. Every handling check must pass
without execution errors; blocked business outcomes remain blocked. Reports use
new `build/output/agent-checks/` paths. After tests, use
`node evals/check.mjs --out <NEW_DIRECTORY>` to select a report directory.

Doctor checks inputs, model-list endpoint and NATS TLS. It does no login,
generation or business read. Exit 0 means those prerequisites passed; 1 means
unavailable/invalid. It does not prove GPU use, access or data coverage.

## Compare and measure

```text
npm run eval:compare -- <BEFORE/results.json> <AFTER/results.json> --out <NEW_DIRECTORY>
npm run eval:compare -- <BEFORE/results.json> <AFTER/results.json> --out <NEW_DIRECTORY> --strict
```

Use the same profile and case IDs. Default comparison also matches corpus,
grader, fixtures, tool schemas, backend contract, deadline and model files/settings.
Live party selection must match. Source/prompt changes can be the experiment.
Different repeats are labelled; no cases are dropped.

`--allow-incompatible` permits a labelled descriptive comparison of other differing
identifiers. It cannot mix profiles or case sets. Strict comparison returns 2
for per-case rate regression or incompatibility. Valid normal comparisons return
0; invalid input/write/overwrite errors return 1. Use full private reports, not
committed aggregates. Live data can change; inspect traces before assigning cause.

Metrics distinguish proposed tools/arguments from validated execution. Numeric
attribution checks figures; evidence attribution includes sources. Clarification
and refusal have separate denominators. Missing records are unmeasured.

Chat spans: `agent`, `selection`, `inference`, `tool`, `adapter`, `render`.
`x-request-id` links them. Spans overlap; do not sum them. Render timing measures
formatting, not browser paint. Missing old samples do not mean zero latency.
Nearest-rank p50/p95 require 20+ samples. Mixed questions are not latency benchmarks.

Timing events exclude prompts, identities, arguments, data and exception text.
General tool audits can identify tenant/party. Protect logs and private reports.
Runtime changes need a new [deployment](deployment.md); evaluation-only runs do not.

## GitHub CPU CI

Agent workflows run once per day on the default branch:

| Workflow | Time (UTC) |
| --- | --- |
| `Agent CPU evaluation` | 03:17 |
| `Agent component` | 03:47 |

Pushes and PRs do not start these jobs. GitHub can delay scheduled jobs.
Manual runs remain available. CPU runs select `all`, `original`, `holdout` or
`review`. GitHub needs the workflow on the default branch for schedules and
manual dispatch. A workflow change on a fixes branch takes effect after merge.

Each Windows/Linux job installs the pinned CPU llama.cpp runtime and LFM3 model
in a temporary root. It forces zero GPU layers, keeps context 262144 and uses
one slot. It verifies the agent, checks real generation, then runs the `model`
profile against labelled fixtures. No NATS, production secrets or live server
is needed. CPU and CUDA results are separate experiments.

The default runs all three corpora once, with a 120-second turn deadline.
Setup/test/report failures fail CI. Quality failures remain visible findings.
Manual `require_handling` also fails CI on handling failures; blocked business
capabilities stay blocked. A green default job is not a claim that all cases pass.

Results appear in the job summary and `agent-cpu-<OS>` artifacts for 14 days.
Artifacts contain fixture reports, CPU settings and redacted inference logs.
They exclude keys, private config and control records. Cleanup stops inference
even after failure. Jobs have a 90-minute limit and cancel older runs on the same ref.
