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
Legacy fixtures have a native/base blind spot; see [request design](request-plan.md).

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
correct wording or semantic errors. Keep the
[historical baseline](evaluation-baseline.md), failures and known oracle limits.
See [comparison and timing](reproducible-evaluation.md) for controlled comparisons.
A poor score does not approve a server or data change.
