# Verify, compare and measure

From `projects/ores.agent`:

```text
npm ci --ignore-scripts
npm run verify
npm run package
```

`verify` runs component tests and all three replay corpora. Every handling check
must pass without execution errors. Correct refusals retain their blocked
business outcomes. Reports go to new ignored `build/output/agent-checks/` paths.
To select a path after tests, run `node evals/check.mjs --out <NEW_DIRECTORY>`.

The manual Windows/Linux Node 22/24 workflow runs this check without live
credentials. See [validation](validation.md) for actual execution evidence.

## Check prerequisites

```text
npm run eval:doctor -- --profile model --config <PRIVATE_CONFIG>
npm run eval:doctor -- --profile live --config <PRIVATE_CONFIG> --credentials <PRIVATE_CREDENTIALS> --party <PARTY_UUID>
```

Doctor checks inputs, the model-list endpoint and live NATS TLS connectivity.
It does no login, generation or business read. Exit 0 means those checks passed;
1 means unavailable/invalid prerequisites. It does not prove GPU use, permissions
or data coverage. Live evaluation separately requires `--live-read`.
See [evaluation](evaluation.md) for full run commands.

## Compare complete reports

```text
npm run eval:compare -- <BEFORE/results.json> <AFTER/results.json> --out <NEW_DIRECTORY>
npm run eval:compare -- <BEFORE/results.json> <AFTER/results.json> --out <NEW_DIRECTORY> --strict
```

Use the same profile and case IDs. Default comparison also requires matching
corpus, grader, fixture hash, tool schemas, backend contract, deadline and model
files/settings. Live party selection must match. Source/prompt changes can be
the experiment. Different repeat counts are labelled; no cases are dropped.

`--allow-incompatible` permits a labelled descriptive comparison when other
identifiers differ. It cannot mix profiles or case sets. Strict mode returns
2 for a per-case rate regression or incompatible comparison. Valid normal
comparison returns 0; invalid input/write/overwrite errors return 1.

Use full private reports, not committed aggregates. Live records can change
despite identical configuration. Inspect traces before assigning a cause.
Configured file hashes do not attest a running process.

## Metrics and timing

Scores distinguish proposed tools/arguments from validated execution. Numeric
attribution counts figure/value checks; evidence attribution includes source
checks. Clarification and refusal use separate denominators. Missing turn records
are unmeasured, not passed. These checks are not a semantic judge.

Chat spans cover `agent`, `selection`, `inference`, `tool`, `adapter` and `render`.
The HTTP `x-request-id` links them. Nested spans overlap: do not sum them.
`render` measures formatter work, not browser paint. Older missing samples do
not mean zero latency.

Each distribution includes sample count. Nearest-rank p50/p95 appear at 20+
samples. Mixed questions and single smoke runs are not latency benchmarks.
Timing success does not prove a correct financial answer.

Timing events exclude prompts, arguments, identities, tokens, data and exception
text. General tool audit events can still identify tenant/party; protect logs.
Raw evaluation reports contain business data even after secret redaction.

## Release

Only runtime changes need an agent package and [deployment](deployment.md).
Evaluation-only work and document changes need neither a restart nor a model
download. Retain the previous release and private configuration for rollback.
Do not rebuild/restart native services or prepare business data for this process.

Historical evidence: [Stage 6](stage6-validation.md) and
[review cleanup](review-validation.md). Open security, data and GPU checks remain
in [validation](validation.md).
