# Read intent and clarification

The agent uses one bounded tool loop and a shared chat/MCP executor. Intent
selection narrows the model's tool catalogue. It is not an access-control rule;
the executor and backend still validate requests.

The intent parser recognises supported scope-value questions, FX pairs and
explicit selectors. It accepts valid ISO dates and full English dates.
Invalid/conflicting dates require clarification. Relative dates, date ranges,
interpolation and new price calculations are not supported by this parser.

Explicit IDs, dates, factors, scenarios, currency and providers are preserved.
Omitted model fields can be filled from the request. Conflicting guesses are
rejected. Provider forms include `from ore`, `using ore` and `from provider ore`.
The model cannot enable partial-result consent.

Recognised write, booking and report-execution requests receive a read-only
response before inference. Unknown wording can reach the bounded model loop,
but no write tool exists.

Pending provider/scope/run choices expire after five minutes. They are bound
to tenant, party and backend session. An identity change or unrelated message
clears them. There is no persistent general history.

Selected validation failures return a safe error code and withhold the value.
They do not ask the model to substitute an unrelated lookup. See the
[typed request design](request-plan.md).

## Evaluation

The original corpus/grader stays frozen. Eight separate paraphrase cases are
in `evals/holdout-stage5.json`. They have a separate denominator and are not an
independent statistical benchmark.

```text
node evals/run.mjs --profile replay --corpus evals/holdout-stage5.json
```

See [evaluation](evaluation.md) for model/live options and
[historical Stage 5 results](stage5-validation.md) for retained failures.
