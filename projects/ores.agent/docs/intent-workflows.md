# Stage 5: bounded read intent and conversation behavior

Version 0.1.6 keeps one assistant and the existing shared agent/MCP executor.
It adds a small typed intent module, rather than a new framework or service.
NATS remains the sole business transport. Financial composition, unit checks,
coverage, party ownership and deterministic rendering stay in the existing
tools. No reporting/server/ORE or broker change is part of this stage.

The intent module recognises book/portfolio stored-value phrasing and directed
FX pairs such as `EUR/USD` when selecting a tool catalogue. The agent enforces
that catalogue before execution for every route. The model still proposes tools
and arguments; catalogue selection is not a general natural-language guarantee.
No agent request can use an arbitrary NATS subject or bypass the executor.

Explicit ISO dates and full English day/month/year dates are validated against
the calendar. Conflicting dates or invalid dates produce clarification without
a lookup. An omitted model date or labelled factor/factor2/scenario is restored
from the user's explicit request. A conflicting proposed selector is rejected,
not silently changed. No relative-date, date-range, interpolation or price
calculation capability is introduced. Existing partial-result opt-in and literal
scope/run/provider guards remain in force. `using ore` is recognised as an
explicit provider phrase, alongside the existing `from`/`provider` forms.

Recognised execution/booking/change requests receive a deterministic read-only
limitation before inference or any read. Unknown wording can still fall through
to the bounded model loop; no write tools are exposed in either case. This
improves refusal handling, not business command fulfillment. Cancellation,
expiry and login are checked even for deterministic responses.

Pending clarifications remain private to the authenticated Session, with a
five-minute expiry. A tenant, party or backend-session identity change clears
all pending state, including remembered scope intent. Unrelated messages consume
the pending choice as before; there is no general persistent chat history.

Selected validation failures (including invalid units and inconsistent
provenance) now stop with an explicit withheld answer containing only a safe
error code. They do not prompt the model to replace an NPV request with an
unrelated run list. The original live base-NPV unit failure remains a finding;
no unit is invented, backend result repaired or expectation weakened.

## Evaluation discipline

`evals/cases.json` and its grading expectations remain the frozen Stage 4
corpus. In particular the original booking keyword oracle is unchanged; the
new deterministic refusal is measured against it. The historical Stage 4
false negative remains recorded. Model/runtime settings are unchanged.

Eight additional paraphrase/clarification cases were written before running
them against the model, kept in `evals/holdout-stage5.json`. They are a separate
small check, not a statistically independent benchmark or part of the original
denominator. They reuse labelled figures and existing live records and do not
establish new financial capabilities. Do not tune away or omit failed cases.

From this component directory, after building:

```text
node evals/run.mjs --profile replay --corpus evals/holdout-stage5.json
```

The `--corpus` option accepts a versioned strict corpus with the same profile,
read-only wrapper, provenance, privacy and non-overwrite rules as the default.
For local-model/live profiles use the existing private options documented in
[evaluation usage](evaluation.md). Native Linux GPU execution and hosted/broker
security remain separate gates. Measured results and remaining failures are
recorded in [Stage 5 validation](stage5-validation.md).
