# Built-in agent

Open the combined application's `/` page, sign in using your OreStudio account,
and select a party when required. The browser holds only an ephemeral agent
access token in memory. Refreshing the page requires signing in again. It does
not receive the backend token, NATS certificate or inference API key. Sign out
revokes the session; Cancel aborts the request and inference fetch. Replies use Markdown for headings, tables, lists and code blocks. Raw HTML
and images are disabled. Tool evidence stays in a separate plain-text panel.
The chat API returns the source in `answer` and safe HTML in `answerHtml`.

Try: “List the first three configured currencies. Use limit 3 and offset 0.”
Discover a series with `list_market_series`, then ask for observations using
its exact ID. Search applies only to a requested page; it is not a global
instrument resolver. No booking, quote calculation or database access is
exposed. This is the plan's first read-only deliverable, not the future trading
workflow described in the issue comments.

`POST /chat` accepts `{ "message": "..." }` and the same Bearer authentication
as MCP. General conversation history is not persisted. Version 0.1.1 retains
only a pending market provider/series clarification for five minutes within
the same authenticated session; see [market lookups](market-lookups.md).
The model receives the shared tool catalogue, proposes typed tool calls, and
the agent invokes `ToolExecutor.execute` directly in this process. It never
creates an MCP client or calls its own HTTP listener. Both entry paths use
the same validation, user/party identity, budgets and NATS adapter.

Version 0.1.4 adds [scope summaries](scope-reporting.md). Book/portfolio NPV
questions receive a narrow catalogue, use a literal historical scope and
require an explicitly selected run. Pending run/scope choices are private to
the session for five minutes. Totals and contributor figures render directly
from validated results. Greek requests cannot be silently changed to NPV.
Version 0.1.5 adds [raw sensitivity buckets](greeks.md) for Delta/Gamma, vega
at its stated volatility bump and signed ORE rate Delta at a 1bp target bump.
Only complete buckets have full-scope totals; sparse results stay explicit.

Version 0.1.6 adds [bounded read-intent workflows](intent-workflows.md): broader
catalogue routing, preservation of explicit dates/bucket selectors, deterministic
command refusal and identity-bound clarification state. Invalid units/provenance
withhold the requested result explicitly instead of substituting another lookup.

Version 0.1.7 adds private correlated timing for chat selection, inference,
tool/adapter reads and formatting. The response `x-request-id` links its timing
spans without including request text, arguments, identities, data or exception
messages. See [reproduction and measurement definitions](reproducible-evaluation.md).

Limits: 2,000-character input, four model rounds, six tool calls, at most four
proposed calls per response, one active inference request globally, two active
tools per session, 60 tool calls per minute per session, 32 KiB tool output,
64 KiB inference reply, 2048 generated tokens by default (configurable through
`inference.maxOutputTokens`, 128–8192) and a two-minute overall deadline.
Repeated identical calls do not execute again. Recoverable errors are returned to the model
without credentials/internal exception details. Expiry and logout are checked
before accepting results. A prompt is guidance, not an authorization boundary.
The evidence contains authoritative tool results; model prose can still be wrong.

Deterministic tests use labelled fixtures. Live Windows evaluation on 2026-11-06
used CUDA LFM3 QAD Q4_0 and Acme's real NATS data: the three-currency request
returned AED, AFN, ALL in 2,237 ms; a sampled empty series returned a clear
no-observations answer in 1,546 ms. These are individual smoke timings, not
performance guarantees or a financial-data accuracy benchmark.
