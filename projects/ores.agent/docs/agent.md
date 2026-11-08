# Use the agent

Open the agent URL. The local installation uses
[http://127.0.0.1:20100/](http://127.0.0.1:20100/).
Sign in with an existing OreStudio account. If sign-in lists parties, enter one
of their IDs and sign in again.

The browser keeps a short-lived agent token in memory. Refresh requires sign-in.
Sign out revokes the token. Cancel stops the chat request and inference fetch;
an in-flight NATS read can continue until its deadline.

## Example questions

- “List the first three configured currencies.”
- “Show my books.”
- “What was EUR/USD spot from ore on 2016-12-30?”
- “What is the base NPV of book Group FX Hedges from run <RUN_UUID>?”
- “Show Delta for book Group FX Hedges from run <RUN_UUID> for factor DiscountCurve/GBP/0/1Y.”

Replace `<RUN_UUID>` with a stored run ID. Missing data, ambiguous choices and
invalid units can prevent an answer. The agent does not book trades, run reports
or provide executable quotes.

Replies support Markdown headings, tables, lists and code blocks. Raw HTML and
images are disabled. Open **OreStudio tool evidence** to inspect source data.
Model text can be wrong. Scope totals are calculated by the agent from checked
reads; they are not a server summary API.

## API and conversation

`POST /chat` accepts `{ "message": "..." }` with an agent Bearer token.
It returns `answer`, rendered `answerHtml`, tool `evidence` and timing fields.
Use `x-request-id` to find related timing events.

Chat and MCP call the same tool executor directly. The model proposes calls;
code validates arguments and session identity. Explicit IDs, dates, measures
and providers cannot be replaced by model guesses.

Pending provider, scope or run choices expire after five minutes. They belong
to one session. Unrelated messages, logout or restart clear them. There is no
saved general conversation history.

## Limits

| Limit | Value |
| --- | ---: |
| Message | 2,000 characters |
| Model rounds / tool calls per chat | 4 / 6 |
| Proposed calls per model response | 4 |
| Active inference requests | 1 |
| Concurrent tools / tools per minute per session | 2 / 60 |
| Tool output / inference reply | 32 KiB / 64 KiB |
| Overall chat deadline | 2 minutes |

Generated output defaults to 2048 tokens. See [inference](inference.md) for
configuration and [validation](validation.md) for tested behavior and open gaps.
