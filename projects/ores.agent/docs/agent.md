# Use the agent

Open [the local chat](http://127.0.0.1:20100/). Sign in with an existing OreStudio
account. If sign-in lists parties, enter a listed party ID and sign in again.
Refresh requires sign-in. Sign out revokes the token. Cancel stops chat/inference;
an in-flight NATS read can continue until its eight-second deadline.

## Example questions

- “List the first three configured currencies.”
- “Show my books.”
- “What was EUR/USD spot from ore on 2016-12-30?”
- “What is the base NPV of book Group FX Hedges from run <RUN_UUID>?”
- “Show Delta for book Group FX Hedges from run <RUN_UUID> for factor DiscountCurve/GBP/0/1Y.”

Replace IDs with stored record IDs. Replies support Markdown headings, tables,
lists and code blocks. Raw HTML and images are disabled. Open **OreStudio tool
evidence** to inspect source data. Model text can be wrong.

The agent does not book trades, start reports or issue executable quotes.
Scope totals are client-derived analysis, not a server summary API.

## Market data

`read_fx_spot` returns quote currency per one base currency. It does not invert
pairs, calculate cross-rates or interpolate.

`read_ir_rate` matches currency, instrument (`ir_swap`, `mm`, `zero`, `discount`)
and maturity `term`. Floating tenor is separate from maturity. Optional provider,
index, curve, floating tenor and forward start resolve ambiguity. Periods use
exact month/day equivalents; rates retain decimal units. Discount factors have
separate units. No curve pricing or date-coordinate maturity lookup is provided.

Both tools support provider/series choice, UTC `asOf` and `maxAgeHours` (default 72).
Future dates are rejected. Historical cutoffs use UTC day end; today's cutoff
cannot exceed now. Stale observations remain dated. Missing/conflicting data,
incomplete pages or scan ceilings supply no value. Scans allow 1,000 rows.

The local EUR/USD sample is 1.0552 from `ore`, observed 2016-12-30. It is historical.
The sampled USD 5Y swap has no observations; positive live IR remains unverified.

## Discovery and trade values

Discovery tools find trades, books, portfolios and counterparties. Trade search
matches ID, origin reference or type, not currency-pair terms. Exact matches take
priority; ambiguity requires a choice. Membership comes from the existing inline
portfolio export, not name similarity.

Trades/books/portfolios are selected-party data. Counterparties are an entitled
tenant catalogue. Name searches cover one labelled counterparty page; they cannot
prove global absence or uniqueness. Most catalogue scans allow 2,000 rows;
membership export allows 1,000 trades. Output pages allow 25 rows.

`read_trade` returns booking and supported terms. Book hierarchy and direct
portfolio assignments stay separate. FX-forward decimal amounts are preserved.
Bond terms are issue ID/notional only. Missing terms are not inferred.
A header recheck detects changes; related reads are not one atomic snapshot.

`find_report_runs`, `read_run_status` and `read_trade_results` read stored results.
Supported trade measures are `npv` and `npv_base`. Exactly one matching row must
agree with trade, run, definition, pricing date, currency and source. Default
selection needs clean completed health and actual pricing dates. Ambiguous runs
need an explicit ID. Runs are not combined. No FX conversion is done.

Explicit partial inspection needs a run ID and a completed-with-errors run.
Failed/running output stays unavailable. Undated results older than three days
are stale. Historical dates must match actual pricing dates. The known live
trade base-NPV case still withholds `invalid_result_units`.

## Scope totals and Greeks

`summarize_scope_results` requires a book/portfolio, literal name/ID and chosen
run. Membership uses `started_at`, preserving microseconds, not the pricing date.
Portfolio scope follows historical booking hierarchy, not direct assignments.

Every expected trade needs one result. Missing/duplicate members, empty scopes
or mixed currencies prevent a full total. Default `npv_base` joins each metric's
explicit `BaseCurrency`; current book currency is not inferred. Exact decimal
sums cannot recover precision lost in double storage. Contributor pages do not
change the whole-scope total. Reads are not an atomic server snapshot.

Greeks accept raw `sensitivity_report` rows from `sensitivity.csv`, with `IsPar=false`.
They are cash finite differences at stated shifts, not normalized derivatives.
The API does not state the exact forward/central/backward stencil. No FX conversion,
sign reversal or gamma half-factor is applied.

| Selector | Meaning |
| --- | --- |
| `delta` | Single-factor Delta; excludes cross-gamma rows' Delta columns. |
| `gamma` | Gamma; diagonal and cross-factor groups stay separate. |
| `vega` | Volatility-factor Delta: cash for its stated bump. |
| `dv01` | Signed DiscountCurve/YieldCurve/IndexCurve Delta at exactly 0.0001 target shift. No negation or rescaling. |

Preserve measure, factors, exact shifts, scenario, currency, source and raw/par
status. Missing/suppressed rows do not prove zero. Incomplete groups have no full
total. There is no grand total across factors. Per-unit Greeks and live vega
need separate proof.

Scope scans allow 2,000 rows, 1,000 trades and 50 portfolio parents. Bucket pages
default to five (maximum ten); contributor pages default to ten (maximum 25).
The tool deadline is 30 seconds. Independent reference/smoke scripts remain in
`scripts/`; historical commands are linked from [validation](validation.md).

## API, MCP and request handling

`POST /chat` accepts `{ "message": "..." }` with an agent Bearer token. It returns
`answer`, safe `answerHtml`, `evidence` and timing fields. `x-request-id` links timing.

For MCP, sign in through `POST /auth/login` using `principal`, `password` and
optional `partyId`. Send the returned `accessToken` as `Authorization: Bearer <TOKEN>`
on every `/mcp` exchange. Sign out through `POST /auth/logout`. The 14 read tools
need no model when called through MCP. Backend tokens and NATS keys stay server-side.
An MCP session ID is not a credential. Supported protocol revisions are
2026-07-28 and 2025-11-25. This local profile has no OAuth discovery/enrollment.

```text
node scripts/smoke-mcp.mjs <AGENT_URL> <PRIVATE_CREDENTIALS_JSON> <PARTY_UUID>
```

Explicit IDs, measures, dates, factors and providers cannot be replaced by model
guesses. Partial consent must come from the user. Complete scope requests can
execute without inference. A pending run/provider/scope choice lasts five minutes
in one session. Unrelated messages, identity changes or logout clear it.
There is no saved general chat history.

Implementation: `intent.ts` holds typed requests; `conversation.ts` holds choices;
`catalogue.ts` selects tools; `presentation.ts` selects a formatter. Domain adapters
retain financial checks. `read-pages.ts` shares page checks; `markdown.ts` renders replies.

## Chat limits

Message: 2,000 characters. Model rounds/tool calls: 4/6. Proposed calls per response:
4. Active inference requests: 1. Tools per session: 2 concurrent, 60 per minute.
Tool output/inference reply: 32/64 KiB. Overall deadline: two minutes.
Generated output defaults to 2048 tokens; see [deployment](deployment.md).
