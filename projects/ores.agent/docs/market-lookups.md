# FX spot and basic rate lookup

Examples:

- “What is EUR/USD spot?” Then choose a listed provider, such as “ore”.
- “What was EUR/USD spot from ore on 2016-12-30?”
- “What is the USD 5Y swap rate from bloomberg?”

These tools use existing NATS series and observation reads. They do not create
prices or import data.

## Matching and units

`read_fx_spot` accepts `baseCurrency` and `quoteCurrency`. Units are quote currency
per one base currency. There is no inversion, triangulation or interpolation.

`read_ir_rate` accepts currency, instrument (`ir_swap`, `mm`, `zero`, `discount`)
and maturity `term`. Optional index, curve, floating tenor and forward start
resolve ambiguity. Floating tenor is separate from maturity: a 3M floating leg
can belong to a 5Y swap.

Periods match exact month/day equivalents: 1Y=12M and 1W=7D. Months are not
converted to approximate days. Named indices are not inferred from unnamed
series. Rates keep decimal units; discount factors have separate units.
Date-coordinate maturities and curve pricing are unsupported.

## Dates and ambiguity

Both tools accept provider or exact matching series ID, optional UTC `asOf`
and `maxAgeHours` (default 72). Future dates are rejected. Historical dates use
UTC day end; today's cutoff cannot exceed now. Freshness uses that cutoff.
Replies include provider, point URI, observation date, source and units.

Results are `ok`, `stale`, `ambiguous`, `not_found`, `no_observation` or
`incomplete`. Stale data remains dated. Providers are not ranked by data presence.
Series and observations must match the selected tenant/party.

The agent preserves explicit provider/date selectors. A choice lasts five minutes
in one session. Invalid pages, conflicting observations or scan ceilings return
no value. Each scan allows 1,000 rows; the tool deadline is 20 seconds.
An in-flight NATS read can continue for up to eight seconds.

## Recorded data

The local sample EUR/USD point is 1.0552 from `ore`, observed 2016-12-30,
source `fed.h10.2016-12-30`. It is historical sample data. The sampled Bloomberg
USD 5Y swap series has no observations. Positive rate cases use labelled fixtures;
positive live IR remains unverified.

```text
node scripts/smoke-market.mjs <AGENT_URL> <PRIVATE_CREDENTIALS_JSON> <PARTY_UUID>
```
