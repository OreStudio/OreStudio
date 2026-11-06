# FX spot and basic IR lookup

This enhancement changes only `ores.agent`. It uses the existing series-list
and observation-list NATS operations. No OreStudio server build/restart,
database changes, new NATS subjects or broker configuration changes are required.

Examples:

- “What is EURUSD spot?” — choose a provider if several match; reply “ore”.
- “What is EURUSD spot from ore?” — show the latest stored value, its date,
  source and a stale warning if appropriate.
- “What was EURUSD spot from ore on 2016-12-30?” — historical valuation-date lookup.
- “What is the USD 5Y swap rate from bloomberg?” — exact maturity-point lookup.

`read_fx_spot` accepts baseCurrency/quoteCurrency; units are quote currency per
one base currency. No inversion, triangulation or interpolation is performed.
`read_ir_rate` accepts currency, instrument (`ir_swap`, `mm`, `zero`, `discount`)
and maturity `term`. Optional indexName, curveId, floatingTenor and forwardStart
disambiguate the series. The floating-leg tenor is **not** the requested
maturity: a `tenor=3M` swap series can contain a `term=5Y` observation. Periods
are compared by exact month/day equivalents (1Y = 12M, 1W = 7D); months are not
converted into approximate days. Named indices are not inferred from unnamed
series. Rates retain decimal units and precision; discount factors have distinct
units. Date-coordinate maturities, curve interpolation and pricing are outside
this basic period-maturity lookup.

Both tools allow an explicit provider or exact matching seriesId, optional UTC
asOf date and maxAgeHours (default 72). Future valuation dates are rejected.
Historical dates use an inclusive UTC day end; today's cutoff is capped at now.
Freshness is measured against that cutoff, not against retrieval time. Every
value includes provider, point URI, observedAt, source, units and freshness.
`stale` results preserve the historical observation but never label it current.

Results are `ok`, `stale`, `ambiguous`, `not_found`, `no_observation` or
`incomplete`. Ambiguous providers are not ranked by which happens to contain
data. No implicit other-party, system-catalogue or reverse-pair fallback exists.
The selected session tenant/party must match both series and observation.
Generic series/observation tools also restrict returned data to the selected
party. Backend authorization remains authoritative.

The agent renders market facts directly from validated tool output. It cannot
use another generation turn to choose an ambiguous provider or rewrite an old
price as current. A short provider/series clarification is retained per session
for five minutes; logout/restart loses it. This is not general conversation
memory. Explicit “from <provider>” / “provider <code>” phrases are preserved even
if the small model omits an optional argument; unrequested model-selected
providers/IDs are rejected.

The adapter scans pages of 50 with a 1,000-row ceiling for each catalogue or
observation scan. Catalogue reads share an as-of snapshot, and provider/ID
filters narrow them. Observations use the existing default datetime-descending
order and an inclusive cutoff filter. Exact point matching reads through the
latest timestamp group; duplicate/conflicting observations, inconsistent pages
and scan ceilings produce no quote. A 20-second lookup cancellation deadline
applies; an outstanding core NATS read can take up to its existing eight-second
deadline to finish. A larger installation may benefit from an atomic backend
instrument/point resolver, but this release does not require or add one.

Live Acme checks found EURUSD provider `ore` at **1.0552**, observed
**2016-12-30**, source `fed.h10.2016-12-30`. This is imported historical sample
data, not a current quote. The sampled USD 5Y Bloomberg swap series has no
observations; the tool reports that and supplies no rate. Populated IR success
cases, negative rates, exact maturities and discount factors are verified with
explicitly labelled fixtures. No live-populated IR success is claimed.

Run `node scripts/smoke-market.mjs <agent-url> <private-credentials-json> <party-id>`
against that existing Acme dataset to test external MCP, natural-language calls,
provider clarification and a currency regression without changing backend data.
