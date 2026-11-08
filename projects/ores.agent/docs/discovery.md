# Find trades and reference data

Tools: `find_trades`, `read_trade`, `find_books`, `find_portfolios` and
`find_counterparties`. All are read-only and use the shared chat/MCP executor.

Examples:

- “Show my books.”
- “Show trades in book Group FX Hedges.”
- “Show trades in portfolio Group Treasury Portfolio.”
- “Find counterparty CARECR9.”
- “Read trade <TRADE_UUID>.”

## Search and scope

Trade search matches UUID, origin reference or trade type. It does not search
currency-pair terms. Exact names/IDs take priority over substring matches.
Multiple matches require a choice. A pending choice lasts five minutes.

Book/portfolio membership comes from the existing inline `export_portfolio`
operation. Similar names do not establish membership. Trades, books and
portfolios must match the selected tenant/party. Counterparties form an entitled
tenant catalogue; they do not have an invented party owner.

Most catalogue scans have a 2,000-row ceiling. Membership export allows 1,000
trades. Output pages allow 25 rows. Duplicate or inconsistent pages produce an
error. Counterparty name searches cover one labelled catalogue page; use
`nextOffset` to continue. A page cannot prove global absence or uniqueness.
Exact short-code and UUID reads avoid a full counterparty scan.

## Trade details

The tool checks ownership before related reads. It returns booking, counterparty,
book hierarchy and direct portfolio assignments. Those assignments are separate
from the booking hierarchy. Cycles and foreign links are rejected.
The header is read again to detect changes; this is not an atomic snapshot of
all related tables.

FX forwards include bought/sold amounts and currencies, value date and settlement.
Decimal amounts are preserved. Bonds expose issue ID and notional only, labelled
`issue_reference_only`. Missing or unsupported terms are not inferred.
These tools do not value or book a trade.

## Check existing data

```text
node scripts/smoke-discovery.mjs <AGENT_URL> <PRIVATE_CREDENTIALS_JSON> <PARTY_UUID> <OPTIONAL_OTHER_PARTY_UUID>
```

The second-party check uses a separate login. It does not prove low-permission
or cross-tenant security. Historical results are in [validation](validation.md).
