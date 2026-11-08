# Historical business-data baseline

Historical inspection: 2026-11-06, 20:33 UTC. This is not a current inventory.
Use [validation](validation.md) for later results.

Backend reference: `fc4f559dfb187a368bf2a8cd4053d8e833348835`.
Account: existing Acme tenant administrator. Party: Acme Corporation Plc,
`b29db53d-325b-407f-9c47-8d229e4a111b`. This tested that account's reads,
not low-permission or cross-tenant policy.

## Recorded findings

| Resource | Selected-party records | Read status |
| --- | ---: | --- |
| Market series | 71 | Successful, complete within ceiling |
| Trades | 73 | Successful; Bond and FxForward headers |
| Books | 2 | Successful |
| Portfolios | 3 | Successful |
| Report definitions | 25 | Successful |
| Report instances | 61 | Successful |
| Stored run-result rows | 584 | Successful, party-filtered |

The 73 trades included 63 FX forwards and ten bonds. Run health: four clean,
two completed with errors, 25 failed and 30 running. These counts can change.
Stored measures included `npv`, `npv_base`, `notional` and `Notional(Base)`.
They did not prove usable Greeks, units or safe totals.

The initial clean-NPV gate was open. A separate report later passed the
[historical NPV check](valuation-validation.md). The current trade base-NPV
unit failure is a separate finding. Positive live IR remains unverified.

## Repeat the read-only inspection

```text
npm run build
node scripts/inspect-business.mjs <CONFIG> <PRIVATE_CREDENTIALS> <PARTY_UUID> <PRIVATE_OUTPUT_JSON>
```

The probe allows existing reads and normal login/logout only. It does not
start reports or prepare data. It scans at most 2,000 rows per resource,
100 run-health records and 50 IR series. Limits cannot prove global absence.
Keep output private; it contains business records and identities.

At this checkpoint, 28 tests passed on Windows and pinned Linux. Existing
services were started for inspection. No rebuild, database recreation or queue
purge occurred in this stage. Normal scheduler activity was separate.
Raw evidence: ignored `build/output/agent-business-baseline.json` and
`agent-business-linux-check.log`.
