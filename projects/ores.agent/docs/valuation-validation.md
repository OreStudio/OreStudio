# Historical NPV test-data preparation

Prepared 2026-11-06; reconciled 2026-11-07. This is an audit record, not setup
instructions. The `cc917e0d36` reporting addition and scope-copy helper were
later reverted. No current tool/evaluation calls the removed operation.
Existing stored results remain. Fresh preparation needs separate agreement.
See the [removal record](architecture-future-prs.md#historical-backend-addition-excluded-from-this-branchs-changes).

Agent at the check: `0.1.3-5b23d543cb79`. Backend read reference:
`fc4f559dfb187a368bf2a8cd4053d8e833348835`.

## Records and attempts

| Record | Identity |
| --- | --- |
| Party | Acme Corporation Plc, `b29db53d-325b-407f-9c47-8d229e4a111b` |
| Source definition | Leverage Ratio, `88d5b458-fb96-465e-9281-cb41ffe8a082` |
| New definition | Agent NPV validation 39eb7b57, `39eb7b57-db88-4669-ae7e-fbe3dbf40016` |
| First attempt, failed before pricing | `5d613507-83a8-44d7-9992-5517c60ff986` |
| Corrected, completed-clean run | `884d0bc9-932d-4c4c-9b66-ee97d2b810bb` |
| Corrected run's workflow | `79f8af60-e883-47d7-8726-b2503af29374` |
| Trade to reconcile | `05dcc4f0-4af9-597f-8e20-94c3018c6b4c` |

The source Leverage Ratio run `bdfd79f3-0089-4512-8fb6-fd8a1e783291` had eleven
errors: ten Bond SA-CCR errors and one missing netting-set definition. Its rows
were partial. The inspected failed HK run `b78e44a9-874b-4593-9e99-e3cad2c2de2a`
was not a Plc validation run.

The operator created an unscheduled NPV-only definition. It used copied settings,
GBP base currency, market date 2026-11-06, one thread and `continue_on_error=false`.
Original bindings stayed unchanged. Manual triggers used `job_instance_id=0`.

The first attempt failed because run-document import did not copy book/portfolio
scope. A temporary reporting build added `copy_run_scope`. It checked ownership,
write permission and an empty, unshared destination, then copied both association
sets atomically. Invalid/repeated copies were refused. The checked portfolio was
`e781f889-96ed-4553-96be-e12b00356075`.

Only five reporting compilation units and a separate DLL were built. A temporary
service tested the operation and stopped before the corrected run. The original
fleet then gathered 73 trades from two books and completed normal packaging,
engine execution and collection. No main-fleet restart or broker change occurred.
The addition was not permanently deployed and has since been removed from the branch.

## Reconciled result

The corrected run completed at `2026-11-06 21:43:39.214032 UTC`, health
`completed clean`, zero errors and one warning. Gathering had also omitted stale
observations. Clean health did not certify the market data.

The trade's NPV was **GBP -7,998,696.427339**, priced at **2026-11-06**, source
`npv.csv`. Direct NATS, independent MCP and GPU chat agreed. The HK party could
not read the Plc result. Earlier errored runs were not relabelled.

48 Windows tests passed; the unchanged agent had already passed 48 Linux tests
and packaging. The 5,327 ms chat sample was not a benchmark. This historical
native-NPV result does not resolve today's separate base-NPV unit rejection.

Read-only scripts remain available:

```text
node scripts/smoke-clean-reporting.mjs <AGENT_URL> <CONFIG> <CREDENTIALS> <PARTY> <TRADE> <RUN> <OPTIONAL_OTHER_PARTY>
node scripts/smoke-reporting.mjs <AGENT_URL> <CREDENTIALS> <PARTY> <OTHER_PARTY> <CLEAN_RUN>
```

These scripts do not create reports. Stored data ages; stale labels are expected.

## Other contract findings

`report_configurations.put_many` looked up `must_not_exist` by configuration type,
so it refused fresh binding IDs. No rows were written by that refused batch.
The bounded setup used fresh UUIDs with `any`, then read back the exact bindings.
This was not proof that general report editing was correct.

The deployed `configuration_analytic_parameters.list` had no responder. The
composite run-document read supplied the parameters. No low-level handler,
HTTP fallback or SQL agent access was added for that finding.

Private evidence: ignored `build/output/npv-*`. Historical operator scripts are
not repeatable migrations. This record does not authorise replaying their writes.
