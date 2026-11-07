# Isolated live NPV validation

Setup/run: 2026-11-06; reconciliation: 2026-11-07. Uses the installed
`0.1.3-5b23d543cb79` agent and its existing read contract
`fc4f559dfb187a368bf2a8cd4053d8e833348835`. A separately validated backend
scope-copy addition was needed for operator setup, as described below.
Backend prerequisite commit: `cc917e0d36`.

## Diagnosis and setup

The Acme Plc Leverage Ratio run
`bdfd79f3-0089-4512-8fb6-fd8a1e783291` has eleven error events: ten unsupported
Bond SA-CCR trade-data errors and a missing netting-set definition. Its NPV
rows are partial output, even though those individual rows exist. The other
inspected failed run `b78e44a9-874b-4593-9e99-e3cad2c2de2a` belongs to Acme HK
and contains malformed product/XML errors; it is not a Plc validation run.

The operator explicitly created a separate local report through existing NATS
operations. This is test-data preparation, independent of agent/MCP reads.

| Record | Identity |
| --- | --- |
| Party | Acme Corporation Plc, `b29db53d-325b-407f-9c47-8d229e4a111b` |
| Source definition | Leverage Ratio, `88d5b458-fb96-465e-9281-cb41ffe8a082` |
| New definition | Agent NPV validation 39eb7b57, `39eb7b57-db88-4669-ae7e-fbe3dbf40016` |
| First attempt, failed before pricing | `5d613507-83a8-44d7-9992-5517c60ff986` |
| Corrected, completed-clean run | `884d0bc9-932d-4c4c-9b66-ee97d2b810bb` |
| Corrected run's workflow | `79f8af60-e883-47d7-8726-b2503af29374` |
| Trade to reconcile | `05dcc4f0-4af9-597f-8e20-94c3018c6b4c` |

The source run document was read with `reporting.v1.run_documents.get`. The new
definition has no schedule, scheduler job, resolved cut or official/regulatory
designation. `reporting.v1.run_documents.put` copied its run settings, market
bindings and engine settings into new configurations, retaining only the active
`npv` analytic and its original parameters. It requests GBP base currency and
the explicit market-data date 2026-11-06, one engine thread, and
`continue_on_error=false`.

Four new binding UUIDs reference the existing today's-market, conventions,
curve-configuration and pricing-engine configurations. The saved document was
read back to verify its owner, single analytic and all six bindings. The
original report's bindings were compared with the pre-write snapshot and were
unchanged. Manual production runs were requested with
`reporting.v1.ops.trigger_report_instance`; the contract explicitly uses
`job_instance_id=0` for manual execution.

This uses normal trade gathering, packaging, engine execution and result
collection. No stored output was fabricated or relabelled clean. The agent
remains read-only; these operator commands are not exposed as chat/MCP tools.

The first attempt failed because run-document import does not copy its
separately stored book/portfolio scope. That failure remains recorded. There
was no deployed NATS scope-copy operation. After declaring this prerequisite,
the backend gained `reporting.v1.ops.copy_run_scope`. It requires
`reporting::run_documents:write`, a selected party that owns both definitions,
an existing nonempty source and an empty, unshared destination configuration.
Both association sets are copied in one database statement. Same-configuration,
wrong-party, populated/shared destination and repeated copies are refused.
The client supplies definition UUIDs, not SQL or an arbitrary list of books.

The repository's authoritative Org paste blocks were updated and regenerated.
Only five reporting compilation units and the reporting-core DLL were rebuilt,
using two compiler jobs. A private Ninja build variant retained existing version
metadata and shared dependencies, writing the DLL to `validation-bin` rather
than replacing a loaded DLL. This is a local validation build, not a new full
backend release. A temporary reporting instance loaded that DLL alongside the
original fleet. The live `copy-validation-scope.mjs` integration check verified
authentication, party ownership and refusal of same/populated/shared/replayed
copies, then copied portfolio `e781f889-96ed-4553-96be-e12b00356075`.

The temporary instance was stopped before the corrected run. The original
fleet gathered **73 trades from two books**, packaged and executed the report,
and collected its results. No main-fleet restart, broker configuration/ACL
change, queue purge, database recreation or agent deployment was performed.
The new operator subject is in source and was tested in the temporary instance;
the original running fleet has not been upgraded to serve it permanently.
Native Linux validation of this backend addition remains outstanding.

## Execution and reconciliation

The clean live single-trade acceptance gate **passed**. The corrected run
completed at **2026-11-06 21:43:39.214032 UTC**, with backend health
`completed clean`. Its engine log recorded zero errors and one warning. This
health word does not assert a warning-free run or externally certified market
data; gathering also reported stale observations that it omitted.

Reconciled NPV: **GBP -7,998,696.427339**, actual pricing date **2026-11-06**,
source **`npv.csv`**. The NATS row, external MCP result and deployed chat answer
agree on the figure, currency, dates and identities. The Acme HK login cannot
read this Plc trade's result. The earlier errored NPV is a different stored
run and is not reused or relabelled.

On 2026-11-07 the clean smoke passed, including a real GPU-backed NPV question
(5,327 ms sample). The reporting smoke also passed its clean default, withheld
errored-run, explicit partial and second-party checks (NPV 5,258 ms, status
1,972 ms, direct partial inspection 1,782 ms). These are smoke samples, not
benchmarks. The existing 48 Windows agent tests passed during this checkpoint;
the unchanged release had already passed the same 48 tests and packaging on
Linux. No new agent artifact is needed for these operator scripts/data. Native
backend/Linux and least-privilege/cross-tenant validation are not claimed.

From `projects/ores.agent`, while the stored pricing date is within the normal
freshness window:

```text
node scripts/smoke-clean-reporting.mjs <agent-url> <agent-config.json> <private-credentials.json> <party-id> <trade-id> <run-id> <optional-other-party-id>
node scripts/smoke-reporting.mjs <agent-url> <private-credentials.json> <party-id> <other-party-id> <clean-run-id>
```

The first script independently reads the exact stored NPV over authenticated
NATS, requires backend health `completed clean`, and reconciles figure,
currency, date, source and identities against the deployed external MCP result.
It also checks a real chat question and optional second-party isolation. It
never creates or triggers a report. The second keeps the original errored run
withheld by default and verifies explicit partial inspection alongside the new
default clean-result path. Both smoke scripts are read-only. As this stored run
ages, normal undated requests will correctly mark it stale; a current-price
demonstration then needs a separately requested fresh run.

## Backend contract observations

The generated `report_configurations.put_many` create check looks up existing
rows by `configuration_type_code` alone. Consequently `must_not_exist` refused
the four fresh binding UUIDs because other reports already use those types.
The refused batch wrote nothing. For this bounded operator setup, fresh binding
UUIDs were written with the supported `any` precondition, whose repository
resolves versions by storage UUID, and the exact definition bindings were read
back. The original source bindings stayed unchanged. Do not treat the generated
natural-key lookup/readback as correct for general report editing; address its
definition/type identity before exposing such editing commands to the agent.

The low-level `configuration_analytic_parameters.list` subject has no responder
in the deployed service. The supported composite run-document read supplies
those parameters, so no handler was added for that low-level subject. This is
separate from the necessary new scope-copy handler above. Neither observation
was worked around by changing a broker ACL or introducing HTTP/SQL agent access.

Local raw evidence and operator setup scripts are retained privately under
ignored `build/output/npv-*`; credentials/tokens are not committed. The setup is
not an automatically rerunnable migration. Inspect its checkpoint before any
retry to avoid duplicate report execution.
