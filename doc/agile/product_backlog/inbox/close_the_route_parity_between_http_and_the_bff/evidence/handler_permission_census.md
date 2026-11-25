# Census: the messaging handlers that check no permission

Measured 2026-11-25 at `9cf96d7deb`, over `projects/ores.*/**/messaging/*_handler.hpp`:

- **456** handlers in the tree.
- **408** call `has_permission`.
- **48** do not, across 17 components.

The task this file belongs to named two of them — `ores.scheduler`'s
`job_instance_handler` and `scheduler_status_handler` — and both now check a
permission, so that part of the capture is stale. The hole it describes is
24 times bigger than the capture says.

## What the gate does today, and what it cannot see

compass-ng's `handler_permissions` codec reads a `components` option (ten
components by bare name, `refdata,analytics,compute,assets,storage,reporting,dq,trading,marketdata,inbox`)
and scans those components' `**/messaging/*_handler.hpp|.cpp`. It checks one
direction only: **every permission code a scanned handler checks is seeded** in
`projects/ores.sql/populate/iam/iam_permissions_populate.sql`.

So a handler that checks nothing passes, and a handler in a component outside
the list is never read. Both halves are the compass codec's, not this
repository's, which is why the enforcement half of this task waits on a compass
release. The handover beside this file states the change.

## The 48, classified by what they are

The classification is a proposal for the sweep: each row either gains a check on
a named code, or states why it needs none. It is not yet decided for the two
groups marked *decide*, because each new code needs a seed and a role grant, and
a wrong code breaks a working screen.

| Kind | Handlers | Proposed decision |
|------|----------|-------------------|
| DQ publish consumer (`publish_from_dq_handler.hpp`, 13 components) | analytics, assets, dq, iam, marketdata, ore, refdata, reporting, synthetic, trading | Invoked by the DQ service after a rule run, not by a person. State *no person, no person's code*, and check that the caller is the DQ service's identity |
| Journey step (`provisioning_step`, `apply_step`, `identity_step`) | refdata ×3, synthetic, marketdata, inbox, reporting, workflow | Runs inside a person's provisioning session. **Decide** the code each step's permission states |
| Compute worker (`work`, `claim`, `result_submit`, `report_submit`, `run_files`) | compute ×5 | The compute wrapper's own identity. **Decide** one worker code, checked once per handler |
| Before a session exists (`auth`, `bootstrap`, `tenant_session`) | iam ×3 | Sign-in, first-run setup and the tenant's session: no session to check. State why, per handler |
| Service to service (`telemetry_handler`, `objects_handler`, `history_handler`) | telemetry, storage, history | **Decide** a service code, or state the transport's own identity |
| The server's own (`http_info_handler`) | http server | The health and info read: public by design. State why |
| Document and engine handlers | ore ×5 (`currency_config`, `ore_import`, `ore_import_execute`, `pricing_engines`, `report_package`, `run_configuration`), refdata ×2 (`calendar_adjustment`, `calendar_materialisation`), refdata/analytics `configuration_document`, reporting ×3 (`run_document`, `report_execution`, `report_instance_trigger`), iam `run_grant_operations`, `agent_grant_operations` | **The likely real holes**: person-facing reads and writes that simply never checked. Each names the code its route or screen already asks for |

## The HTTP side

The router has `auth_required()` and no permission hook, so a generated route
authenticates and leaves the authority to the service behind it. The
hand-written route files are the gap: only `iam_routes.cpp` checks a code, and
`risk`, `variability` and `assets` authenticate and stop. That half is fixed
inside this repository once each route's code is decided; it does not wait on
compass.

## Reproduce

```sh
for f in $(find projects -path "*/messaging/*_handler.hpp"); do
  grep -q has_permission "$f" || echo "$f"
done
```

## The registry can be widened today, and it is not a small change

The `components` option is a codec option, so `.compass.org` can widen it without
a compass release. It was tried, by naming every component that has handlers:

```
| handler_permissions | yes | components=analytics,assets,compute,dq,history,http,iam,inbox,marketdata,ore,refdata,reporting,scheduler,storage,synthetic,telemetry,trading,variability,workflow |
```

`compass-ng check --codec handler_permissions` then reports **102 unseeded codes**:

| Component | Codes checked but never seeded |
|-----------|-------------------------------|
| synthetic | 66 |
| iam | 20 |
| workflow | 12 |
| variability | 4 |

That is the reason the registry names ten components: the other nine check codes
their seeds have never held. So widening it is not a configuration change but a
data change — 102 codes to seed in `iam_permissions_populate.sql`, and each one
granted to the roles that need it — and the option was reverted until those
seeds land, so that no branch is red for a gap that predates it.

The codes are already named by the handlers (for example
`synthetic::calibration_fallbacks:write`, `iam::tenant_types:delete`), so seeding
them is mechanical. **Granting** them is not: which role may write a synthetic
calibration policy, or delete a tenant type, is a product decision per code.
