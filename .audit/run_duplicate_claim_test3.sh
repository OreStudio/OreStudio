#!/usr/bin/env bash
# Live run, take 3: the duplicate the claim has to survive.
#
#   bash .audit/run_duplicate_claim_test3.sh
#
# Takes 1 and 2 were both contaminated by the stream, and this script is what
# the contamination taught:
#
#   * The engine publishes a step command with `Nats-Msg-Id = <step id>` (see
#     workflow_engine::publish_command) and the stream carries a 2m Duplicate
#     Window. So a second copy that carries that header, published within two
#     minutes of the first, is dropped by the server and never delivered. That
#     is why take 1 and take 2 each produced only one delivery.
#   * The same header on the step-completed event means a second completion for
#     one step inside two minutes is dropped as well, so the engine never hears
#     it and the claim is never released.
#
# So both copies here go without the dedup header, which is the only way to get
# two stored messages for one step. Everything else is the engine's headers.

set -uo pipefail

ROOT=/home/marco/Development/OreStudio/ores_dev_brave_hopper
cd "$ROOT"
set -a
# shellcheck disable=SC1091
. "$ROOT/.env"
set +a

K="$ROOT/build/keys/nats"
NATS_BIN=/usr/local/bin/nats
NATS_ARGS=(--server "$ORES_NATS_URL" --tlsca "$K/ca.crt"
           --tlscert "$K/ores.reporting.service.crt" --tlskey "$K/ores.reporting.service.key")

export PGPASSWORD="$ORES_DB_DDL_PASSWORD"
DB_NAME="${ORES_DATABASE_NAME:-ores_dev_brave_hopper}"
DB_HOST="${ORES_DB_HOST:-localhost}"
PSQL=(psql -h "$DB_HOST" -U "$ORES_DB_DDL_USER" -d "$DB_NAME" -At -q)
psql_at() { "${PSQL[@]}" -c "$1"; }

STEP=2003e140-7273-4396-a13f-bc44ecffd8d9
INST=fb05e403-5d92-4078-831d-e2b05faa39a6
TEN=b0973dd8-2eac-49aa-9306-80f9a068eeea
SUBJ="${ORES_NATS_SUBJECT_PREFIX}.reporting.v1.ops.gather_trades"
IN_PROGRESS=9e3c37a5-08f2-452e-b5aa-7730c54f507e
FAILED_ID=85b350d6-fbe6-43a2-9f64-5863130f2fa3
PAYLOAD='{"report_instance_id":"29f70300-2e4b-44ca-87c2-dfd433d32bc6","definition_id":"26c90761-a658-47c5-9944-f103410a3e2e","tenant_id":"b0973dd8-2eac-49aa-9306-80f9a068eeea","correlation_id":"29f70300-2e4b-44ca-87c2-dfd433d32bc6"}'
HEADERS=(--header "X-Workflow-Step-Id:$STEP"
         --header "X-Workflow-Instance-Id:$INST"
         --header "X-Tenant-Id:$TEN")

state_now() {
    psql_at "select st.name from ores_workflow_workflow_steps_tbl s join ores_dq_fsm_states_tbl st on st.id=s.state_id where s.id='$STEP'::uuid and s.valid_to=ores_utility_infinity_timestamp_fn();"
}

echo "=== duplicate-claim live run, take 3 ==="
date -u +'%Y-%m-%dT%H:%M:%SZ'
echo "step state at start: $(state_now)"

# The runs before this one left a claim behind that the engine never released,
# because the completion it was waiting for was deduplicated. Clear it.
psql_at "delete from ores_workflow_step_claims_tbl where step_id='$STEP'::uuid;" >/dev/null
psql_at "drop table if exists ores_audit_claim_samples;" >/dev/null
psql_at "create table ores_audit_claim_samples(sampled_at timestamptz, step_id uuid, owner text, attempts int);" >/dev/null

echo
echo "--- put the step back to in_progress ---"
psql_at "update ores_workflow_workflow_steps_tbl set state_id='$IN_PROGRESS'::uuid, command_published_at=now() where id='$STEP'::uuid and valid_to=ores_utility_infinity_timestamp_fn();" >/dev/null
echo "state: $(state_now)"

sampler_sql="
do \$\$
declare t0 timestamptz := clock_timestamp();
begin
  while clock_timestamp() - t0 < interval '25 seconds' loop
    insert into ores_audit_claim_samples
      select clock_timestamp(), step_id, owner, attempts from ores_workflow_step_claims_tbl;
    perform pg_sleep(0.01);
  end loop;
end \$\$;"

echo
echo "--- two copies, neither carrying Nats-Msg-Id, published back to back ---"
"${PSQL[@]}" -c "$sampler_sql" >/dev/null &
SAMPLER=$!
sleep 1
"$NATS_BIN" "${NATS_ARGS[@]}" pub "${HEADERS[@]}" "$SUBJ" "$PAYLOAD" >/dev/null
"$NATS_BIN" "${NATS_ARGS[@]}" pub "${HEADERS[@]}" "$SUBJ" "$PAYLOAD" >/dev/null
echo "both published at $(date -u +'%H:%M:%S.%3N')"
wait "$SAMPLER"
echo "sampler done at $(date -u +'%H:%M:%S.%3N')"

echo
echo "--- claim rows the sampler saw ---"
"${PSQL[@]}" -c "select min(sampled_at)::time(3) as first_seen, max(sampled_at)::time(3) as last_seen, owner, attempts, count(*) as samples from ores_audit_claim_samples group by owner, attempts order by first_seen;"
echo
echo "--- the assertions ---"
"${PSQL[@]}" -c "select count(distinct owner) as distinct_owners, coalesce(max(attempts),0) as max_attempts, count(*) as samples from ores_audit_claim_samples;"
echo
echo "state after: $(state_now)"
echo "claim rows left: $(psql_at "select count(*) from ores_workflow_step_claims_tbl;")"
