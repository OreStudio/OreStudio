#!/usr/bin/env bash
# Answers one question: when two copies of one step command are published, how
# many of them does the consumer actually deliver?
#
#   bash .audit/run_delivery_count_test.sh
#
# The claim table cannot tell the difference between "the loser was refused" and
# "there was no loser". The consumer's own delivered-sequence numbers can, so
# they are read before and after.

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
PSQL=(psql -h "${ORES_DB_HOST:-localhost}" -U "$ORES_DB_DDL_USER" -d "${ORES_DATABASE_NAME:-ores_dev_brave_hopper}" -At -q)
psql_at() { "${PSQL[@]}" -c "$1"; }

STEP=2003e140-7273-4396-a13f-bc44ecffd8d9
INST=fb05e403-5d92-4078-831d-e2b05faa39a6
TEN=b0973dd8-2eac-49aa-9306-80f9a068eeea
SUBJ="${ORES_NATS_SUBJECT_PREFIX}.reporting.v1.ops.gather_trades"
IN_PROGRESS=9e3c37a5-08f2-452e-b5aa-7730c54f507e
PAYLOAD='{"report_instance_id":"29f70300-2e4b-44ca-87c2-dfd433d32bc6","definition_id":"26c90761-a658-47c5-9944-f103410a3e2e","tenant_id":"b0973dd8-2eac-49aa-9306-80f9a068eeea","correlation_id":"29f70300-2e4b-44ca-87c2-dfd433d32bc6"}'
HEADERS=(--header "X-Workflow-Step-Id:$STEP"
         --header "X-Workflow-Instance-Id:$INST"
         --header "X-Tenant-Id:$TEN")

delivery_counters() {
    "$NATS_BIN" "${NATS_ARGS[@]}" consumer info \
        "${ORES_NATS_SUBJECT_PREFIX//./_}_reporting_execution_steps" \
        "${ORES_NATS_SUBJECT_PREFIX//./_}_reporting_execution_steps_consumer" --json 2>/dev/null |
        python3 -c 'import json,sys; d=json.load(sys.stdin); print("delivered="+str(d["delivered"]["consumer_seq"]), "ack_floor="+str(d["ack_floor"]["consumer_seq"]), "redelivered="+str(d.get("num_redelivered",0)))'
}

echo "=== delivery-count test ==="
date -u +'%Y-%m-%dT%H:%M:%SZ'

psql_at "delete from ores_workflow_step_claims_tbl where step_id='$STEP'::uuid;" >/dev/null
psql_at "update ores_workflow_workflow_steps_tbl set state_id='$IN_PROGRESS'::uuid where id='$STEP'::uuid and valid_to=ores_utility_infinity_timestamp_fn();" >/dev/null
echo "step state: $(psql_at "select st.name from ores_workflow_workflow_steps_tbl s join ores_dq_fsm_states_tbl st on st.id=s.state_id where s.id='$STEP'::uuid and s.valid_to=ores_utility_infinity_timestamp_fn();")"
echo "before: $(delivery_counters)"

echo
"$NATS_BIN" "${NATS_ARGS[@]}" pub "${HEADERS[@]}" "$SUBJ" "$PAYLOAD" >/dev/null
"$NATS_BIN" "${NATS_ARGS[@]}" pub "${HEADERS[@]}" "$SUBJ" "$PAYLOAD" >/dev/null
echo "two copies published at $(date -u +'%H:%M:%S.%3N')"

sleep 6
echo "after : $(delivery_counters)"
echo
echo "step state: $(psql_at "select st.name from ores_workflow_workflow_steps_tbl s join ores_dq_fsm_states_tbl st on st.id=s.state_id where s.id='$STEP'::uuid and s.valid_to=ores_utility_infinity_timestamp_fn();")"
echo "claims left: $(psql_at "select count(*) from ores_workflow_step_claims_tbl where step_id='$STEP'::uuid;")"
echo
echo "--- journal for this step in the window ---"
journalctl --user -u ores.reporting.service-brave_hopper --since "-30 seconds" --no-pager 2>/dev/null | grep -c "gather_trades starting" | sed 's/^/systemd replica handler runs: /'
journalctl --user -u ores.reporting.service-brave_hopper --since "-30 seconds" --no-pager 2>/dev/null | grep -c "is held by another delivery" | sed 's/^/systemd replica busy answers: /'
grep -c "gather_trades starting" "$ROOT/.audit/second_reporting_replica.log" | sed 's/^/second replica handler runs (whole log): /'
grep -c "is held by another delivery" "$ROOT/.audit/second_reporting_replica.log" | sed 's/^/second replica busy answers (whole log): /'
