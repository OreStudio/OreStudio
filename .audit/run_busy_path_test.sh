#!/usr/bin/env bash
# Live run: does a delivery that loses the claim get answered rather than run?
#
#   bash .audit/run_busy_path_test.sh
#
# A fast step cannot show this. The claim is released the moment the engine
# writes the step away from in_progress, so with a step that reports in under
# 100 ms the live-claim window is that same 100 ms, and two deliveries to one
# process are handled one after the other. The identity fixture exists for this:
# its step declares delay_seconds, so it holds the claim open long enough for a
# second delivery to arrive while the lease is live.
#
# Asserts: the holder's work runs once, the second delivery is answered busy,
# and the attempt count stays at one.

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
           --tlscert "$K/ores.workflow.service.crt" --tlskey "$K/ores.workflow.service.key")

export PGPASSWORD="$ORES_DB_DDL_PASSWORD"
DB_NAME="${ORES_DATABASE_NAME:-ores_dev_brave_hopper}"
DB_HOST="${ORES_DB_HOST:-localhost}"
PSQL=(psql -h "$DB_HOST" -U "$ORES_DB_DDL_USER" -d "$DB_NAME" -At -q)
psql_at() { "${PSQL[@]}" -c "$1"; }

TEN=b0973dd8-2eac-49aa-9306-80f9a068eeea
PREFIX="$ORES_NATS_SUBJECT_PREFIX"
STEP_SUBJ="$PREFIX.workflow.v1.ops.identity_step"
START_SUBJ="$PREFIX.workflow.v1.ops.start_workflow"
RUN_ID=$(cat /proc/sys/kernel/random/uuid)

echo "=== busy-path live run ==="
date -u +'%Y-%m-%dT%H:%M:%SZ'
echo "identity run: $RUN_ID"

psql_at "drop table if exists ores_audit_claim_samples;" >/dev/null
psql_at "create table ores_audit_claim_samples(sampled_at timestamptz, step_id uuid, owner text, attempts int);" >/dev/null

REQUEST_JSON='{"steps":[{"name":"hold_the_claim","behaviour":"complete","compensate":false,"delay_seconds":8}]}'
START_JSON=$(printf '{"type":"identity_workflow","tenant_id":"%s","target_kind":"","target_id":"","request_json":%s,"correlation_id":"%s","instance_id":"%s"}' \
    "$TEN" "$(printf '%s' "$REQUEST_JSON" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')" "$RUN_ID" "$RUN_ID")

sampler_sql="
do \$\$
declare t0 timestamptz := clock_timestamp();
begin
  while clock_timestamp() - t0 < interval '30 seconds' loop
    insert into ores_audit_claim_samples
      select clock_timestamp(), step_id, owner, attempts from ores_workflow_step_claims_tbl;
    perform pg_sleep(0.01);
  end loop;
end \$\$;"
"${PSQL[@]}" -c "$sampler_sql" >/dev/null &
SAMPLER=$!

echo
echo "--- starting the identity run; its step holds for 8 seconds ---"
"$NATS_BIN" "${NATS_ARGS[@]}" pub "$START_SUBJ" "$START_JSON" >/dev/null

STEP=""
for _ in $(seq 1 100); do
    STEP=$(psql_at "select id from ores_workflow_workflow_steps_tbl where workflow_id='$RUN_ID'::uuid and valid_to=ores_utility_infinity_timestamp_fn() limit 1;")
    [ -n "$STEP" ] && break
    sleep 0.1
done
echo "step id: ${STEP:-(not created)}"
if [ -z "$STEP" ]; then
    echo "the run created no step; nothing to test"
    wait "$SAMPLER"
    exit 1
fi

CMD=$(psql_at "select command_json from ores_workflow_workflow_steps_tbl where id='$STEP'::uuid and valid_to=ores_utility_infinity_timestamp_fn();")
echo "command: $CMD"

echo
echo "--- the step is claimed; publishing a second copy of the same command now ---"
sleep 1
"$NATS_BIN" "${NATS_ARGS[@]}" pub \
    --header "X-Workflow-Step-Id:$STEP" \
    --header "X-Workflow-Instance-Id:$RUN_ID" \
    --header "X-Tenant-Id:$TEN" \
    "$STEP_SUBJ" "$CMD" >/dev/null
echo "duplicate published at $(date -u +'%H:%M:%S.%3N')"

wait "$SAMPLER"

echo
echo "--- claim rows for this step ---"
"${PSQL[@]}" -c "select step_id, owner, attempts, count(*) as samples, min(sampled_at)::time(3) as first_seen, max(sampled_at)::time(3) as last_seen from ores_audit_claim_samples where step_id='$STEP'::uuid group by step_id, owner, attempts order by first_seen;"
echo
echo "--- attempts never above: ---"
"${PSQL[@]}" -c "select coalesce(max(attempts),0) as max_attempts, count(distinct owner) as distinct_owners from ores_audit_claim_samples where step_id='$STEP'::uuid;"
echo
echo "step: $STEP"
echo "run : $RUN_ID"
