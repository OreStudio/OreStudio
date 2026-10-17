#!/usr/bin/env bash
# Live run: does the claim admit exactly one owner when one step command is
# delivered twice?
#
#   bash .audit/run_duplicate_claim_test.sh
#
# Two phases, because they answer two different questions:
#
#   Phase A -- the step is terminal (completed). The command is published twice
#              with the engine's own headers, and separately twice without the
#              Nats-Msg-Id the engine sets. Shows what the stream's duplicate
#              window does, and that a replay after completion is a no-op.
#
#   Phase B -- the step is put back to in_progress, which is the only state in
#              which a claim is granted. The command is published twice back to
#              back and the claim table is sampled while the work runs.
#
# Evidence lands in .audit/duplicate_claim_test.txt (this script's stdout) plus
# .audit/claim_samples.txt (every distinct claim row seen by the sampler).

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
psql_at() { psql -h "$DB_HOST" -U "$ORES_DB_DDL_USER" -d "$DB_NAME" -At -c "$1"; }

STEP=2003e140-7273-4396-a13f-bc44ecffd8d9
INST=fb05e403-5d92-4078-831d-e2b05faa39a6
TEN=b0973dd8-2eac-49aa-9306-80f9a068eeea
SUBJ="${ORES_NATS_SUBJECT_PREFIX}.reporting.v1.ops.gather_trades"
IN_PROGRESS=9e3c37a5-08f2-452e-b5aa-7730c54f507e
PAYLOAD='{"report_instance_id":"29f70300-2e4b-44ca-87c2-dfd433d32bc6","definition_id":"26c90761-a658-47c5-9944-f103410a3e2e","tenant_id":"b0973dd8-2eac-49aa-9306-80f9a068eeea","correlation_id":"29f70300-2e4b-44ca-87c2-dfd433d32bc6"}'

HEADERS_COMMON=(--header "X-Workflow-Step-Id:$STEP"
                --header "X-Workflow-Instance-Id:$INST"
                --header "X-Tenant-Id:$TEN")

publish_engine_style() {
    # exactly the headers workflow_engine::publish_command sets
    "$NATS_BIN" "${NATS_ARGS[@]}" pub "${HEADERS_COMMON[@]}" \
        --header "Nats-Msg-Id:$STEP" \
        --header "Nats-Correlation-Id:$STEP" "$SUBJ" "$PAYLOAD"
}

publish_without_dedup_header() {
    # the same command, with the one header that lets the stream deduplicate it
    # left off, so the server stores and delivers two messages
    "$NATS_BIN" "${NATS_ARGS[@]}" pub "${HEADERS_COMMON[@]}" "$SUBJ" "$PAYLOAD"
}

claims_now() {
    psql_at "select coalesce(string_agg(step_id::text||' owner='||owner||' attempts='||attempts, ' ; '), '(none)') from ores_workflow_step_claims_tbl where step_id='$STEP'::uuid;"
}

stream_messages() {
    "$NATS_BIN" "${NATS_ARGS[@]}" stream info "${ORES_NATS_SUBJECT_PREFIX//./_}_reporting_execution_steps" --json 2>/dev/null |
        python3 -c 'import json,sys; d=json.load(sys.stdin); print(d["state"]["messages"])'
}

set_step_state() {
    psql_at "update ores_workflow_workflow_steps_tbl set state_id='$1'::uuid where id='$STEP'::uuid and valid_to=ores_utility_infinity_timestamp_fn(); select state_id from ores_workflow_workflow_steps_tbl where id='$STEP'::uuid and valid_to=ores_utility_infinity_timestamp_fn();"
}

state_now() {
    psql_at "select st.name from ores_workflow_workflow_steps_tbl s join ores_dq_fsm_states_tbl st on st.id=s.state_id where s.id='$STEP'::uuid and s.valid_to=ores_utility_infinity_timestamp_fn();"
}

echo "=== duplicate-claim live run ==="
date -u +'%Y-%m-%dT%H:%M:%SZ'
echo "step   : $STEP ($(state_now))"
echo "subject: $SUBJ"

echo
echo "--- phase A: the step is terminal, publish twice with the engine's headers ---"
echo "stream messages before: $(stream_messages)"
echo "claims before: $(claims_now)"
publish_engine_style
publish_engine_style
sleep 3
echo "stream messages after two identical publishes: $(stream_messages)"
echo "(one stored message means the stream's 2m Duplicate Window dropped the second)"
echo "claims after: $(claims_now)"

echo
echo "--- phase A2: the same command twice without Nats-Msg-Id ---"
echo "stream messages before: $(stream_messages)"
publish_without_dedup_header
publish_without_dedup_header
sleep 4
echo "stream messages after: $(stream_messages)"
echo "claims after: $(claims_now)"

echo
echo "--- phase B: put the step back to in_progress, then publish twice ---"
echo "state: $(state_now)"
set_step_state "$IN_PROGRESS" >/dev/null
echo "state now: $(state_now)"
echo "claims before: $(claims_now)"

# Sample the claim table while the work runs, so a row that exists only for the
# duration of the step is still caught.
SAMPLES="$ROOT/.audit/claim_samples.txt"
: >"$SAMPLES"
(
    for _ in $(seq 1 120); do
        printf '%s|%s\n' "$(date -u +'%H:%M:%S.%3N')" "$(claims_now)" >>"$SAMPLES"
        sleep 0.25
    done
) &
SAMPLER=$!

publish_engine_style
publish_without_dedup_header
echo "published at $(date -u +'%H:%M:%S.%3N')"

wait "$SAMPLER"
echo
echo "--- distinct claim rows seen by the sampler ---"
sort -u -t'|' -k2 "$SAMPLES"
echo
echo "--- state and claims after ---"
echo "state: $(state_now)"
echo "claims: $(claims_now)"
echo "stream messages: $(stream_messages)"