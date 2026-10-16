#!/usr/bin/env bash
# Starts a SECOND ores.reporting.service process with exactly the argv and
# environment the systemd unit uses, so the step-command durable consumer has
# two members in its queue group. Used by the atomic-step-claim live run.
#
#   .audit/start_second_reporting_replica.sh
#
# The pid is written to .audit/second_reporting_replica.pid and the output to
# .audit/second_reporting_replica.log.
set -euo pipefail

ROOT=/home/marco/Development/OreStudio/ores_dev_brave_hopper
cd "$ROOT"

set -a
# shellcheck disable=SC1091
. "$ROOT/.env"
set +a

BIN="$ROOT/build/output/${ORES_PRESET}/publish/bin"
PIDFILE="$ROOT/.audit/second_reporting_replica.pid"
LOGFILE="$ROOT/.audit/second_reporting_replica.log"

if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
    echo "already running: pid $(cat "$PIDFILE")"
    exit 0
fi

cd "$BIN"
setsid nohup ./ores.reporting.service \
    --log-enabled --log-level info --log-to-console \
    --nats-tls-ca "$ORES_NATS_TLS_CA" \
    --nats-tls-cert "$ROOT/build/keys/nats/ores.reporting.service.crt" \
    --nats-tls-key "$ROOT/build/keys/nats/ores.reporting.service.key" \
    --http-base-url "http://localhost:${ORES_HTTP_PORT}" \
    >"$LOGFILE" 2>&1 </dev/null &

echo $! >"$PIDFILE"
echo "started second replica: pid $(cat "$PIDFILE")"
