#!/usr/bin/env bash

# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

# Start everything the prototype needs and leave it running.
#
# The C++ services are started one per process because each presents its own
# NATS client certificate; `compass services start` installs systemd units
# outside the checkout, which needs privileges this workspace does not have.
#
# Usage:
#   scripts/dev-stack.sh start     start NATS, the services, the BFF and the web server
#   scripts/dev-stack.sh stop      stop everything this script started
#   scripts/dev-stack.sh status    report what is listening
set -euo pipefail

WORKSPACE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUN_DIR="$WORKSPACE/.runtime/pids"
LOG_DIR="$WORKSPACE/.runtime/logs"
NATS_CONF="$WORKSPACE/.runtime/nats/nats.conf"

mkdir -p "$RUN_DIR" "$LOG_DIR"

# Long-lived children get their own process group so a stop cannot leave a
# half-killed service holding a port.
start_one() {
  local name="$1"
  shift
  if [[ -f "$RUN_DIR/$name.pid" ]] && kill -0 "$(cat "$RUN_DIR/$name.pid")" 2>/dev/null; then
    echo "  $name already running"
    return
  fi
  setsid "$@" >"$LOG_DIR/$name.out" 2>&1 &
  echo $! >"$RUN_DIR/$name.pid"
  echo "  $name started (pid $!)"
}

stop_one() {
  local name="$1"
  local pid_file="$RUN_DIR/$name.pid"
  if [[ ! -f "$pid_file" ]]; then
    return
  fi
  local pid
  pid="$(cat "$pid_file")"
  if kill -0 "$pid" 2>/dev/null; then
    # Negative pid targets the whole process group.
    kill -- "-$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
    echo "  $name stopped"
  fi
  rm -f "$pid_file"
}

wait_for_port() {
  local port="$1"
  local label="$2"
  for _ in $(seq 1 60); do
    if (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; then
      exec 3<&- 3>&- || true
      echo "  $label is up on $port"
      return 0
    fi
    sleep 0.5
  done
  echo "  $label did not come up on $port" >&2
  return 1
}

case "${1:-start}" in
  start)
    echo "starting the stack"
    start_one nats nats-server -c "$NATS_CONF"
    wait_for_port 21805 nats

    start_one iam bash "$WORKSPACE/scripts/run-service.sh" iam \
      --tenant ffffffff-ffff-ffff-ffff-ffffffffffff
    # The IAM service loads party summaries from refdata over the bus, and a
    # login that arrives first gets empty party names, so start refdata too.
    start_one refdata bash "$WORKSPACE/scripts/run-service.sh" refdata
    wait_for_port 5432 postgres

    start_one bff npx tsx --env-file="$WORKSPACE/.env" "$WORKSPACE/packages/bff/src/main.ts"
    wait_for_port 21801 bff

    start_one web npx vite --host 127.0.0.1 --port 21802 "$WORKSPACE/packages/web"
    wait_for_port 21802 web

    echo
    echo "open http://127.0.0.1:21802/"
    echo "logs in $LOG_DIR"
    ;;

  stop)
    echo "stopping the stack"
    for name in web bff refdata iam nats; do
      stop_one "$name"
    done
    ;;

  status)
    echo "listening:"
    ss -ltn 2>/dev/null | grep -E ":(21801|21802|21805|21806)\b" || echo "  nothing from this stack"
    ;;

  *)
    echo "usage: dev-stack.sh {start|stop|status}" >&2
    exit 2
    ;;
esac
