#!/usr/bin/env bash
# Remote side of `compass env deploy <host> --stop` — runs ON the target
# WSL host, piped in via `ssh <host> bash -s`. Removes the containers
# started by docker/remote-run.sh. The staged certs volume is left in
# place (cheap to keep, re-staged on every remote-run.sh anyway) unless
# PURGE=1 is passed.
set -euo pipefail

REMOTE_ROOT="${REMOTE_ROOT:?REMOTE_ROOT not set}"
ROLE="${ROLE:-runtime}"
PURGE="${PURGE:-}"

export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

cd "$REMOTE_ROOT"

if [[ "$ROLE" == "compute" ]]; then
    compute_env="$REMOTE_ROOT/compute/compute.env"
    if [[ -f "$compute_env" ]]; then
        # shellcheck disable=SC1091
        source "$compute_env"
    fi
    label="${ORES_COMPUTE_LABEL:-ores}"
    container="ores-compute-node-${label}"
    echo "=== Stopping compute node '$container' ==="
    podman rm -f "$container" >/dev/null 2>&1 || true
else
    env_file="$REMOTE_ROOT/docker/.env"
    if [[ -f "$env_file" ]]; then
        # shellcheck disable=SC1091
        source "$env_file"
    fi
    label="${ORES_CHECKOUT_LABEL:-ores}"
    echo "=== Stopping NATS + services containers (label '$label') ==="
    # remote-run.sh names containers "ores-nats-<label>" and
    # "ores-<service-dotted-to-dashes>-<label>" -- remove them all, scoped
    # to this label so other environments' containers are left alone.
    for c in $(podman ps -aq --filter "name=ores-.*-${label}" 2>/dev/null); do
        podman rm -f "$c" >/dev/null 2>&1 || true
    done
fi

if [[ -n "$PURGE" ]]; then
    certs_volume="ores-nats-client-certs-${label}"
    echo "=== Removing staged certs volume '$certs_volume' ==="
    podman volume rm -f "$certs_volume" >/dev/null 2>&1 || true
fi

echo "Done."
