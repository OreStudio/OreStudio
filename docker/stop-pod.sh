#!/usr/bin/env bash
# Stops and removes the pod started by docker/run-pod.sh. The staged NATS
# client-cert volume is left in place (cheap to keep, re-staged fresh on
# every run-pod.sh anyway) unless --purge is passed.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

# shellcheck disable=SC1091
source .env

label="${ORES_CHECKOUT_LABEL:-$(basename "$root")}"
pod_name="ores-pod-${label}"
certs_volume="ores-nats-client-certs-${label}"

echo "=== Stopping pod '$pod_name' ==="
podman pod rm -f "$pod_name" >/dev/null 2>&1 || true

if [[ "${1:-}" == "--purge" ]]; then
    echo "=== Removing staged certs volume '$certs_volume' ==="
    podman volume rm -f "$certs_volume" >/dev/null 2>&1 || true
fi

echo "Done."
