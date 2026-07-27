#!/usr/bin/env bash
# Runs the ORE Studio service-runtime container plus its NATS sidecar as a
# single podman pod (shared network namespace -- "localhost" resolves to
# the sidecar with no host-alias rewriting needed, see generate-env.sh).
#
# NATS client TLS certs are staged into a podman-managed volume rather than
# bind-mounted straight from the host. Bind-mounting them directly breaks
# the TLS handshake: cnats/OpenSSL silently fails to present the client
# certificate over a bind mount even though the bytes, path, permissions,
# uid, seccomp profile and capabilities are all otherwise identical to a
# working run (confirmed by elimination -- baking the same files into an
# image, or staging them through a plain podman volume, both work; only
# the direct host bind mount reproduces the failure). The NATS server's own
# cert (loaded by the Go nats-server binary in the sidecar, not our C++
# code) is unaffected and stays a plain bind mount.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

if [[ ! -f docker/.env ]]; then
    echo "Error: docker/.env not found -- run docker/generate-env.sh first" >&2
    exit 1
fi

# shellcheck disable=SC1091
source docker/.env

label="${ORES_CHECKOUT_LABEL:-$(basename "$root")}"
pod_name="ores-pod-${label}"
services_image="localhost/ores-service-runtime:local"
nats_image="localhost/ores-nats:local"
certs_volume="ores-nats-client-certs-${label}"
keys_dir="$root/build/keys/nats"
nats_config="$root/build/config/nats-${label}.conf"
nats_store_dir="${ORES_NATS_STORE_DIR:?ORES_NATS_STORE_DIR not set in .env -- run compass env configure}"

if [[ ! -f "$nats_config" ]]; then
    echo "Error: $nats_config not found -- run: compass nats init" >&2
    exit 1
fi

echo "=== Staging NATS client certs into volume '$certs_volume' ==="
podman volume create "$certs_volume" >/dev/null 2>&1 || true
volume_mountpoint="$(podman volume inspect "$certs_volume" --format '{{.Mountpoint}}')"
cp -a "$keys_dir"/. "$volume_mountpoint"/
# Owner-only: the services container reads this volume as --user "$(id -u):$(id -g)"
# (the same host uid/gid doing the staging here), and $keys_dir holds every
# service's client private key plus the CA private key -- no other local
# account needs read access to them.
chmod -R u+rwX,go-rwx "$volume_mountpoint"
echo "  Staged $(find "$volume_mountpoint" -maxdepth 1 -type f | wc -l) files"

echo "=== Recreating pod '$pod_name' ==="
podman pod rm -f "$pod_name" >/dev/null 2>&1 || true
podman pod create --name "$pod_name" \
    -p "${ORES_HTTP_PORT}:${ORES_HTTP_PORT}" \
    -p "${ORES_CONTROLLER_SERVICE_WT_PORT}:${ORES_CONTROLLER_SERVICE_WT_PORT}" \
    -p "${ORES_NATS_PORT}:${ORES_NATS_PORT}" \
    -p "${ORES_NATS_MONITOR_PORT}:${ORES_NATS_MONITOR_PORT}" \
    >/dev/null

echo "=== Starting NATS sidecar ==="
podman run -d --rm --pod "$pod_name" --name "ores-nats-${label}" \
    -v "$nats_config:$nats_config:ro" \
    -v "$keys_dir:$keys_dir:ro" \
    -v "$nats_store_dir:$nats_store_dir:rw" \
    "$nats_image" --config "$nats_config" >/dev/null

echo "=== Starting services container ==="
podman run -d --rm --pod "$pod_name" --name "ores-services-${label}" \
    --user "$(id -u):$(id -g)" \
    --env-file docker/.env \
    -v "$certs_volume:$keys_dir:ro" \
    "$services_image" >/dev/null

echo
echo "Pod '$pod_name' started."
echo "  Logs (NATS)     : podman logs -f ores-nats-${label}"
echo "  Logs (services) : podman logs -f ores-services-${label}"
echo "  Stop            : docker/stop-pod.sh"
