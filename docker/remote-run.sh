#!/usr/bin/env bash
# Remote side of `compass env deploy <host>` — runs ON the target WSL
# host, piped in via `ssh <host> bash -s` (REMOTE_ROOT and ROLE arrive as
# inline environment assignments on the ssh command line).
#
# This mirrors docker/run-pod.sh but for the sprint-24 Newton-proven
# networking model: Postgres is a native install on the WSL host
# (localhost:5433), unreachable through a pod's slirp4netns, so both
# containers run with --network=host — no pod object, no -p mappings,
# and the services container passes --user <uid>:<gid> explicitly (no
# pod to carry --userns=keep-id on).
#
# Layout on the remote host (mirrors the local checkout):
#   $REMOTE_ROOT/docker/.env                          per-host env profile
#   $REMOTE_ROOT/build/keys/nats/                     NATS client certs
#   $REMOTE_ROOT/build/keys/iam-rsa-private.pem       IAM JWT signing key
#   $REMOTE_ROOT/build/config/nats-<label>.conf       rendered NATS config
#   $REMOTE_ROOT/compute/compute.env                  compute-node config
#   $REMOTE_ROOT/compute/keys/                        compute client certs
#
# ROLE=runtime (default): service-runtime + NATS sidecar containers.
# ROLE=compute: just the ores.compute.wrapper compute node, connecting
# outward to the serving environment's NATS core (partial environment).
#
# Idempotent: containers are rm -f'd before recreate; re-running picks
# up a newly transferred image or env without manual cleanup.
set -euo pipefail

REMOTE_ROOT="${REMOTE_ROOT:?REMOTE_ROOT not set}"
ROLE="${ROLE:-runtime}"
IMAGE_TAG="${IMAGE_TAG:-local}"

echo "=== remote-run starting ==="
echo "  REMOTE_ROOT : $REMOTE_ROOT"
echo "  ROLE        : $ROLE"
echo "  IMAGE_TAG   : $IMAGE_TAG"
echo "  hostname    : $(hostname)"
echo "  whoami      : $(whoami)"
echo "  uid/gid     : $(id -u)/$(id -g)"

# Rootless podman under a non-login ssh shell may not inherit the user
# session's XDG_RUNTIME_DIR (the sprint-24 Newton session hit exactly
# this — see its dbus.socket notes). Set it defensively.
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
echo "  XDG_RUNTIME_DIR: $XDG_RUNTIME_DIR"

cd "$REMOTE_ROOT"
echo "  cwd : $(pwd)"

if [[ "$ROLE" == "compute" ]]; then
    compute_env="$REMOTE_ROOT/compute/compute.env"
    if [[ ! -f "$compute_env" ]]; then
        echo "Error: $compute_env not found — run: compass env deploy $ORES_REMOTE_HOST --role compute" >&2
        exit 1
    fi
    # shellcheck disable=SC1091
    source "$compute_env"

    label="${ORES_COMPUTE_LABEL:-ores}"
    container="ores-compute-node-${label}"
    certs_volume="ores-nats-client-certs-${label}"
    keys_dir="$REMOTE_ROOT/compute/keys"
    work_dir="$REMOTE_ROOT/compute/work"
    image="localhost/ores-compute-wrapper:${IMAGE_TAG}"
    host_id="${ORES_COMPUTE_WRAPPER_HOST_ID:?compute env missing ORES_COMPUTE_WRAPPER_HOST_ID}"
    tenant_id="${ORES_COMPUTE_WRAPPER_TENANT_ID:?compute env missing ORES_COMPUTE_WRAPPER_TENANT_ID}"
    http_base_url="${ORES_COMPUTE_WRAPPER_HTTP_BASE_URL:-}"

    echo "=== Staging compute client certs into volume '$certs_volume' ==="
    podman volume create "$certs_volume" >/dev/null 2>&1 || true
    volume_mountpoint="$(podman volume inspect "$certs_volume" --format '{{.Mountpoint}}')"
    cp -a "$keys_dir"/. "$volume_mountpoint"/
    # Owner-only: the wrapper container reads this volume as the same
    # uid that staged it (--user below), and the keys are the serving
    # environment's client private key + CA.
    chmod -R u+rwX,go-rwx "$volume_mountpoint"

    mkdir -p "$work_dir"

    echo "=== Recreating compute node container '$container' ==="
    podman rm -f "$container" >/dev/null 2>&1 || true
    extra_args=()
    if [[ -n "$http_base_url" ]]; then
        extra_args=(--http-base-url "$http_base_url")
    fi
    podman run -d --rm --network=host --userns=keep-id \
        --name "$container" \
        --user "${ORES_REMOTE_USER:-$(id -u)}:${ORES_REMOTE_GROUP:-$(id -g)}" \
        --env-file "$compute_env" \
        -v "$certs_volume:$keys_dir:ro" \
        -v "$work_dir:$work_dir:rw" \
        "$image" \
        --host-id "$host_id" --tenant-id "$tenant_id" \
        --work-dir "$work_dir" "${extra_args[@]}" >/dev/null

    echo
    echo "Compute node '$container' started (NATS: $ORES_COMPUTE_WRAPPER_NATS_URL)."
    echo "  Logs : podman logs -f $container"
    echo "  Stop : compass env deploy <host> --role compute --stop"
    exit 0
fi

# --- Runtime role: service-runtime + NATS sidecar -------------------------

env_file="$REMOTE_ROOT/docker/.env"
if [[ ! -f "$env_file" ]]; then
    echo "Error: $env_file not found on remote — run: compass env deploy <host>" >&2
    exit 1
fi
# shellcheck disable=SC1091
source "$env_file"

label="${ORES_CHECKOUT_LABEL:-ores}"
nats_container="ores-nats-${label}"
services_container="ores-services-${label}"
services_image="localhost/ores-service-runtime:${IMAGE_TAG}"
nats_image="localhost/ores-nats:${IMAGE_TAG}"
certs_volume="ores-nats-client-certs-${label}"
keys_dir="$REMOTE_ROOT/build/keys/nats"
nats_config="$REMOTE_ROOT/build/config/nats-${label}.conf"
nats_store_dir="${ORES_NATS_STORE_DIR:?ORES_NATS_STORE_DIR not set in remote env — re-run compass env deploy}"
jwt_key_file="$REMOTE_ROOT/build/keys/iam-rsa-private.pem"

if [[ ! -f "$nats_config" ]]; then
    echo "Error: $nats_config not found on remote" >&2
    exit 1
fi
if [[ ! -d "$keys_dir" ]]; then
    echo "Error: $keys_dir not found on remote" >&2
    exit 1
fi

echo "=== Staging NATS client certs into volume '$certs_volume' ==="
podman volume create "$certs_volume" >/dev/null 2>&1 || true
volume_mountpoint="$(podman volume inspect "$certs_volume" --format '{{.Mountpoint}}')"
cp -a "$keys_dir"/. "$volume_mountpoint"/
# Owner-only: the services container reads this volume as
# --user <uid>:<gid> (the same uid doing the staging here), and $keys_dir
# holds every service's client private key plus the CA private key.
chmod -R u+rwX,go-rwx "$volume_mountpoint"

mkdir -p "$nats_store_dir"

echo "=== Recreating containers ==="
podman rm -f "$nats_container" "$services_container" >/dev/null 2>&1 || true

echo "=== Starting NATS sidecar ($nats_image) ==="
# The NATS server's own cert (loaded by the Go nats-server binary) is
# unaffected by the client-cert bind-mount quirk and stays a plain bind
# mount — same as run-pod.sh.
nats_cid=$(podman run -d --rm --network=host --name "$nats_container" \
    -v "$nats_config:$nats_config:ro" \
    -v "$keys_dir:$keys_dir:ro" \
    -v "$nats_store_dir:$nats_store_dir:rw" \
    "$nats_image" --config "$nats_config")
echo "  NATS container ID : $nats_cid"

echo "=== Starting services container ($services_image) ==="
# ORES_IAM_SERVICE_JWT_PRIVATE_KEY's env value only ever has escaped '\n'
# (not real newlines), which OpenSSL's PEM parser can't read — podman's
# --env-file format has no way to represent a real multi-line value. Pass
# it separately, straight from the real PEM file; --env after --env-file
# wins for the same key. Mirrors run-pod.sh and compass_services.py.
#
# --userns=keep-id: the cert volume files are owned by the host's
# uid (staged by `cp -a` running as the host user). Without keep-id,
# rootless podman remaps the container's uid through a user namespace,
# and the container's uid no longer matches the file owner on the volume
# — OpenSSL silently fails to read the client cert, and the TLS
# handshake produces "bad record MAC". run-pod.sh avoids this via
# --userns=keep-id on the pod; with --network=host (no pod) we apply it
# per container.
jwt_key_args=()
if [[ -f "$jwt_key_file" ]]; then
    jwt_key_args=(--env "ORES_IAM_SERVICE_JWT_PRIVATE_KEY=$(cat "$jwt_key_file")")
    echo "  JWT key : $jwt_key_file (real PEM, via --env)"
else
    echo "  WARNING : JWT key not found at $jwt_key_file — services will start without signing capability"
fi
# Per-service mTLS cert: systemd units pass --nats-tls-cert/--nats-tls-key
# per service (e.g. ores.iam.service.crt for the IAM entrypoint). The
# shared env vars ORES_NATS_TLS_CERT / ORES_NATS_TLS_KEY use the Qt
# client cert, which is NOT valid for service-to-NATS mTLS (the server
# rejects it with "bad record MAC"). Pass the actual service cert as a
# CLI arg; --nats-tls-* flags override the env vars.
tls_cert="$keys_dir/ores.iam.service.crt"
tls_key="$keys_dir/ores.iam.service.key"
tls_ca="$keys_dir/ca.crt"
# Mount the host's publish/log so logs survive container restarts and
# are inspectable via `ssh <host> tail -f <remote_root>/publish/log/...`
log_dir="$REMOTE_ROOT/build/output/$ORES_PRESET/publish/log"
mkdir -p "$log_dir"
svc_cid=$(podman run -d --network=host --userns=keep-id \
    --name "$services_container" \
    --user "${ORES_REMOTE_USER:-$(id -u)}:${ORES_REMOTE_GROUP:-$(id -g)}" \
    --env-file "$env_file" \
    "${jwt_key_args[@]}" \
    -v "$certs_volume:$keys_dir:ro" \
    -v "$log_dir:/app/log:rw" \
    "$services_image" \
    --log-enabled --log-level info --log-directory ../log --log-replica-index 0 \
    --nats-tls-ca "$tls_ca" \
    --nats-tls-cert "$tls_cert" \
    --nats-tls-key "$tls_key")
echo "  Services container ID : $svc_cid"

echo
echo "=== ${ROLE} containers started ==="
echo "  NATS     : $nats_container ($nats_cid)"
echo "  Services : $services_container ($svc_cid)"
echo "  Logs (NATS)     : podman logs -f $nats_container"
echo "  Logs (services) : podman logs -f $services_container"
echo "  Stop            : compass env deploy <host> --stop"

# Give the services a few seconds to pass their own healthcheck before
# the ssh session closes — if they exit immediately we want to see it.
echo "=== Waiting for healthcheck (5s) ==="
sleep 5
if podman ps --filter "name=$nats_container" --format "{{.Status}}" | grep -q "Up"; then
    echo "  NATS     : running"
else
    echo "  NATS     : STOPPED — podman logs $nats_container"
fi
if podman ps --filter "name=$services_container" --format "{{.Status}}" | grep -q "Up"; then
    echo "  Services : running"
else
    echo "  Services : STOPPED — podman logs $services_container"
fi
