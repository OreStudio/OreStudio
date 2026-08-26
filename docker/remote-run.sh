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
        echo "Error: $compute_env not found — run: compass env deploy <host> --role compute" >&2
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
    log_dir="$REMOTE_ROOT/compute/log"
    mkdir -p "$log_dir"

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
        -v "$log_dir:/app/log:rw" \
        "$image" \
        --log-enabled --log-level info --log-directory ../log --log-replica-index 0 \
        --host-id "$host_id" --tenant-id "$tenant_id" \
        --work-dir "$work_dir" "${extra_args[@]}" >/dev/null

    echo
    echo "Compute node '$container' started (NATS: $ORES_COMPUTE_WRAPPER_NATS_URL)."
    echo "  Logs : $log_dir/*.log (podman logs -f $container for stdout)"
    echo "  Stop : compass env deploy <host> --role compute --stop"
    exit 0
fi

# --- Runtime role: per-service containers + NATS sidecar -------------------

env_file="$REMOTE_ROOT/docker/.env"
if [[ ! -f "$env_file" ]]; then
    echo "Error: $env_file not found on remote — run: compass env deploy <host>" >&2
    exit 1
fi
# shellcheck disable=SC1091
source "$env_file"

label="${ORES_CHECKOUT_LABEL:-ores}"
nats_container="ores-nats-${label}"
nats_image="localhost/ores-nats:${IMAGE_TAG}"
certs_volume="ores-nats-client-certs-${label}"
keys_dir="$REMOTE_ROOT/build/keys/nats"
nats_config="$REMOTE_ROOT/build/config/nats-${label}.conf"
nats_store_dir="${ORES_NATS_STORE_DIR:?ORES_NATS_STORE_DIR not set in remote env — re-run compass env deploy}"
jwt_key_file="$REMOTE_ROOT/build/keys/iam-rsa-private.pem"
log_dir="$REMOTE_ROOT/build/output/$ORES_PRESET/publish/log"
mkdir -p "$log_dir"
# Storage work dir.  /app/storage in the images is a 0755 placeholder
# (buildah normalizes COPY dir modes — see service-base.Dockerfile), so
# the writable storage dir is host-owned and bind-mounted, like /app/log.
storage_dir="$REMOTE_ROOT/build/output/$ORES_PRESET/publish/storage"
mkdir -p "$storage_dir"
# ORE service per-import work dir — same host-owned pattern; the env
# profile points ORES_ORE_SERVICE_WORK_DIR at ../var/ore-service/work
# (resolved against WORKDIR /app/bin → /app/var/ore-service/work), and
# that path does not exist writable in the image, so mount it here.
ore_work_dir="$REMOTE_ROOT/build/output/$ORES_PRESET/publish/ore-service-work"
mkdir -p "$ore_work_dir"

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
chmod -R u+rwX,go-rwx "$volume_mountpoint"

mkdir -p "$nats_store_dir"

echo "=== Stopping old containers ==="
# Scoped to this label (ores-nats-<label>, ores-<svc-dashed>-<label>) so a
# shared host's other environments' containers are left alone -- same
# filter as remote-stop.sh.
for c in $(podman ps -aq --filter "name=ores-.*-${label}" 2>/dev/null); do
    podman rm -f "$c" >/dev/null 2>&1 || true
done

echo "=== Starting NATS sidecar ($nats_image) ==="
nats_cid=$(podman run -d --rm --network=host --name "$nats_container" \
    -v "$nats_config:$nats_config:ro" \
    -v "$keys_dir:$keys_dir:ro" \
    -v "$nats_store_dir:$nats_store_dir:rw" \
    "$nats_image" --config "$nats_config")
echo "  NATS container ID : $nats_cid"

# SERVICES is a space-separated list from env_deploy.py (_runtime_services).
SERVICES="${SERVICES:-}"
if [[ -z "$SERVICES" ]]; then
    echo "Error: SERVICES not set — env_deploy.py should pass the service list" >&2
    exit 1
fi

running=0
failed=0
for svc in $SERVICES; do
    container="ores-${svc//./-}-${label}"
    image="localhost/${svc}:${IMAGE_TAG}"
    tls_cert="$keys_dir/${svc}.crt"
    tls_key="$keys_dir/${svc}.key"

    # Only IAM needs the JWT signing key.
    jwt_args=()
    if [[ "$svc" == "ores.iam.service" && -f "$jwt_key_file" ]]; then
        jwt_args=(--env "ORES_IAM_SERVICE_JWT_PRIVATE_KEY=$(cat "$jwt_key_file")")
    fi

    echo -n "  $svc ... "
    cid=$(podman run -d --network=host --userns=keep-id \
        --name "$container" \
        --user "${ORES_REMOTE_USER:-$(id -u)}:${ORES_REMOTE_GROUP:-$(id -g)}" \
        --env-file "$env_file" \
        "${jwt_args[@]}" \
        -v "$certs_volume:$keys_dir:ro" \
        -v "$log_dir:/app/log:rw" \
        -v "$storage_dir:/app/storage:rw" \
        -v "$ore_work_dir:/app/var/ore-service/work:rw" \
        "$image" \
        --log-enabled --log-level info --log-directory ../log --log-replica-index 0 \
        --nats-tls-ca "$keys_dir/ca.crt" \
        --nats-tls-cert "$tls_cert" \
        --nats-tls-key "$tls_key" \
        2>/dev/null) || true
    if [[ -n "$cid" ]]; then
        echo "running ($cid)"
        # ((running++)) would return the pre-increment value (0 on the
        # first service) and kill the script under `set -e` — use += so
        # the expression value is the new, always-nonzero count.
        ((running += 1))
    else
        echo "FAILED"
        ((failed += 1))
    fi
done

echo
echo "=== ${ROLE} containers started: ${running} running, ${failed} failed ==="
echo "  Logs (NATS) : podman logs -f $nats_container"
echo "  Logs        : $log_dir/*.log"
echo "  Stop        : compass env deploy <host> --stop"

echo "=== Waiting for healthcheck (5s) ==="
sleep 5
nats_ok=$(podman ps --filter "name=$nats_container" --format "{{.Status}}" | grep -c "Up" || true)
echo "  NATS     : $([ "$nats_ok" -gt 0 ] && echo "running" || echo "STOPPED")"
for svc in $SERVICES; do
    container="ores-${svc//./-}-${label}"
    status=$(podman ps --filter "name=$container" --format "{{.Status}}" 2>/dev/null)
    if [[ -n "$status" ]]; then
        echo "  $svc : $status"
    else
        echo "  $svc : EXITED — podman logs $container"
    fi
done
