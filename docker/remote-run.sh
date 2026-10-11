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
    # One wrapper per core but one, so the host stays responsive.
    cores="$(nproc)"
    replicas="${ORES_COMPUTE_REPLICAS:-$(( cores > 1 ? cores - 1 : 1 ))}"
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

    # The wrappers run as systemd user services built from Quadlet files, so
    # they restart after a crash and start again at boot (the user must have
    # lingering enabled, which `compass env deploy --setup-host` does).
    export DBUS_SESSION_BUS_ADDRESS="${DBUS_SESSION_BUS_ADDRESS:-unix:path=$XDG_RUNTIME_DIR/bus}"
    unit_dir="${XDG_CONFIG_HOME:-$HOME/.config}/containers/systemd"
    mkdir -p "$unit_dir"
    user_manager="$(systemctl --user is-system-running 2>&1 || true)"
    case "$user_manager" in
        running | degraded) ;;
        *)
            echo "Error: the systemd user manager is not running for $(whoami) ($user_manager)." >&2
            echo "  Run: loginctl enable-linger $(whoami)  (as root), then retry." >&2
            exit 1
            ;;
    esac

    echo "=== Replacing $replicas compute node unit(s) '$container-N' ==="
    # Remove the units and containers of an earlier deploy, including a
    # single-container deploy from before replicas existed.
    systemctl --user stop "$container-*.service" >/dev/null 2>&1 || true
    for old_unit in "$unit_dir/$container"-*.container; do
        [[ -e "$old_unit" ]] && rm -f "$old_unit"
    done
    for old in $(podman ps -aq --filter "name=^${container}" 2>/dev/null); do
        podman rm -f "$old" >/dev/null 2>&1 || true
    done

    extra_args=""
    if [[ -n "$http_base_url" ]]; then
        extra_args=" --http-base-url $http_base_url"
    fi
    run_uid="${ORES_REMOTE_USER:-$(id -u)}"
    run_gid="${ORES_REMOTE_GROUP:-$(id -g)}"
    node_name="$(hostname)"
    units=()
    for replica in $(seq 1 "$replicas"); do
        # Same derivation as the local services: UUID v5 of "<hostname>:<replica>".
        host_id="$(python3 -c 'import sys,uuid; print(uuid.uuid5(uuid.UUID("6ba7b810-9dad-11d1-80b4-00c04fd430c8"), sys.argv[1]))' "$node_name:$replica")"
        replica_dir="$work_dir/$replica"
        mkdir -p "$replica_dir"
        cat > "$unit_dir/$container-$replica.container" <<UNIT
[Unit]
Description=ORE Studio compute wrapper $replica ($label)
StartLimitIntervalSec=0

[Container]
Image=$image
ContainerName=$container-$replica
Network=host
UserNS=keep-id
User=$run_uid
Group=$run_gid
EnvironmentFile=$compute_env
Volume=$certs_volume:$keys_dir:ro
Volume=$work_dir:$work_dir:rw
Volume=$log_dir:/app/log:rw
Exec=--log-enabled --log-level info --log-to-console --log-directory ../log --log-filename ores.compute.wrapper --log-replica-index $((replica - 1)) --host-id $host_id --tenant-id $tenant_id --work-dir $replica_dir --package-dir $work_dir/packages$extra_args

[Service]
Restart=always
RestartSec=10

[Install]
WantedBy=default.target
UNIT
        units+=("$container-$replica.service")
    done

    systemctl --user daemon-reload
    # A unit that systemd cannot see means the Quadlet generator is missing
    # (podman older than 4.4) or the file is malformed.
    if ! systemctl --user cat "${units[0]}" >/dev/null 2>&1; then
        echo "Error: systemd did not generate ${units[0]} from $unit_dir." >&2
        echo "  Quadlet needs podman 4.4 or later (this host: $(podman --version))." >&2
        exit 1
    fi
    systemctl --user start "${units[@]}"

    echo
    echo "$replicas compute node(s) started as '$container-N' (NATS: $ORES_COMPUTE_WRAPPER_NATS_URL)."
    echo "  Units: systemctl --user status $container-1.service (restart on failure, start at boot)"
    echo "  Logs : $log_dir/*.log (podman logs -f $container-1 for stdout)"
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
        --log-enabled --log-level info --log-to-console \
        --log-directory ../log --log-filename "$svc" --log-replica-index 0 \
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
