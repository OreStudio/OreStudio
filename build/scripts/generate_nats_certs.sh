#!/usr/bin/env bash
# generate_nats_certs.sh — Generate CA + per-service mTLS certificates for NATS.
#
# Usage:
#   build/scripts/generate_nats_certs.sh [--force] [--hostname <host>]
#
# Creates build/keys/nats/ with:
#   ca.key, ca.crt              — Internal CA (1 year validity)
#   nats-server.key/crt         — NATS broker certificate (SAN: localhost + <host>)
#   <service>.key/crt           — One client cert per service (90-day validity)
#
# Existing files are NOT overwritten unless --force is given.
# Run with --force in CI (ephemeral keys); omit in dev (stable keys).
#
# Requires: openssl

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
KEYS_DIR="$PROJECT_DIR/build/keys/nats"

FORCE=false
HOSTNAME="localhost"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --force)    FORCE=true; shift ;;
        --hostname) HOSTNAME="$2"; shift 2 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

SERVICES=(
    ores.iam.service
    ores.refdata.service
    ores.dq.service
    ores.variability.service
    ores.assets.service
    ores.synthetic.service
    ores.scheduler.service
    ores.reporting.service
    ores.telemetry.service
    ores.trading.service
    ores.compute.service
    ores.http.server
    ores.wt.service
    ores.qt.client
    ores.compute.wrapper
)

CA_DAYS=365
LEAF_DAYS=90

mkdir -p "$KEYS_DIR"

maybe_generate() {
    local target="$1"
    if [[ -f "$target" ]] && [[ "$FORCE" == false ]]; then
        echo "  (exists, skipping) $target"
        return 1   # signal: skip
    fi
    return 0       # signal: generate
}

echo "==> CA"
if maybe_generate "$KEYS_DIR/ca.key"; then
    openssl ecparam -name prime256v1 -genkey -noout -out "$KEYS_DIR/ca.key"
    MSYS2_ARG_CONV_EXCL="/CN=" openssl req -new -x509 \
        -key "$KEYS_DIR/ca.key" \
        -out "$KEYS_DIR/ca.crt" \
        -days "$CA_DAYS" \
        -subj "/CN=ores-nats-ca/O=ORE Studio"
    echo "  Generated CA: $KEYS_DIR/ca.crt"
fi

echo "==> NATS server certificate"
if maybe_generate "$KEYS_DIR/nats-server.key"; then
    openssl ecparam -name prime256v1 -genkey -noout \
        -out "$KEYS_DIR/nats-server.key"
    MSYS2_ARG_CONV_EXCL="/CN=" openssl req -new \
        -key "$KEYS_DIR/nats-server.key" \
        -out "$KEYS_DIR/nats-server.csr" \
        -subj "/CN=nats-server/O=ORE Studio"
    # SAN extension for localhost and the deployment hostname
    SAN_EXT="subjectAltName=DNS:localhost,DNS:${HOSTNAME},IP:127.0.0.1"
    openssl x509 -req \
        -in "$KEYS_DIR/nats-server.csr" \
        -CA "$KEYS_DIR/ca.crt" \
        -CAkey "$KEYS_DIR/ca.key" \
        -CAcreateserial \
        -out "$KEYS_DIR/nats-server.crt" \
        -days "$LEAF_DAYS" \
        -extfile <(echo "$SAN_EXT")
    rm -f "$KEYS_DIR/nats-server.csr"
    echo "  Generated: $KEYS_DIR/nats-server.crt (SAN: localhost, $HOSTNAME)"
fi

echo "==> Service client certificates"
for SERVICE in "${SERVICES[@]}"; do
    KEY="$KEYS_DIR/${SERVICE}.key"
    CERT="$KEYS_DIR/${SERVICE}.crt"
    if maybe_generate "$KEY"; then
        openssl ecparam -name prime256v1 -genkey -noout -out "$KEY"
        MSYS2_ARG_CONV_EXCL="/CN=" openssl req -new \
            -key "$KEY" \
            -out "$KEYS_DIR/${SERVICE}.csr" \
            -subj "/CN=${SERVICE}/O=ORE Studio"
        openssl x509 -req \
            -in "$KEYS_DIR/${SERVICE}.csr" \
            -CA "$KEYS_DIR/ca.crt" \
            -CAkey "$KEYS_DIR/ca.key" \
            -CAcreateserial \
            -out "$CERT" \
            -days "$LEAF_DAYS"
        rm -f "$KEYS_DIR/${SERVICE}.csr"
        echo "  Generated: $CERT"
    fi
done

rm -f "$KEYS_DIR/ca.srl"

echo ""
echo "Done. Certificates written to $KEYS_DIR"
echo "Rotation: re-run with --force to regenerate all certificates."
