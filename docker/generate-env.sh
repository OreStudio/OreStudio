#!/usr/bin/env bash
# Derives docker/.env from the checkout's .env. NATS runs as a sidecar
# container sharing the services container's pod network namespace, so
# "localhost" already resolves correctly with no rewriting — this is a
# straight copy. Postgres is a real remote host already (offloaded to a
# WSL host over SSH), so it too passes through unchanged. Once Postgres
# gets its own sidecar this file may need a similar localhost-is-fine note,
# but nothing to translate either way.
# Filesystem paths (TLS certs, NATS store dir) are left untouched — the
# containers bind-mount them at identical paths as the host.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
src="$root/.env"
dst="$root/docker/.env"

if [[ ! -f "$src" ]]; then
    echo "Error: $src not found — run compass env configure first" >&2
    exit 1
fi

cp "$src" "$dst"

echo "Wrote $dst"
