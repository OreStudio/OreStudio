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
#
# Quote stripping: the source .env uses KEY="value" for any value containing
# an embedded \n escape (e.g. ORES_IAM_SERVICE_JWT_PRIVATE_KEY's PEM, with
# real newlines encoded as literal backslash-n so it survives as one .env
# line) -- bash's `source` strips those quotes on assignment, but podman's
# `--env-file` parser does not: it passes the literal quote characters
# through as part of the value, corrupting anything that must parse as
# exact content (like a PEM). Strip one matching pair of surrounding double
# quotes per line so podman's env-file consumption matches what `source`
# would produce.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
src="$root/.env"
dst="$root/docker/.env"

if [[ ! -f "$src" ]]; then
    echo "Error: $src not found — run compass env configure first" >&2
    exit 1
fi

sed -E 's/^([A-Za-z_][A-Za-z0-9_]*)="(.*)"$/\1=\2/' "$src" > "$dst"

echo "Wrote $dst"
