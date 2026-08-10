#!/usr/bin/env bash
# -*- mode: sh; tab-width: 4; indent-tabs-offset: nil; sh-basic-offset: 4 -*-
#
# setup_nats_server.sh - put the vendored nats-server binary on PATH.
#
# Unpacks the pinned nats-server release under external/nats/packages/
# for the current OS/architecture into build/nats/vendored/ and prepends
# that directory to PATH via GITHUB_PATH. Every CI platform runs the same
# pinned binary this way, instead of whatever its package manager ships
# (apt on Linux, brew on macOS, a GitHub-release download on Windows).
#
# Usage (from the checkout root):
#   ./build/scripts/setup_nats_server.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
VERSION="v2.14.3"
PACKAGES_DIR="${CHECKOUT_ROOT}/external/nats/packages"

os="$(uname -s)"
arch="$(uname -m)"
case "${os}" in
    Linux*)
        if [[ "${arch}" == "arm64" ]]; then
            pkg="linux-arm64"
        else
            pkg="linux-amd64"
        fi
        ;;
    Darwin*)
        if [[ "${arch}" == "arm64" ]]; then
            pkg="darwin-arm64"
        else
            pkg="darwin-amd64"
        fi
        ;;
    *)
        echo "error: unsupported platform: ${os}-${arch} (Windows CI uses setup_nats_server.ps1)" >&2
        exit 1
        ;;
esac

archive="${PACKAGES_DIR}/nats-server-${VERSION}-${pkg}.tar.gz"
if [[ ! -f "${archive}" ]]; then
    echo "error: no vendored nats-server archive for ${os}-${arch}: ${archive}" >&2
    exit 1
fi

dest="${CHECKOUT_ROOT}/build/nats/vendored/${pkg}"
bin=""
if [[ -x "${dest}/nats-server" ]]; then
    bin="${dest}/nats-server"
fi

if [[ -z "${bin}" ]]; then
    scratch="$(mktemp -d)"
    trap 'rm -rf "${scratch}"' EXIT
    tar -xzf "${PACKAGES_DIR}/nats-server-${VERSION}-${pkg}.tar.gz" -C "${scratch}"
    rm -rf "${dest}"
    mkdir -p "$(dirname "${dest}")"
    mv "${scratch}"/nats-server-* "${dest}"
    if [[ -x "${dest}/nats-server" ]]; then
        bin="${dest}/nats-server"
    fi
fi

if [[ -z "${bin}" ]]; then
    echo "error: nats-server binary not found after unpacking: ${dest}" >&2
    exit 1
fi

if [[ -z "${GITHUB_PATH:-}" ]]; then
    echo "warning: GITHUB_PATH is not set; ${bin} is not on PATH" >&2
else
    echo "${dest}" >> "${GITHUB_PATH}"
fi
