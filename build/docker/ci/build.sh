#!/usr/bin/env bash
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Builds the Linux CI image with the versions in versions.env, then checks
# that each tool in the image reports the version the file states.
#
# The tag names the main versions and a hash of the files the image is built
# from, so a change to any of them gives a new tag:
#
#   ubuntu24.04-clang19-gcc14-<hash>
#
# Usage:
#   build/docker/ci/build.sh [--image NAME] [--engine docker|podman] [--print-ref]
#
# The tag identifies the inputs, not the content. The base image and the apt
# packages can move between two builds of the same tag, so a published tag is
# never built again.
#
# NAME defaults to ores-ci. --print-ref prints the image reference and builds
# nothing, so a caller can ask whether that tag is already published. The
# engine defaults to docker, or podman when docker is absent. The last line of
# the output is the full image reference.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "${here}/../../.." && pwd)"
image="ores-ci"
engine=""
print_ref=0

while [[ $# -gt 0 ]]; do
    case "$1" in
        --image) image="$2"; shift 2 ;;
        --engine) engine="$2"; shift 2 ;;
        --print-ref) print_ref=1; shift ;;
        *) echo "unknown argument: $1" >&2; exit 2 ;;
    esac
done

if [[ -z "${engine}" ]]; then
    if command -v docker > /dev/null; then
        engine=docker
    else
        engine=podman
    fi
fi

build_args=()
while IFS='=' read -r key value; do
    [[ -z "${key}" || "${key}" == \#* ]] && continue
    declare "${key}=${value}"
    build_args+=(--build-arg "${key}=${value}")
done < "${here}/versions.env"

inputs=("${here}/Dockerfile" "${here}/versions.env" "${root}/build/scripts/install_llvm.sh")
hash="$(cat "${inputs[@]}" | sha256sum | cut -c1-12)"
tag="ubuntu${UBUNTU_VERSION}-clang${LLVM_MAJOR}-gcc${GCC_MAJOR}-${hash}"
ref="${image}:${tag}"

if [[ "${print_ref}" -eq 1 ]]; then
    echo "${ref}"
    exit 0
fi

"${engine}" build -f "${here}/Dockerfile" "${build_args[@]}" -t "${ref}" "${root}"

"${engine}" run --rm "${ref}" bash -euo pipefail -c "
    clang++ --version | head -n 1 | grep -F ' ${LLVM_MAJOR}.'
    llvm-cov --version | grep -F ' ${LLVM_MAJOR}.'
    clang-tidy --version | grep -F ' ${LLVM_MAJOR}.'
    cmake --version | head -n 1 | grep -F '${CMAKE_VERSION}'
    ninja --version | grep -xF '${NINJA_VERSION}'
    node --version | grep -xF 'v${NODE_VERSION}'
    postgres --version | grep -F ' ${PG_MAJOR}.'
    test -f /usr/share/postgresql/${PG_MAJOR}/extension/timescaledb.control
    g++-${GCC_MAJOR} --version | head -n 1
    git --version
    python3 --version
    valgrind --version
    gh --version | head -n 1
    test -f /etc/postgresql/${PG_MAJOR}/main/conf.d/timescaledb.conf
    printf '#include <print>\nint main() { std::println(\"C++23 ok\"); }\n' > /tmp/t.cpp
    clang++ -std=c++23 -v /tmp/t.cpp -o /tmp/t 2>&1 | grep -F 'Selected GCC installation' | grep -F '/${GCC_MAJOR}'
    /tmp/t
"

echo "${ref}"
