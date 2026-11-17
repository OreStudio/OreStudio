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
# Stages the web service's built output into build/docker-stage/web/ for
# docker/service-web.Dockerfile to COPY.
#
# The service is Node, not a compiled binary, so what it needs is different
# from the other services: the compiled JavaScript of the two packages, the
# browser bundle the BFF serves, its site configuration, and the installed
# dependencies with the symlinks a workspace install uses. Those symlinks are
# relative and sit inside the tree, so copying the tree whole keeps them
# working; installing inside the image would instead need the registry and the
# workspace packages published nowhere.
#
# The build itself is the C++ build's TypeScript target — nothing is run here.
# A checkout without the compiled output is an error rather than a silent
# partial image.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
web="$root/projects/ores.web"
stage="$root/build/docker-stage/web"

for required in packages/bff/dist packages/web/dist node_modules; do
    if [[ ! -d "$web/$required" ]]; then
        echo "Error: $web/$required not found — build the web bundle first" >&2
        echo "  (cd projects/ores.web && npm ci && npm run build)" >&2
        exit 1
    fi
done

rm -rf "$stage"
mkdir -p "$stage"

# The BFF resolves its site configuration from the working directory and the
# browser bundle from its own module path, so both have to keep the layout the
# checkout has: config/ beside packages/bff/dist and packages/web/dist.
cp -a "$web/config" "$stage/config"
cp -a "$web/package.json" "$stage/package.json"
mkdir -p "$stage/packages/bff" "$stage/packages/web"
cp -a "$web/packages/bff/package.json" "$stage/packages/bff/package.json"
cp -a "$web/packages/bff/dist" "$stage/packages/bff/dist"
cp -a "$web/packages/web/package.json" "$stage/packages/web/package.json"
cp -a "$web/packages/web/dist" "$stage/packages/web/dist"

# The workspace packages the BFF imports at runtime are linked from the root
# node_modules, so the installed tree travels with the image — but only the
# runtime half of it. The checkout's tree carries the compiler, the test
# runner and their dependencies, which the image never loads and which are
# most of its size; pruning the staged copy takes it from 207 MB to 60 MB.
cp -a "$web/node_modules" "$stage/node_modules"
cp -a "$web/package-lock.json" "$stage/package-lock.json" 2>/dev/null || true
( cd "$stage" && npm prune --omit=dev --ignore-scripts --no-audit --no-fund \
    >/dev/null 2>&1 ) || echo "  warning: npm prune failed, image carries dev dependencies"

# The workspace packages themselves are linked into node_modules, and a link
# whose target is outside the tree would dangle in the image. The links point
# at packages/*, so the package directories travel too (their compiled output
# only).
for pkg in "$web"/packages/*/; do
    name="$(basename "$pkg")"
    if [[ -d "$pkg/dist" ]]; then
        mkdir -p "$stage/packages/$name"
        cp -a "$pkg/package.json" "$stage/packages/$name/package.json"
        cp -a "$pkg/dist" "$stage/packages/$name/dist"
    fi
done

# Source maps are for a debugger, not for a browser: the wire protocol's
# alone are 28 MB of the staged tree, and the image serves only compiled code.
find "$stage" -name '*.map' -delete

echo "Staged web service:"
du -sh "$stage" "$stage/node_modules" "$stage/packages/bff/dist" \
    "$stage/packages/web/dist" 2>/dev/null
