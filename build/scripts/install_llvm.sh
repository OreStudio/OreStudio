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
# Installs clang and the LLVM tools of one major version from apt.llvm.org on
# Ubuntu. The Ubuntu archive has an older point release of the same major, and
# the CI compiler must not change version without a commit that says so.
#
# The continuous build and the CI image both run this script, so the two get the
# same compiler.
#
# Usage:
#   ./install_llvm.sh [MAJOR]
#
# MAJOR defaults to 19. The tools go to /usr/lib/llvm-MAJOR/bin; put that
# directory first on PATH so that clang++ and llvm-cov resolve to them.
set -euo pipefail

major="${1:-19}"
sudo_cmd=""
if [[ "$(id -u)" -ne 0 ]]; then
    sudo_cmd="sudo"
fi
apt_opts=(-y -q -o Acquire::Retries=3 --no-install-recommends)
export DEBIAN_FRONTEND=noninteractive

${sudo_cmd} apt-get update -q -o Acquire::Retries=3
${sudo_cmd} apt-get install "${apt_opts[@]}" ca-certificates curl gnupg lsb-release

codename="$(lsb_release -cs)"
keyring=/usr/share/keyrings/llvm-archive-keyring.gpg
curl -fsSL https://apt.llvm.org/llvm-snapshot.gpg.key \
    | ${sudo_cmd} gpg --batch --yes --dearmor -o "${keyring}"
echo "deb [signed-by=${keyring}] https://apt.llvm.org/${codename}/ llvm-toolchain-${codename}-${major} main" \
    | ${sudo_cmd} tee "/etc/apt/sources.list.d/llvm-${major}.list" > /dev/null

${sudo_cmd} apt-get update -q -o Acquire::Retries=3
${sudo_cmd} apt-get install "${apt_opts[@]}" "clang-${major}" "llvm-${major}"

"/usr/lib/llvm-${major}/bin/clang++" --version
