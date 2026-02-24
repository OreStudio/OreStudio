#!/bin/bash
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
# Installs system packages required to build OreStudio on Debian/Ubuntu.
#
# Usage:
#   ./install_debian_packages.sh [--full-install]
#
# Options:
#   --full-install    Full developer environment setup. Installs compilers
#                     (GCC, Clang), Ninja, CMake, PostgreSQL, Qt6, Valgrind,
#                     and all other build tools from the distro.
#                     Use this on a fresh Debian/Ubuntu box.
#
set -e

full_install=0

while [[ $# -gt 0 ]]; do
    case "$1" in
        --full-install)
            full_install=1
            shift
            ;;
        *)
            echo "Unknown argument: $1"
            echo "Usage: $0 [--full-install]"
            exit 1
            ;;
    esac
done

# ---------------------------------------------------------------------------
# Baseline packages (required by CI and local builds alike)
# These are X11/GL development headers needed to build Qt applications.
# ---------------------------------------------------------------------------
packages=(
    autoconf
    autoconf-archive
    automake
    fontconfig
    freeglut3-dev
    libegl1
    libltdl-dev
    libtool
    '^libxcb.*-dev'
    libx11-xcb-dev
    libglu1-mesa-dev
    libxrender-dev
    libxi-dev
    libxkbcommon-dev
    libxkbcommon-x11-dev
    libegl1-mesa-dev
    libxcursor-dev
    libxinerama-dev
    mesa-common-dev
    pkg-config
    xorg-dev
)

# Full install adds everything needed on a clean developer machine.
# (GitHub runners already have most of these pre-installed.)
if [[ $full_install -eq 1 ]]; then
    packages+=(
        # Core build toolchain
        build-essential
        gcc
        g++
        clang
        lld
        # Build system
        cmake
        ninja-build
        # Version control + download tools
        git
        curl
        wget
        ca-certificates
        # Archive tools (required by vcpkg)
        unzip
        zip
        tar
        # Additional tools used by vcpkg ports
        gperf
        nasm
        # Python (used by validate_schemas.sh)
        python3
        python3-pip
        python3-venv
        # OpenSSL (library headers + CLI for password generation)
        openssl
        libssl-dev
        # PostgreSQL (server + client library)
        postgresql
        postgresql-client
        libpq-dev
        # Qt6 from the distro
        qt6-base-dev
        qt6-tools-dev
        qt6-l10n-tools
        libqt6charts6-dev
        libqt6svg6-dev
        libqt6concurrent6t64
        # Memory analysis
        valgrind
    )
fi

install_packages() {
    sudo apt-get update -o Acquire::Retries=3 && \
    sudo apt-get install -y -o Acquire::Retries=3 "${packages[@]}"
}

max_retries=5
retry_delay=5

for ((attempt=1; attempt<=max_retries; attempt++)); do
    echo "Attempt $attempt of $max_retries..."
    if install_packages; then
        echo "Package installation succeeded."
        break
    fi

    if [[ $attempt -eq $max_retries ]]; then
        echo "Package installation failed after $max_retries attempts."
        exit 1
    fi

    echo "Package installation failed. Retrying in ${retry_delay}s..."
    sleep $retry_delay
    retry_delay=$((retry_delay * 2))
done

sudo apt-get clean
sudo apt-get autoremove -y

if [[ $full_install -eq 0 ]]; then
    exit 0
fi

# ---------------------------------------------------------------------------
# Post-install summary
# ---------------------------------------------------------------------------
echo ""
echo "======================================================================="
echo "  OreStudio developer environment installed successfully."
echo "======================================================================="
echo ""
echo "  Installed components:"
echo "    CMake   : $(cmake --version | head -1)"
echo "    Clang   : $(clang --version | head -1)"
echo "    GCC     : $(gcc --version | head -1)"
echo "    Ninja   : $(ninja --version)"
echo "    Postgres: $(psql --version)"
echo "    Qt6     : $(qmake6 --version 2>/dev/null | head -1 || echo 'see qt6-base-dev')"
echo "    Valgrind: $(valgrind --version)"
echo ""
echo "  Configure the project with:"
echo ""
echo "    cmake --preset linux-clang-debug"
echo "    cmake --build --preset linux-clang-debug"
echo ""
echo "  See projects/ores.sql/recreate_database.sh to set up the database."
echo "======================================================================="
