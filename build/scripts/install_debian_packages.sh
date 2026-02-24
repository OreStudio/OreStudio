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
#   ./install_debian_packages.sh [--with-valgrind] [--full-install] [--qt-dir DIR]
#
# Options:
#   --with-valgrind   Also install Valgrind (used by nightly builds).
#   --full-install    Full developer environment setup. Installs compilers
#                     (GCC, Clang 19), Ninja, CMake 3.28+, PostgreSQL 18,
#                     and Qt 6.8.3. Use this on a fresh Debian/Ubuntu box.
#   --qt-dir DIR      Qt installation directory (default: /opt/Qt).
#                     Only used with --full-install.
#
set -e

with_valgrind=0
full_install=0
qt_dir="/opt/Qt"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --with-valgrind)
            with_valgrind=1
            shift
            ;;
        --full-install)
            full_install=1
            shift
            ;;
        --qt-dir)
            qt_dir="$2"
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            echo "Usage: $0 [--with-valgrind] [--full-install] [--qt-dir DIR]"
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

if [[ $with_valgrind -eq 1 ]]; then
    packages+=(valgrind)
fi

# Full install adds essential build tools needed on a clean machine.
# (GitHub runners already have these pre-installed.)
if [[ $full_install -eq 1 ]]; then
    packages+=(
        # Core build toolchain
        build-essential
        gcc
        g++
        # Build system
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
        # Python (used by validate_schemas.sh and aqtinstall)
        python3
        python3-pip
        python3-venv
        # OpenSSL (library headers + CLI for password generation)
        openssl
        libssl-dev
        # lsb-release needed to detect distro codename
        lsb-release
        gnupg
        software-properties-common
        # Qt6 from the distro (may be older than the aqtinstall version but
        # provides system-wide tools and IDE integration)
        qt6-base-dev
        qt6-tools-dev
        qt6-l10n-tools
        libqt6charts6-dev
        libqt6svg6-dev
        libqt6concurrent6t64
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

# ---------------------------------------------------------------------------
# Full install: compilers, CMake, PostgreSQL, Qt
# These steps are only run when --full-install is specified.
# ---------------------------------------------------------------------------
if [[ $full_install -eq 0 ]]; then
    exit 0
fi

distro_codename=$(lsb_release -cs)
echo ""
echo "=== Full install mode: distro codename is '${distro_codename}' ==="

# ---------------------------------------------------------------------------
# CMake 3.28+ via Kitware APT
# Ubuntu 24.04 ships 3.28.3 which satisfies our minimum; still prefer the
# Kitware repo so developers get the latest patch release automatically.
# ---------------------------------------------------------------------------
echo ""
echo "=== Installing CMake from Kitware APT ==="
wget -qO /tmp/kitware-keyring.asc https://apt.kitware.com/keys/kitware-archive-latest.asc
gpg --dearmor < /tmp/kitware-keyring.asc \
    | sudo tee /usr/share/keyrings/kitware-archive-keyring.gpg > /dev/null
echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] \
https://apt.kitware.com/ubuntu/ ${distro_codename} main" \
    | sudo tee /etc/apt/sources.list.d/kitware.list > /dev/null
sudo apt-get update -o Acquire::Retries=3
sudo apt-get install -y cmake

# ---------------------------------------------------------------------------
# Clang 19 via LLVM APT
# Matches the version used in CI (KyleMayes/install-llvm-action@v2 "19").
# ---------------------------------------------------------------------------
echo ""
echo "=== Installing Clang 19 from LLVM APT ==="
wget -qO /tmp/llvm-archive-keyring.asc https://apt.llvm.org/llvm-snapshot.gpg.key
gpg --dearmor < /tmp/llvm-archive-keyring.asc \
    | sudo tee /usr/share/keyrings/llvm-archive-keyring.gpg > /dev/null
echo "deb [signed-by=/usr/share/keyrings/llvm-archive-keyring.gpg] \
http://apt.llvm.org/${distro_codename}/ llvm-toolchain-${distro_codename}-19 main" \
    | sudo tee /etc/apt/sources.list.d/llvm-19.list > /dev/null
sudo apt-get update -o Acquire::Retries=3
sudo apt-get install -y clang-19 clang++-19 lld-19 llvm-19

# Register Clang 19 with update-alternatives so it can be selected as the
# default clang/clang++ without hardcoding paths in CMake.
sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-19 100
sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-19 100

# ---------------------------------------------------------------------------
# PostgreSQL 18 via PGDG APT
# Matches the version used in CI (ikalnytskyi/action-setup-postgres@v8 "18").
# This only installs the server and client library; database initialisation
# (users, schemas, etc.) is done separately by recreate_database.sh.
# ---------------------------------------------------------------------------
echo ""
echo "=== Installing PostgreSQL 18 from PGDG APT ==="
sudo install -d /usr/share/postgresql-common/pgdg
sudo curl -fsSLo /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc \
    https://www.postgresql.org/media/keys/ACCC4CF8.asc
echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] \
https://apt.postgresql.org/pub/repos/apt ${distro_codename}-pgdg main" \
    | sudo tee /etc/apt/sources.list.d/pgdg.list > /dev/null
sudo apt-get update -o Acquire::Retries=3
sudo apt-get install -y postgresql-18 postgresql-client-18 libpq-dev

# ---------------------------------------------------------------------------
# Qt 6.8.3 via aqtinstall
# Matches the version and modules used in CI (jurplel/install-qt-action@v4).
# Installs to ${qt_dir}/6.8.3/gcc_64.
# ---------------------------------------------------------------------------
echo ""
echo "=== Installing Qt 6.8.3 to ${qt_dir} ==="
pip3 install --break-system-packages --quiet aqtinstall
sudo mkdir -p "${qt_dir}"
sudo chmod a+w "${qt_dir}"
python3 -m aqt install-qt linux desktop 6.8.3 gcc_64 \
    --outputdir "${qt_dir}" \
    -m qtcharts

qt_prefix="${qt_dir}/6.8.3/gcc_64"

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
echo "    Clang   : $(clang-19 --version | head -1)"
echo "    GCC     : $(gcc --version | head -1)"
echo "    Ninja   : $(ninja --version)"
echo "    Postgres: $(psql --version)"
echo "    Qt      : ${qt_prefix}"
echo ""
echo "  To use Qt with CMake, add the following to your shell profile"
echo "  (e.g. ~/.bashrc or ~/.zshrc):"
echo ""
echo "    export Qt6_DIR=${qt_prefix}/lib/cmake/Qt6"
echo "    export PATH=\${PATH}:${qt_prefix}/bin"
echo ""
echo "  Then configure the project with:"
echo ""
echo "    cmake --preset linux-clang-debug"
echo "    cmake --build --preset linux-clang-debug"
echo ""
echo "  See projects/ores.sql/recreate_database.sh to set up the database."
echo "======================================================================="
