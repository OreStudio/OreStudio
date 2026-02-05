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
set -e

with_valgrind=0

while [[ $# -gt 0 ]]; do
    case "$1" in
        --with-valgrind)
            with_valgrind=1
            shift
            ;;
        *)
            echo "Unknown argument: $1"
            echo "Usage: $0 [--with-valgrind]"
            exit 1
            ;;
    esac
done

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
