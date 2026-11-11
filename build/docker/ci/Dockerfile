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
# The Linux CI image: the toolchain, the system packages and PostgreSQL with
# TimescaleDB that the continuous build otherwise installs at the start of every
# job. The versions match the ones that build installs, so a job in this image
# compiles with the same tools.
#
# The image holds no vcpkg packages. The build restores them from the actions
# cache in about a second, and an image without them does not go stale when
# vcpkg.json changes.
#
# Every version comes from versions.env, beside this file. Build the image with
# build/docker/ci/build.sh, which passes them as build arguments, tags the image
# and checks the tools in it. To change a version, change versions.env only.
ARG UBUNTU_VERSION
FROM ubuntu:${UBUNTU_VERSION}

ARG LLVM_MAJOR
ARG GCC_MAJOR
ARG CMAKE_VERSION
ARG NINJA_VERSION
ARG NODE_VERSION
ARG PG_MAJOR

# Stop at once when a version is missing, which happens when the image is built
# without build.sh.
RUN : "${LLVM_MAJOR:?}" "${GCC_MAJOR:?}" "${CMAKE_VERSION:?}" \
      "${NINJA_VERSION:?}" "${NODE_VERSION:?}" "${PG_MAJOR:?}"

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8

# The baseline of compass env install-packages, plus the tools that the GitHub
# runner image has and the build uses: a C compiler for the vcpkg ports, git,
# archive tools, Python for the scripts, sudo for the scripts that call it, and
# libraries that the vcpkg ports link against. Clang takes the C++ standard
# library of the newest GCC it finds; g++-${GCC_MAJOR} gives it the same
# libstdc++ as the runner image.
RUN apt-get update -q -o Acquire::Retries=3 \
 && apt-get install -y -q -o Acquire::Retries=3 --no-install-recommends \
        autoconf autoconf-archive automake bison build-essential \
        ca-certificates curl file flex "g++-${GCC_MAJOR}" gnupg gperf git libltdl-dev libssl-dev \
        libtool linux-libc-dev lsb-release nasm openssl pkg-config python3 \
        python3-pip python3-venv sudo tar unzip xz-utils zip zstd \
 && rm -rf /var/lib/apt/lists/*

COPY build/scripts/install_llvm.sh /tmp/install_llvm.sh
RUN /tmp/install_llvm.sh "${LLVM_MAJOR}" \
 && rm -rf /tmp/install_llvm.sh /var/lib/apt/lists/*
ENV PATH=/usr/lib/llvm-${LLVM_MAJOR}/bin:${PATH}

RUN curl -fsSL "https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}-linux-x86_64.tar.gz" \
        | tar -xz -C /opt \
 && ln -s "/opt/cmake-${CMAKE_VERSION}-linux-x86_64/bin/"* /usr/local/bin/ \
 && curl -fsSL -o /tmp/ninja.zip "https://github.com/ninja-build/ninja/releases/download/v${NINJA_VERSION}/ninja-linux.zip" \
 && unzip -q /tmp/ninja.zip -d /usr/local/bin \
 && rm /tmp/ninja.zip

RUN curl -fsSL "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-x64.tar.xz" \
        | tar -xJ -C /opt \
 && ln -s "/opt/node-v${NODE_VERSION}-linux-x64/bin/"* /usr/local/bin/

# The same PGDG repository and packages as setup_postgres_extensions.sh.
RUN curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc \
        | gpg --batch --dearmor -o /usr/share/keyrings/postgresql-keyring.gpg \
 && echo "deb [signed-by=/usr/share/keyrings/postgresql-keyring.gpg] https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
        > /etc/apt/sources.list.d/pgdg.list \
 && apt-get update -q -o Acquire::Retries=3 \
 && apt-get install -y -q -o Acquire::Retries=3 --no-install-recommends \
        "postgresql-${PG_MAJOR}" "postgresql-${PG_MAJOR}-timescaledb" \
        "postgresql-client-${PG_MAJOR}" libpq-dev \
 && rm -rf /var/lib/apt/lists/*
ENV PATH=/usr/lib/postgresql/${PG_MAJOR}/bin:${PATH}
