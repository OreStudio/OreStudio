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
# job.
#
# Every build tool comes from the Ubuntu image itself, at the version that image
# ships: no third-party repository, no compiler PPA, no downloaded archive. A
# developer on the same Ubuntu release installs the same packages, so a warning
# the CI reports is a warning anyone can reproduce.
#
# PostgreSQL is the single exception, and the reason is below: the archive build
# of it fails this project's queries.
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

# Links the package in GitHub Container Registry to this repository, so the
# repository's workflows can pull it and its page lists it.
LABEL org.opencontainers.image.source=https://github.com/OreStudio/OreStudio

ARG LLVM_MAJOR
ARG GCC_MAJOR
ARG PG_MAJOR

# Stop at once when a version is missing, which happens when the image is built
# without build.sh.
RUN : "${LLVM_MAJOR:?}" "${GCC_MAJOR:?}" "${PG_MAJOR:?}"

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8

# The baseline of compass env install-packages, plus the tools that the build
# uses: a C compiler for the vcpkg ports, git, archive tools, Python for the
# scripts, sudo for the scripts that call it, valgrind for the nightly memcheck,
# the coverage runtime, and libraries that the vcpkg ports link against. Clang
# takes the C++ standard library of the newest GCC it finds, and
# g++-${GCC_MAJOR} gives it that libstdc++.
RUN apt-get update -q -o Acquire::Retries=3 \
 && apt-get install -y -q -o Acquire::Retries=3 --no-install-recommends \
        autoconf autoconf-archive automake bison build-essential \
        ca-certificates curl file flex gnupg gperf git \
        "clang-${LLVM_MAJOR}" "clang-tidy-${LLVM_MAJOR}" "llvm-${LLVM_MAJOR}" \
        "libclang-rt-${LLVM_MAJOR}-dev" \
        cmake "g++-${GCC_MAJOR}" gh libltdl-dev libssl-dev libtool \
        linux-libc-dev lsb-release nasm ninja-build nodejs npm openssl \
        pkg-config python3 procps python3-pip python3-venv sudo tar tzdata \
        unzip valgrind xz-utils zip zstd \
 && rm -rf /var/lib/apt/lists/*

# The gcc preset names g++ and takes it from PATH, so the versioned pair the
# file states becomes the default through the first directory on PATH.
RUN ln -sf "/usr/bin/gcc-${GCC_MAJOR}" /usr/local/bin/gcc \
 && ln -sf "/usr/bin/g++-${GCC_MAJOR}" /usr/local/bin/g++

# PostgreSQL is the one package the image does not take from Ubuntu. The
# archive carries 18.6 with TimescaleDB 2.25, and the queries this project runs
# fail on that pair with a planner error, "variable not found in subplan target
# list". PGDG carries 18.7 with TimescaleDB 2.31, which the CI ran green on.
# The package creates the main cluster, and build/scripts/start_ci_postgres.sh
# starts it in the build jobs.
RUN curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc \
        | gpg --batch --dearmor -o /usr/share/keyrings/postgresql-keyring.gpg \
 && echo "deb [signed-by=/usr/share/keyrings/postgresql-keyring.gpg] https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
        > /etc/apt/sources.list.d/pgdg.list \
 && apt-get update -q -o Acquire::Retries=3 \
 && apt-get install -y -q -o Acquire::Retries=3 --no-install-recommends \
        "postgresql-${PG_MAJOR}" "postgresql-${PG_MAJOR}-timescaledb" \
        "postgresql-client-${PG_MAJOR}" libpq-dev \
 && rm -rf /var/lib/apt/lists/* \
 && echo "shared_preload_libraries = 'timescaledb'" \
        > "/etc/postgresql/${PG_MAJOR}/main/conf.d/timescaledb.conf"

ENV PATH=/usr/lib/llvm-${LLVM_MAJOR}/bin:/usr/lib/postgresql/${PG_MAJOR}/bin:${PATH}
