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
# The fleet's transaction pooler, for a host that shares its PostgreSQL between
# environments.
#
# NATS reaches scratch because it is one static binary. pgbouncer links libevent,
# OpenSSL and c-ares, so it needs a libc and a loader: Alpine is the smallest
# base that carries it, and nothing of ours links into it, so musl is safe. The
# package is 1.24 or later, which is past the prepared-statement floor the
# statement task decided; Debian 12's 1.18 is below it and cannot be used.
#
# The image carries no configuration. The environment generates it, at
# /etc/pgbouncer/pgbouncer.ini, from the same script the host's unit runs, so
# there is one owner for the settings however the pooler is deployed.

FROM alpine:3.22

RUN apk add --no-cache --no-progress pgbouncer && \
    mkdir -p /etc/pgbouncer

# The configuration is a mount, so the image states only where it lives. The
# pooler logs to stderr, which is what the container runtime collects.
VOLUME ["/etc/pgbouncer"]

ENTRYPOINT ["/usr/bin/pgbouncer", "/etc/pgbouncer/pgbouncer.ini"]
CMD ["-q"]
