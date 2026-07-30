# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(files
    "config/nats_configuration.cpp"
    "domain/compression.cpp"
    "domain/correlation.cpp"
    "domain/wire_codec.cpp"
    "domain/wire_format.cpp"
    "service/client.cpp"
    "service/jetstream_admin.cpp"
    "service/jwks.cpp"
    "service/nats_client.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/config/nats_configuration.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/config/nats_options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/compression.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/consumer_info.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/correlation.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/headers.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/message.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/stream_info.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/stream_message.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/wire_codec.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/domain/wire_format.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/ores.nats.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/buffered_subscription.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/client.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/jetstream_admin.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/jwks.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/nats_client.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/nats_connect_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/request_helpers.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/retry.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/session_expired_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.nats/service/subscription.hpp"
)
