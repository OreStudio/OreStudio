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
    "domain/http_request.cpp"
    "domain/http_response.cpp"
    "domain/route.cpp"
    "net/http_server.cpp"
    "net/http_server_options.cpp"
    "net/http_session.cpp"
    "net/router.cpp"
    "openapi/endpoint_registry.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/domain/http_method.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/domain/http_request.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/domain/http_response.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/domain/http_status.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/domain/route.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/messaging/http_info_protocol.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/net/http_server.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/net/http_server_options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/net/http_session.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/net/router.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.http.api/openapi/endpoint_registry.hpp"
)
