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
    "app/application.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/badge_definition_event_registrar.cpp"
    "messaging/badge_severity_event_registrar.cpp"
    "messaging/catalog_event_registrar.cpp"
    "messaging/change_reason_category_event_registrar.cpp"
    "messaging/change_reason_event_registrar.cpp"
    "messaging/code_domain_event_registrar.cpp"
    "messaging/data_domain_event_registrar.cpp"
    "messaging/dataset_bundle_event_registrar.cpp"
    "messaging/event_registrar.cpp"
    "messaging/subject_area_event_registrar.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/badge_definition_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/badge_severity_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/catalog_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/change_reason_category_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/change_reason_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/code_domain_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/data_domain_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/dataset_bundle_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/messaging/subject_area_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.dq.service/ores.dq.service.hpp"
)
