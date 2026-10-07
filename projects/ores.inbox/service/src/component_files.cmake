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
# AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
# Template: cmake_component_files_src.mustache
# To modify, update the template and regenerate.
set(files
    "app/application.cpp"
    "app/approval_expiry_sweeper.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/approval_decision_event_registrar.cpp"
    "messaging/approval_decision_type_event_registrar.cpp"
    "messaging/approval_kind_event_registrar.cpp"
    "messaging/approval_request_event_registrar.cpp"
    "messaging/approval_request_state_event_registrar.cpp"
    "messaging/delivery_outcome_type_event_registrar.cpp"
    "messaging/notification_channel_event_registrar.cpp"
    "messaging/notification_delivery_event_registrar.cpp"
    "messaging/notification_event_registrar.cpp"
    "messaging/notification_kind_event_registrar.cpp"
    "messaging/notification_preference_event_registrar.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/app/application_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/app/approval_expiry_sweeper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/approval_decision_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/approval_decision_type_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/approval_kind_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/approval_request_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/approval_request_state_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/delivery_outcome_type_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/notification_channel_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/notification_delivery_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/notification_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/notification_kind_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/messaging/notification_preference_event_registrar.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.inbox.service/ores.inbox.service.hpp"
)
