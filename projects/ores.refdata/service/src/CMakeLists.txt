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
set(name "ores.refdata.service")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "app/application.cpp"
    "app/host.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
    "messaging/asset_class_code_event_registrar.cpp"
    "messaging/book_event_registrar.cpp"
    "messaging/book_purpose_type_event_registrar.cpp"
    "messaging/book_status_event_registrar.cpp"
    "messaging/business_centre_event_registrar.cpp"
    "messaging/business_day_convention_type_event_registrar.cpp"
    "messaging/business_unit_event_registrar.cpp"
    "messaging/business_unit_type_event_registrar.cpp"
    "messaging/calendar_event_registrar.cpp"
    "messaging/calendar_exception_event_registrar.cpp"
    "messaging/calendar_rule_event_registrar.cpp"
    "messaging/contact_type_event_registrar.cpp"
    "messaging/counterparty_contact_information_event_registrar.cpp"
    "messaging/counterparty_event_registrar.cpp"
    "messaging/counterparty_identifier_event_registrar.cpp"
    "messaging/country_event_registrar.cpp"
    "messaging/crm_driver_pair_event_registrar.cpp"
    "messaging/crm_enabled_derived_pair_event_registrar.cpp"
    "messaging/crm_topology_config_event_registrar.cpp"
    "messaging/currency_event_registrar.cpp"
    "messaging/currency_market_tier_event_registrar.cpp"
    "messaging/currency_pair_convention_event_registrar.cpp"
    "messaging/currency_pair_event_registrar.cpp"
    "messaging/curve_role_event_registrar.cpp"
    "messaging/day_count_fraction_type_event_registrar.cpp"
    "messaging/event_registrar.cpp"
    "messaging/floating_index_type_event_registrar.cpp"
    "messaging/instrument_code_event_registrar.cpp"
    "messaging/ledger_feed_type_event_registrar.cpp"
    "messaging/leg_type_event_registrar.cpp"
    "messaging/monetary_nature_event_registrar.cpp"
    "messaging/party_contact_information_event_registrar.cpp"
    "messaging/party_id_scheme_event_registrar.cpp"
    "messaging/party_identifier_event_registrar.cpp"
    "messaging/party_type_event_registrar.cpp"
    "messaging/payment_frequency_event_registrar.cpp"
    "messaging/portfolio_event_registrar.cpp"
    "messaging/purpose_type_event_registrar.cpp"
    "messaging/regulatory_book_type_event_registrar.cpp"
    "messaging/rounding_type_event_registrar.cpp"
    "messaging/tenor_anchor_event_registrar.cpp"
    "messaging/tenor_convention_event_registrar.cpp"
    "messaging/tenor_event_registrar.cpp"
    "messaging/tenor_kind_event_registrar.cpp"
    "messaging/tenor_resolution_algorithm_event_registrar.cpp"
    "messaging/tenor_unit_event_registrar.cpp"
)

set(lib_files ${files})
list(FILTER lib_files EXCLUDE REGEX "main.cpp")

add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_REFDATA_SERVICE_LIBRARY)
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.service.lib
        ores.security.lib
        ores.refdata.core.lib
        ores.eventing.core.lib
        ores.nats.lib
        ores.database.lib
        ores.utility.lib
        ores.telemetry.core.lib)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)

set(exe_binary_name ${name})
set(exe_target_name ${name}.exe)

set(exe_files ${files})
list(FILTER exe_files INCLUDE REGEX "main.cpp")

add_executable(${exe_target_name} ${exe_files})
set_target_properties(${exe_target_name} PROPERTIES
    OUTPUT_NAME ${exe_binary_name})

target_link_libraries(${exe_target_name}
    PRIVATE
        ${lib_target_name}
        ores.openssl_cleanup
        ${CMAKE_THREAD_LIBS_INIT})

install(TARGETS ${exe_target_name} RUNTIME DESTINATION bin)
