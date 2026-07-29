# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
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
set(name "ores.cli")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "app/application.cpp"
    "app/host.cpp"
    "config/add_account_options.cpp"
    "config/add_change_reason_category_options.cpp"
    "config/add_change_reason_options.cpp"
    "config/add_compute_app_options.cpp"
    "config/add_compute_app_version_options.cpp"
    "config/add_compute_batch_options.cpp"
    "config/add_compute_host_options.cpp"
    "config/add_compute_workunit_options.cpp"
    "config/add_country_options.cpp"
    "config/add_currency_options.cpp"
    "config/add_floating_index_type_options.cpp"
    "config/add_leg_type_options.cpp"
    "config/add_login_info_options.cpp"
    "config/add_options.cpp"
    "config/add_permission_options.cpp"
    "config/add_role_options.cpp"
    "config/add_system_setting_options.cpp"
    "config/delete_options.cpp"
    "config/domain_parsers/compute_parser.cpp"
    "config/domain_parsers/dq_parser.cpp"
    "config/domain_parsers/iam_parser.cpp"
    "config/domain_parsers/ore_parser.cpp"
    "config/domain_parsers/refdata_parser.cpp"
    "config/domain_parsers/trading_parser.cpp"
    "config/domain_parsers/variability_parser.cpp"
    "config/entity_parsers/accounts_parser.cpp"
    "config/entity_parsers/change_reason_categories_parser.cpp"
    "config/entity_parsers/change_reasons_parser.cpp"
    "config/entity_parsers/compute_app_versions_parser.cpp"
    "config/entity_parsers/compute_apps_parser.cpp"
    "config/entity_parsers/compute_batches_parser.cpp"
    "config/entity_parsers/compute_hosts_parser.cpp"
    "config/entity_parsers/compute_results_parser.cpp"
    "config/entity_parsers/compute_workunits_parser.cpp"
    "config/entity_parsers/countries_parser.cpp"
    "config/entity_parsers/currencies_parser.cpp"
    "config/entity_parsers/floating_index_types_parser.cpp"
    "config/entity_parsers/leg_types_parser.cpp"
    "config/entity_parsers/login_info_parser.cpp"
    "config/entity_parsers/permissions_parser.cpp"
    "config/entity_parsers/roles_parser.cpp"
    "config/entity_parsers/system_settings_parser.cpp"
    "config/export_options.cpp"
    "config/import_options.cpp"
    "config/list_options.cpp"
    "config/options.cpp"
    "config/ore_roundtrip_options.cpp"
    "config/parser.cpp"
    "config/parser_helpers.cpp"
    "main.cpp"
)

set(lib_files ${files})
list(FILTER lib_files EXCLUDE REGEX "main.cpp")

add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_CLI_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.cli/include)

find_package(cli CONFIG REQUIRED)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.database.lib
        ores.telemetry.core.lib
        ores.utility.lib
    PRIVATE
        ores.iam.core.lib
        ores.dq.core.lib
        ores.ore.core.lib
        ores.refdata.core.lib
        ores.variability.core.lib
        ores.security.lib
        ores.compute.core.lib
        ores.trading.core.lib
        magic_enum::magic_enum
        cli::cli)

install(TARGETS ${lib_target_name}
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)

set(exe_binary_name ${name})
set(exe_target_name ${name}.exe)

set(exe_files ${files})
list(FILTER exe_files INCLUDE REGEX "main.cpp")

add_executable(${exe_target_name} ${exe_files})
set_target_properties(${exe_target_name} PROPERTIES
    OUTPUT_NAME ${exe_binary_name})

target_include_directories(${exe_target_name}
    PRIVATE ${CMAKE_SOURCE_DIR}/projects)

target_link_libraries(${exe_target_name}
    PRIVATE
        ${lib_target_name}
        ores.openssl_cleanup
        ${CMAKE_THREAD_LIBS_INIT})

install(TARGETS ${exe_target_name} RUNTIME DESTINATION bin)

add_custom_target(ore-domain-roundtrip
    COMMAND $<TARGET_FILE:${exe_target_name}>
            ore roundtrip
            ${CMAKE_SOURCE_DIR}/external/ore/examples
            --output-dir ${CMAKE_SOURCE_DIR}/assets/test_data/domain_roundtrip
    DEPENDS ${exe_target_name}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Regenerating ORE domain roundtrip outputs"
)
