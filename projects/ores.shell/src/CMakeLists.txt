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
set(name "ores.shell")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "app/application.cpp"
    "app/command_args.cpp"
    "app/command_feedback.cpp"
    "app/commands/account_parties_commands.cpp"
    "app/commands/accounts_commands.cpp"
    "app/commands/bundles_commands.cpp"
    "app/commands/change_reason_categories_commands.cpp"
    "app/commands/change_reasons_commands.cpp"
    "app/commands/connection_commands.cpp"
    "app/commands/countries_commands.cpp"
    "app/commands/crm_commands.cpp"
    "app/commands/currencies_commands.cpp"
    "app/commands/history_diff_renderer.cpp"
    "app/commands/lei_commands.cpp"
    "app/commands/marketdata_commands.cpp"
    "app/commands/navigation_commands.cpp"
    "app/commands/orgmode_commands.cpp"
    "app/commands/parties_commands.cpp"
    "app/commands/provision_commands.cpp"
    "app/commands/rbac_commands.cpp"
    "app/commands/reports_commands.cpp"
    "app/commands/script_commands.cpp"
    "app/commands/subscription_commands.cpp"
    "app/commands/synthetic_commands.cpp"
    "app/commands/tenants_commands.cpp"
    "app/commands/variability_commands.cpp"
    "app/commands/workflow_commands.cpp"
    "app/host.cpp"
    "app/pagination_context.cpp"
    "app/repl.cpp"
    "app/script_runner.cpp"
    "config/login_options.cpp"
    "config/options.cpp"
    "config/parser.cpp"
    "main.cpp"
)

set(lib_files ${files})
list(FILTER lib_files EXCLUDE REGEX "main.cpp")

add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_SHELL_LIBRARY)
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.shell/include)

find_package(cli CONFIG REQUIRED)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.telemetry.core.lib
        ores.utility.lib
    PRIVATE
        ores.history.api.lib
        ores.iam.api.lib
        ores.dq.api.lib
        ores.marketdata.api.lib
        ores.marketdata.client.lib
        ores.variability.api.lib
        ores.refdata.api.lib
        ores.reporting.api.lib
        ores.synthetic.api.lib
        ores.workflow.api.lib
        ores.nats.lib
        ores.orgmode.lib
        cli::cli)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)

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
