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

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/application.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/command_args.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/command_feedback.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/account_parties_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/accounts_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/bundles_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/change_reason_categories_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/change_reasons_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/connection_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/countries_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/crm_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/currencies_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/history_diff_renderer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/lei_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/marketdata_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/navigation_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/orgmode_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/parties_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/provision_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/rbac_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/reports_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/script_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/subscription_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/tenants_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/variability_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/workflow_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/host.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/ores.shell.app.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/pagination_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/repl.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/script_runner.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/config/login_options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/config/options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/config/ores.shell.config.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/config/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/config/parser_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/ores.shell.hpp"
)
