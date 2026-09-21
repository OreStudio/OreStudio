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
    "app/commands/iam/account_contact_information_commands.cpp"
    "app/commands/iam/account_history_operations_commands.cpp"
    "app/commands/iam/account_operations_commands.cpp"
    "app/commands/iam/account_type_commands.cpp"
    "app/commands/iam/authorization_operations_commands.cpp"
    "app/commands/iam/bootstrap_operations_commands.cpp"
    "app/commands/iam/login_operations_commands.cpp"
    "app/commands/iam/permission_commands.cpp"
    "app/commands/iam/reset_operations_commands.cpp"
    "app/commands/iam/role_commands.cpp"
    "app/commands/iam/session_operations_commands.cpp"
    "app/commands/iam/session_samples_operations_commands.cpp"
    "app/commands/iam/signup_operations_commands.cpp"
    "app/commands/iam/tenant_commands.cpp"
    "app/commands/iam/tenant_provisioning_operations_commands.cpp"
    "app/commands/iam/tenant_status_commands.cpp"
    "app/commands/iam/tenant_type_commands.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/account_contact_information_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/account_history_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/account_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/account_type_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/authorization_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/bootstrap_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/login_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/permission_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/reset_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/role_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/session_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/session_samples_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/signup_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/tenant_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/tenant_provisioning_operations_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/tenant_status_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/iam/tenant_type_commands.hpp"
)
