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
    "app/commands/synthetic/folder_commands.cpp"
    "app/commands/synthetic/fx_spot_generation_config_commands.cpp"
    "app/commands/synthetic/gmm_component_commands.cpp"
    "app/commands/synthetic/ir_curve_generation_config_commands.cpp"
    "app/commands/synthetic/ir_curve_generation_config_process_parameter_value_commands.cpp"
    "app/commands/synthetic/ir_curve_template_entry_commands.cpp"
    "app/commands/synthetic/market_data_generation_config_commands.cpp"
    "app/commands/synthetic/yield_curve_process_parameter_definition_commands.cpp"
    "app/commands/synthetic/yield_curve_process_type_commands.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/folder_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/fx_spot_generation_config_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/gmm_component_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/ir_curve_generation_config_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/ir_curve_generation_config_process_parameter_value_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/ir_curve_template_entry_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/market_data_generation_config_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/yield_curve_process_parameter_definition_commands.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.shell/app/commands/synthetic/yield_curve_process_type_commands.hpp"
)
