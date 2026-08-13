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
    "database_helper.cpp"
    "database_lifecycle_listener.cpp"
    "logging_listener.cpp"
    "make_generation_context.cpp"
    "project_root.cpp"
    "test_database_manager.cpp"
    "test_timeout_listener.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/database_helper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/database_lifecycle_listener.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/logging_listener.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/make_generation_context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/nats_options_helper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/ores.testing.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/project_root.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/run_coroutine_test.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/scoped_database_helper.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/scoped_environment_override.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/test_database_manager.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.testing/test_timeout_listener.hpp"
)
