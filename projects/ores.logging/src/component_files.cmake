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
    "boost_severity.cpp"
    "lifecycle_manager.cpp"
    "logging_configuration.cpp"
    "logging_options.cpp"
    "logging_options_validator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/boost_severity.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/lifecycle_manager.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/logging_configuration.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/logging_exception.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/logging_options.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/logging_options_validator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/make_logger.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/ores.logging.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/scoped_attribute.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.logging/severity_level.hpp"
)
