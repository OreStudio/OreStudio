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
    "domain/heading.cpp"
    "indexing/resolver.cpp"
    "parsing/parser.cpp"
)

# The headers are listed for the install and IDE targets.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/domain/document.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/domain/heading.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/domain/keyword.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/domain/link.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/domain/property.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/domain/resolved_target.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/domain/table.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/indexing/resolver.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/ores.orgmode.parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.orgmode.parser/parsing/parser.hpp"
)
