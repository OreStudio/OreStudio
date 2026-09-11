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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/headless)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/api)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/workspace)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/iam)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/compute)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/scheduler)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/refdata)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/dq)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/marketdata)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/mktdata)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/synthetic)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/analytics)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/trading)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/workflow)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/application)
