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
    "BadgeDefinitionController.cpp"
    "BadgeDefinitionDetailDialog.cpp"
    "BadgeDefinitionHistoryDialog.cpp"
    "BadgeDefinitionMdiWindow.cpp"
    "BadgeMappingsTab.cpp"
    "BadgeSeverityController.cpp"
    "BadgeSeverityDetailDialog.cpp"
    "BadgeSeverityHistoryDialog.cpp"
    "BadgeSeverityMdiWindow.cpp"
    "ClientBadgeDefinitionModel.cpp"
    "ClientBadgeSeverityModel.cpp"
    "ClientCodeDomainModel.cpp"
    "CodeDomainController.cpp"
    "CodeDomainDetailDialog.cpp"
    "CodeDomainHistoryDialog.cpp"
    "CodeDomainMdiWindow.cpp"
    "DqPlugin.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeMappingsTab.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientBadgeDefinitionModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientBadgeSeverityModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientCodeDomainModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DqPlugin.hpp"
)
