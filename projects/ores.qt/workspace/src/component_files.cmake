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
    "ClientWorkspaceModel.cpp"
    "WorkspaceController.cpp"
    "WorkspaceDetailDialog.cpp"
    "WorkspaceHistoryDialog.cpp"
    "WorkspaceMdiWindow.cpp"
    "WorkspacePlugin.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientWorkspaceModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/WorkspaceController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/WorkspaceDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/WorkspaceHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/WorkspaceMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/WorkspacePlugin.hpp"
)
