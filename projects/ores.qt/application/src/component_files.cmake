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
    "AboutDialog.cpp"
    "AddItemDialog.cpp"
    "ChangePasswordDialog.cpp"
    "ClientTelemetryLogModel.cpp"
    "CommandLineParser.cpp"
    "ConnectionBrowserMdiWindow.cpp"
    "ConnectionDetailPanel.cpp"
    "ConnectionItemDelegate.cpp"
    "ConnectionTreeModel.cpp"
    "EventViewerDialog.cpp"
    "HelpViewer.cpp"
    "LoginDialog.cpp"
    "MainWindow.cpp"
    "MasterPasswordDialog.cpp"
    "MyAccountDialog.cpp"
    "OreLogViewerWidget.cpp"
    "PartyProvisioningWizard.cpp"
    "ScriptEditorMdiWindow.cpp"
    "ScriptHighlighter.cpp"
    "ScriptLibraryPanel.cpp"
    "SessionAuditDialog.cpp"
    "ShellMdiWindow.cpp"
    "SignUpDialog.cpp"
    "SplashScreen.cpp"
    "SystemProvisionerWizard.cpp"
    "TagSelectorWidget.cpp"
    "TelemetryLogDelegate.cpp"
    "TelemetryMdiWindow.cpp"
    "TelemetrySettingsDialog.cpp"
    "TenantProvisioningWizard.cpp"
    "main.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AboutDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AddItemDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ChangePasswordDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientTelemetryLogModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CommandLineParser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ConnectionBrowserMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ConnectionDetailPanel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ConnectionItemDelegate.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ConnectionTreeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ConnectionTypes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/EventViewerDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/HelpViewer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/LoginDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/LogoLabel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MainWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MasterPasswordDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MyAccountDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OreLogViewerWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PartyProvisioningWizard.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ScriptEditorMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ScriptHighlighter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ScriptLibraryPanel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SessionAuditDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ShellMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SignUpDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SplashScreen.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SystemProvisionerWizard.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TagSelectorWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TelemetryLogDelegate.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TelemetryMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TelemetrySettingsDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantProvisioningWizard.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ores.qt.hpp"
)
