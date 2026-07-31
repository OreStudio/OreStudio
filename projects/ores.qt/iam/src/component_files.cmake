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
    "AccountChildEntityTables.cpp"
    "AccountContactInformationController.cpp"
    "AccountContactInformationDetailDialog.cpp"
    "AccountContactInformationMdiWindow.cpp"
    "AccountController.cpp"
    "AccountDetailDialog.cpp"
    "AccountHistoryDialog.cpp"
    "AccountItemDelegate.cpp"
    "AccountMdiWindow.cpp"
    "AccountPartiesWidget.cpp"
    "AccountRolesWidget.cpp"
    "AdminPlugin.cpp"
    "ClientAccountContactInformationModel.cpp"
    "ClientAccountModel.cpp"
    "ClientRoleModel.cpp"
    "ClientSystemSettingModel.cpp"
    "ClientTenantModel.cpp"
    "ClientTenantTypeModel.cpp"
    "OrgChartWidget.cpp"
    "RoleController.cpp"
    "RoleDetailDialog.cpp"
    "RoleMdiWindow.cpp"
    "SystemSettingController.cpp"
    "SystemSettingDetailDialog.cpp"
    "SystemSettingHistoryDialog.cpp"
    "SystemSettingItemDelegate.cpp"
    "SystemSettingMdiWindow.cpp"
    "TenantController.cpp"
    "TenantDetailDialog.cpp"
    "TenantHistoryDialog.cpp"
    "TenantMdiWindow.cpp"
    "TenantOnboardingWizard.cpp"
    "TenantTypeController.cpp"
    "TenantTypeDetailDialog.cpp"
    "TenantTypeHistoryDialog.cpp"
    "TenantTypeMdiWindow.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountChildEntityTables.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountContactInformationController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountContactInformationDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountContactInformationMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountItemDelegate.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountPartiesWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AccountRolesWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AdminPlugin.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientAccountContactInformationModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientAccountModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientRoleModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientSystemSettingModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientTenantModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientTenantTypeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IamExport.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OrgChartWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/RoleController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/RoleDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/RoleMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SystemSettingController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SystemSettingDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SystemSettingHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SystemSettingItemDelegate.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SystemSettingMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantOnboardingWizard.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantTypeController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantTypeDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantTypeHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TenantTypeMdiWindow.hpp"
)
