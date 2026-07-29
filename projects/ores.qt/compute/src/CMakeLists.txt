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
set(name "ores.qt.compute")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_COMPUTE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

# Qt code generation (MOC for Q_OBJECT, UIC for .ui forms)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

set(files
    "AppController.cpp"
    "AppDetailDialog.cpp"
    "AppHistoryDialog.cpp"
    "AppMdiWindow.cpp"
    "AppProvisionerWizard.cpp"
    "AppVersionController.cpp"
    "AppVersionDetailDialog.cpp"
    "AppVersionHistoryDialog.cpp"
    "AppVersionMdiWindow.cpp"
    "BatchDetailDialog.cpp"
    "ClientAppModel.cpp"
    "ClientAppVersionModel.cpp"
    "ClientBatchModel.cpp"
    "ClientConcurrencyPolicyModel.cpp"
    "ClientHostModel.cpp"
    "ClientQueueModel.cpp"
    "ClientReportDefinitionModel.cpp"
    "ClientReportInstanceModel.cpp"
    "ClientReportTypeModel.cpp"
    "ClientWorkunitModel.cpp"
    "ComputeConsoleController.cpp"
    "ComputeConsoleWindow.cpp"
    "ComputeDashboardController.cpp"
    "ComputeDashboardMdiWindow.cpp"
    "ComputePlugin.cpp"
    "ComputeTaskViewModel.cpp"
    "ComputeTransferModel.cpp"
    "ConcurrencyPolicyController.cpp"
    "ConcurrencyPolicyDetailDialog.cpp"
    "ConcurrencyPolicyHistoryDialog.cpp"
    "ConcurrencyPolicyMdiWindow.cpp"
    "CreateQueueDialog.cpp"
    "QueueChartWindow.cpp"
    "QueueDetailDialog.cpp"
    "QueueMonitorController.cpp"
    "QueueMonitorMdiWindow.cpp"
    "ReportDefinitionController.cpp"
    "ReportDefinitionDetailDialog.cpp"
    "ReportDefinitionHistoryDialog.cpp"
    "ReportDefinitionMdiWindow.cpp"
    "ReportInstanceController.cpp"
    "ReportInstanceDetailDialog.cpp"
    "ReportInstanceHistoryDialog.cpp"
    "ReportInstanceMdiWindow.cpp"
    "ReportTypeController.cpp"
    "ReportTypeDetailDialog.cpp"
    "ReportTypeHistoryDialog.cpp"
    "ReportTypeMdiWindow.cpp"
    "ServiceDashboardController.cpp"
    "ServiceDashboardMdiWindow.cpp"
    "TransferProgressDelegate.cpp"
    "WorkunitDetailDialog.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppHistoryDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppProvisionerWizard.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppVersionController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppVersionDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppVersionHistoryDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/AppVersionMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/BatchDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientAppModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientAppVersionModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientBatchModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientConcurrencyPolicyModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientHostModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientQueueModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientReportDefinitionModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientReportInstanceModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientReportTypeModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ClientWorkunitModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ComputeConsoleController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ComputeConsoleWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ComputeDashboardController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ComputeDashboardMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ComputePlugin.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ComputeTaskViewModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ComputeTransferModel.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ConcurrencyPolicyController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ConcurrencyPolicyDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ConcurrencyPolicyHistoryDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ConcurrencyPolicyMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/CreateQueueDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/QueueChartWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/QueueDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/QueueMonitorController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/QueueMonitorMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportDefinitionController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportDefinitionDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportDefinitionHistoryDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportDefinitionMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportInstanceController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportInstanceDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportInstanceHistoryDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportInstanceMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportTypeController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportTypeDetailDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportTypeHistoryDialog.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ReportTypeMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ServiceDashboardController.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/ServiceDashboardMdiWindow.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/TransferProgressDelegate.hpp"
    "${ORES_QT_COMPUTE_DIR}/include/ores.qt/WorkunitDetailDialog.hpp"
)
# UI forms for AUTOUIC.
file(GLOB FORMS "${ORES_QT_COMPUTE_DIR}/ui/*.ui")

add_library(${lib_target_name} SHARED ${files} ${HEADERS} ${FORMS})
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    AUTOUIC_SEARCH_PATHS "${ORES_QT_COMPUTE_DIR}/ui;${ORES_QT_API_UI_DIR}"
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    LIBRARY_OUTPUT_DIRECTORY "${ORES_PLUGIN_OUTPUT_DIRECTORY}"
    INSTALL_RPATH "$ORIGIN/../lib:$ORIGIN"
    BUILD_RPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_COMPUTE_DIR}/include)
target_include_directories(${lib_target_name} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include"
    # Transient: compute controllers include headers still in ores.qt.
    # Remove as files migrate to their final projects.
    "${CMAKE_SOURCE_DIR}/projects/ores.qt/application/include"
    "${CMAKE_SOURCE_DIR}/projects/ores.qt/mktdata/include")

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.qt.api.lib
        ores.qt.mktdata.lib
        ores.compute.api.lib
        ores.scheduler.api.lib
        ores.reporting.api.lib
        ores.workflow.core.lib
        ores.ore.core.lib
        ores.ore.api.lib
        ores.controller.api.lib
        Qt6::Charts
    PRIVATE
        # Transient: many dialogs/controllers still live in ores.qt. Remove when files move.
        ores.qt.lib)

install(TARGETS ${lib_target_name}
    LIBRARY DESTINATION bin
    RUNTIME DESTINATION bin)
