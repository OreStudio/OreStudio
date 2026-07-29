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
set(name "ores.qt.api")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_API_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

# Qt code generation (MOC for Q_OBJECT, UIC for .ui forms)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

set(files
    "BadgeCache.cpp"
    "BadgeComboHelper.cpp"
    "BadgeLabelUtils.cpp"
    "CalendarAssignmentWidget.cpp"
    "ChangeReasonCache.cpp"
    "ChangeReasonDialog.cpp"
    "ChildEntityTableWidget.cpp"
    "ClientDatasetBundleModel.cpp"
    "ClientManager.cpp"
    "ClientManagerExportPortfolio.cpp"
    "ClientManagerTradeInstrument.cpp"
    "ClientManagerTrades.cpp"
    "ClientResultItemDelegate.cpp"
    "ClientResultModel.cpp"
    "ColourSwatchHelper.cpp"
    "CronEditorDialog.cpp"
    "CronExpressionWidget.cpp"
    "DelegatePaintUtils.cpp"
    "DetachableMdiSubWindow.cpp"
    "DetailDialogBase.cpp"
    "EntityController.cpp"
    "EntityItemDelegate.cpp"
    "EntityListMdiWindow.cpp"
    "FlagIconHelper.cpp"
    "FlagSelectorDialog.cpp"
    "HierarchyTreeWidget.cpp"
    "HistoryDialog.cpp"
    "HistoryDialogBase.cpp"
    "HostDisplayNameCache.cpp"
    "IconUtils.cpp"
    "ImageCache.cpp"
    "ImportEntityDialog.cpp"
    "LeiEntityPicker.cpp"
    "LookupFetcher.cpp"
    "MdiAreaWithBackground.cpp"
    "MdiUtils.cpp"
    "MessageBoxHelper.cpp"
    "OreBadgeComboBox.cpp"
    "OreCalendarComboBox.cpp"
    "OreCurrencyComboBox.cpp"
    "OrgDocViewerWindow.cpp"
    "PaginationWidget.cpp"
    "PartyPickerDialog.cpp"
    "PasswordMatchIndicator.cpp"
    "PluginBase.cpp"
    "PluginRegistry.cpp"
    "ProvenanceWidget.cpp"
    "QaValidationRunnerWidget.cpp"
    "RecencyPulseManager.cpp"
    "RelativeTimeHelper.cpp"
    "SettingGatedActionController.cpp"
    "TimestampFormat.cpp"
    "UiPersistence.cpp"
    "WorkspaceSelector.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${ORES_QT_API_DIR}/include/ores.qt/AbstractClientModel.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/BadgeCache.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/BadgeComboHelper.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/BadgeLabelUtils.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/BoolYesNoLabel.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/CalendarAssignmentWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ChangeReasonCache.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ChangeReasonDialog.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ChildEntityTableWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ClientDatasetBundleModel.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ClientManager.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ClientResultItemDelegate.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ClientResultModel.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ColorConstants.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ColourSwatchHelper.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ColumnMetadata.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/CronEditorDialog.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/CronExpressionWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/CronFieldWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/DelegatePaintUtils.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/DetachableMdiSubWindow.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/DetailDialogBase.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/DialogStyles.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/DynamicComboSetup.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/EntityController.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/EntityItemDelegate.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/EntityListMdiWindow.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ExceptionHelper.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/FlagIconHelper.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/FlagSelectorDialog.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/HierarchyTreeWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/HistoryDialog.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/HistoryDialogBase.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/HostDisplayNameCache.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/IBusinessUnitBrowser.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/IPlugin.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/IconUtils.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ImageCache.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ImportEntityDialog.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/LeiEntityPicker.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/LookupFetcher.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/MdiAreaWithBackground.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/MdiUtils.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/MessageBoxHelper.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/OreBadgeComboBox.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/OreCalendarComboBox.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/OreCurrencyComboBox.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/OrgDocViewerWindow.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/PaginationWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/PartyPickerDialog.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/PasswordMatchIndicator.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/PluginBase.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/PluginRegistry.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/ProvenanceWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/QaValidationRunnerWidget.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/RecencyPulseManager.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/RecencyTracker.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/RelativeTimeHelper.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/SettingGatedActionController.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/TextUtils.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/TimestampFormat.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/UiPersistence.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/WatermarkChartView.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/WidgetUtils.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/WorkspaceContext.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/WorkspaceSelector.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/export.hpp"
    "${ORES_QT_API_DIR}/include/ores.qt/plugin_context.hpp"
)
# UI forms for AUTOUIC.
file(GLOB FORMS "${ORES_QT_API_DIR}/ui/*.ui")

add_library(${lib_target_name} SHARED ${files} ${HEADERS} ${FORMS})

# Define ORES_QT_API_LIBRARY when building this target so export.hpp
# expands to BOOST_SYMBOL_EXPORT instead of BOOST_SYMBOL_IMPORT.
target_compile_definitions(${lib_target_name} PRIVATE ORES_QT_API_LIBRARY)

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    AUTOUIC_SEARCH_PATHS ${ORES_QT_API_DIR}/ui
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR})

# Export the ui/ directory path so consumers (e.g. ores.qt) can add it to their
# AUTOUIC_SEARCH_PATHS without hard-coding the source layout.
set(ORES_QT_API_UI_DIR "${ORES_QT_API_DIR}/ui" CACHE INTERNAL "ores.qt.api UI forms directory")

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_API_DIR}/include)
# The autogen include dir (ui_*.h) is only needed internally; keep it PRIVATE.
target_include_directories(${lib_target_name} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include")

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.qt.headless.lib
        ores.nats.lib
        ores.eventing.api.lib
        ores.iam.api.lib
        ores.history.api.lib
        ores.diff.lib
        ores.refdata.api.lib
        ores.compute.api.lib
        ores.scheduler.api.lib
        ores.dq.api.lib
        ores.http.api.lib
        ores.assets.api.lib
        ores.trading.api.lib
        ores.workspace.api.lib
        ores.variability.api.lib
        ores.orgmode.lib
        ores.logging.lib
        ores.utility.lib
        ores.platform.lib
        Qt6::Core
        Qt6::Gui
        Qt6::Widgets
        Qt6::Concurrent
        Qt6::Network
        Qt6::Svg)

install(TARGETS ${lib_target_name}
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin)  # .dll goes to bin/ on Windows
