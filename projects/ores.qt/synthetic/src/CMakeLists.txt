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
set(name "ores.qt.synthetic")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_SYNTHETIC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

# Qt code generation (MOC for Q_OBJECT, UIC for .ui forms)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

set(files
    "ClientFxSpotGenerationConfigModel.cpp"
    "ClientGmmComponentModel.cpp"
    "ClientIrCurveGenerationConfigModel.cpp"
    "ClientIrCurveTemplateEntryModel.cpp"
    "ClientMarketDataGenerationConfigModel.cpp"
    "ClientYieldCurveProcessTypeModel.cpp"
    "CurveShapePreviewChart.cpp"
    "FeedDialog.cpp"
    "FxSpotGenerationConfigController.cpp"
    "FxSpotGenerationConfigDetailDialog.cpp"
    "FxSpotGenerationConfigHistoryDialog.cpp"
    "FxSpotGenerationConfigMdiWindow.cpp"
    "FxSpotRateEditor.cpp"
    "GmmComponentController.cpp"
    "GmmComponentDetailDialog.cpp"
    "GmmComponentHistoryDialog.cpp"
    "GmmComponentMdiWindow.cpp"
    "IrCurveEditor.cpp"
    "IrCurveGenerationConfigController.cpp"
    "IrCurveGenerationConfigDetailDialog.cpp"
    "IrCurveGenerationConfigHistoryDialog.cpp"
    "IrCurveGenerationConfigMdiWindow.cpp"
    "IrCurveTemplateEntryController.cpp"
    "IrCurveTemplateEntryDetailDialog.cpp"
    "IrCurveTemplateEntryMdiWindow.cpp"
    "MarketDataGenerationConfigController.cpp"
    "MarketDataGenerationConfigDetailDialog.cpp"
    "MarketDataGenerationConfigHistoryDialog.cpp"
    "MarketDataGenerationConfigMdiWindow.cpp"
    "MarketSimulatorWindow.cpp"
    "ReturnDistributionChart.cpp"
    "SamplePricePathsChart.cpp"
    "SampleShortRatePathsChart.cpp"
    "SyntheticPlugin.cpp"
    "YieldCurveProcessTypeController.cpp"
    "YieldCurveProcessTypeDetailDialog.cpp"
    "YieldCurveProcessTypeMdiWindow.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ClientFxSpotGenerationConfigModel.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ClientGmmComponentModel.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ClientIrCurveGenerationConfigModel.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ClientIrCurveTemplateEntryModel.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ClientMarketDataGenerationConfigModel.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ClientYieldCurveProcessTypeModel.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/CurveShapePreviewChart.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/FeedDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/FxSpotGenerationConfigController.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/FxSpotGenerationConfigDetailDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/FxSpotGenerationConfigHistoryDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/FxSpotGenerationConfigMdiWindow.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/FxSpotRateEditor.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/GmmComponentController.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/GmmComponentDetailDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/GmmComponentHistoryDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/GmmComponentMdiWindow.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveEditor.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveGenerationConfigController.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveGenerationConfigDetailDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveGenerationConfigHistoryDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveGenerationConfigMdiWindow.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveTemplateEntryController.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveTemplateEntryDetailDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/IrCurveTemplateEntryMdiWindow.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/MarketDataGenerationConfigController.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/MarketDataGenerationConfigDetailDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/MarketDataGenerationConfigHistoryDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/MarketDataGenerationConfigMdiWindow.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/MarketSimulatorWindow.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ProcessTypeLabel.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/ReturnDistributionChart.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/SamplePricePathsChart.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/SampleShortRatePathsChart.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/SyntheticPlugin.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/YieldCurveProcessTypeController.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/YieldCurveProcessTypeDetailDialog.hpp"
    "${ORES_QT_SYNTHETIC_DIR}/include/ores.qt/YieldCurveProcessTypeMdiWindow.hpp"
)
# UI forms for AUTOUIC.
file(GLOB FORMS "${ORES_QT_SYNTHETIC_DIR}/ui/*.ui")

add_library(${lib_target_name} SHARED ${files} ${HEADERS} ${FORMS})
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    AUTOUIC_SEARCH_PATHS "${ORES_QT_SYNTHETIC_DIR}/ui;${ORES_QT_API_UI_DIR}"
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    LIBRARY_OUTPUT_DIRECTORY "${ORES_PLUGIN_OUTPUT_DIRECTORY}"
    INSTALL_RPATH "$ORIGIN/../lib:$ORIGIN"
    BUILD_RPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_SYNTHETIC_DIR}/include)
target_include_directories(${lib_target_name} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include"
    # Transient: controllers include headers still in ores.qt.
    "${CMAKE_SOURCE_DIR}/projects/ores.qt/application/include")

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.qt.api.lib
        ores.analytics.quant.lib
        ores.synthetic.api.lib
        ores.marketdata.api.lib
        ores.dq.api.lib
        ores.storage.lib
        Qt6::Charts
    PRIVATE
        # Transient: many dialogs/controllers still live in ores.qt.
        ores.qt.lib
        ores.marketdata.core.lib)

install(TARGETS ${lib_target_name}
    LIBRARY DESTINATION bin
    RUNTIME DESTINATION bin)
