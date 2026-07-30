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
    "FxSpotGenerationConfigMdiWindow.cpp"
    "FxSpotRateEditor.cpp"
    "GmmComponentController.cpp"
    "GmmComponentDetailDialog.cpp"
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
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientFxSpotGenerationConfigModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientGmmComponentModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientIrCurveGenerationConfigModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientIrCurveTemplateEntryModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientMarketDataGenerationConfigModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientYieldCurveProcessTypeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CurveShapePreviewChart.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FeedDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxSpotGenerationConfigController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxSpotGenerationConfigDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxSpotGenerationConfigMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxSpotRateEditor.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/GmmComponentController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/GmmComponentDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/GmmComponentMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveEditor.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveGenerationConfigController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveGenerationConfigDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveGenerationConfigHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveGenerationConfigMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveTemplateEntryController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveTemplateEntryDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IrCurveTemplateEntryMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MarketDataGenerationConfigController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MarketDataGenerationConfigDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MarketDataGenerationConfigMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MarketSimulatorWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ProcessTypeLabel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ReturnDistributionChart.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SamplePricePathsChart.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SampleShortRatePathsChart.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SyntheticPlugin.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/YieldCurveProcessTypeController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/YieldCurveProcessTypeDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/YieldCurveProcessTypeMdiWindow.hpp"
)
