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
    "BondInstrumentForm.cpp"
    "ClientTradeModel.cpp"
    "CommodityInstrumentForm.cpp"
    "CompositeInstrumentForm.cpp"
    "CompositeLegsWidget.cpp"
    "CreditInstrumentForm.cpp"
    "EquityInstrumentForm.cpp"
    "FxAccumulatorInstrumentForm.cpp"
    "FxAsianForwardInstrumentForm.cpp"
    "FxBarrierOptionInstrumentForm.cpp"
    "FxDigitalOptionInstrumentForm.cpp"
    "FxInstrumentForm.cpp"
    "FxVanillaOptionInstrumentForm.cpp"
    "FxVarianceSwapInstrumentForm.cpp"
    "ImportTradeDialog.cpp"
    "InstrumentFormRegistry.cpp"
    "InstrumentFormUtils.cpp"
    "OreDateEdit.cpp"
    "OreDoubleSpinBox.cpp"
    "OreImportController.cpp"
    "OreImportWizard.cpp"
    "OreImporter.cpp"
    "OrgExplorerMdiWindow.cpp"
    "OrgExplorerTradeModel.cpp"
    "OrgExplorerTreeModel.cpp"
    "PortfolioExplorerMdiWindow.cpp"
    "PortfolioExplorerTradeModel.cpp"
    "PortfolioExplorerTreeModel.cpp"
    "ScriptedInstrumentForm.cpp"
    "SwapInstrumentForm.cpp"
    "TradeController.cpp"
    "TradeDetailDialog.cpp"
    "TradeHistoryDialog.cpp"
    "TradeMdiWindow.cpp"
    "TradingPlugin.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AdaptiveStackedWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BondInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientTradeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CommodityInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CompositeInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CompositeLegsWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CreditInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/EquityInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxAccumulatorInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxAsianForwardInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxBarrierOptionInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxDigitalOptionInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxVanillaOptionInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FxVarianceSwapInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/IInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ImportTradeDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/InstrumentFormRegistry.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/InstrumentFormUtils.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OreDateEdit.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OreDoubleSpinBox.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OreImportController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OreImportWizard.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OreImporter.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OrgExplorerMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OrgExplorerTradeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/OrgExplorerTreeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PortfolioExplorerMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PortfolioExplorerTradeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PortfolioExplorerTreeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ScriptedInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SwapInstrumentForm.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TradeController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TradeDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TradeHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TradeMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/TradingPlugin.hpp"
)
