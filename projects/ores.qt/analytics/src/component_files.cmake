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
    "AnalyticsPlugin.cpp"
    "ClientPricingEngineTypeModel.cpp"
    "ClientPricingModelConfigModel.cpp"
    "ClientPricingModelProductModel.cpp"
    "ClientPricingModelProductParameterModel.cpp"
    "PricingEngineTypeController.cpp"
    "PricingEngineTypeDetailDialog.cpp"
    "PricingEngineTypeHistoryDialog.cpp"
    "PricingEngineTypeMdiWindow.cpp"
    "PricingModelConfigController.cpp"
    "PricingModelConfigDetailDialog.cpp"
    "PricingModelConfigHistoryDialog.cpp"
    "PricingModelConfigMdiWindow.cpp"
    "PricingModelProductController.cpp"
    "PricingModelProductDetailDialog.cpp"
    "PricingModelProductHistoryDialog.cpp"
    "PricingModelProductMdiWindow.cpp"
    "PricingModelProductParameterController.cpp"
    "PricingModelProductParameterDetailDialog.cpp"
    "PricingModelProductParameterHistoryDialog.cpp"
    "PricingModelProductParameterMdiWindow.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/AnalyticsPlugin.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientPricingEngineTypeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientPricingModelConfigModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientPricingModelProductModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientPricingModelProductParameterModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingEngineTypeController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingEngineTypeDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingEngineTypeHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingEngineTypeMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelConfigController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelConfigDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelConfigHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelConfigMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductParameterController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductParameterDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductParameterHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/PricingModelProductParameterMdiWindow.hpp"
)
