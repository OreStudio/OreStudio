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
    "ClientFeedBindingModel.cpp"
    "CrmCrossRatesMatrixController.cpp"
    "CrmCrossRatesMatrixMdiWindow.cpp"
    "CrmRateCellWidget.cpp"
    "CrmRateSparklineWidget.cpp"
    "CurveSnapshotMdiWindow.cpp"
    "FeedBindingController.cpp"
    "FeedBindingDetailDialog.cpp"
    "FeedBindingMdiWindow.cpp"
    "MarketdataPlugin.cpp"
    "RateCurvesMdiWindow.cpp"
    "SyntheticBindingDialog.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientFeedBindingModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CrmCrossRatesMatrixController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CrmCrossRatesMatrixMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CrmRateCellWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CrmRateSparklineWidget.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CurveSnapshotMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FeedBindingController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FeedBindingDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/FeedBindingMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MarketdataExport.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/MarketdataPlugin.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/RateCurvesMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/SyntheticBindingDialog.hpp"
)
