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
    "ArtefactTypeController.cpp"
    "ArtefactTypeDetailDialog.cpp"
    "ArtefactTypeMdiWindow.cpp"
    "BadgeDefinitionController.cpp"
    "BadgeDefinitionDetailDialog.cpp"
    "BadgeDefinitionHistoryDialog.cpp"
    "BadgeDefinitionMdiWindow.cpp"
    "BadgeMappingsTab.cpp"
    "BadgeSeverityController.cpp"
    "BadgeSeverityDetailDialog.cpp"
    "BadgeSeverityHistoryDialog.cpp"
    "BadgeSeverityMdiWindow.cpp"
    "CatalogController.cpp"
    "CatalogDetailDialog.cpp"
    "CatalogMdiWindow.cpp"
    "ChangeReasonCategoryController.cpp"
    "ChangeReasonCategoryDetailDialog.cpp"
    "ChangeReasonCategoryMdiWindow.cpp"
    "ChangeReasonController.cpp"
    "ChangeReasonDetailDialog.cpp"
    "ChangeReasonMdiWindow.cpp"
    "ClientArtefactTypeModel.cpp"
    "ClientBadgeDefinitionModel.cpp"
    "ClientBadgeSeverityModel.cpp"
    "ClientCatalogModel.cpp"
    "ClientChangeReasonCategoryModel.cpp"
    "ClientChangeReasonModel.cpp"
    "ClientCodeDomainModel.cpp"
    "ClientDataDomainModel.cpp"
    "ClientDatasetBundleModel.cpp"
    "ClientLeiEntityModel.cpp"
    "ClientLeiRelationshipModel.cpp"
    "CodeDomainController.cpp"
    "CodeDomainDetailDialog.cpp"
    "CodeDomainHistoryDialog.cpp"
    "CodeDomainMdiWindow.cpp"
    "DataDomainController.cpp"
    "DataDomainDetailDialog.cpp"
    "DataDomainMdiWindow.cpp"
    "DatasetBundleController.cpp"
    "DatasetBundleDetailDialog.cpp"
    "DatasetBundleMdiWindow.cpp"
    "DqPlugin.cpp"
    "LeiEntityController.cpp"
    "LeiEntityMdiWindow.cpp"
    "LeiRelationshipController.cpp"
    "LeiRelationshipMdiWindow.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ArtefactTypeController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ArtefactTypeDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ArtefactTypeMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeDefinitionMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeMappingsTab.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/BadgeSeverityMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CatalogController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CatalogDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CatalogMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ChangeReasonCategoryController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ChangeReasonCategoryDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ChangeReasonCategoryMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ChangeReasonController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ChangeReasonDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ChangeReasonMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientArtefactTypeModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientBadgeDefinitionModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientBadgeSeverityModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientCatalogModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientChangeReasonCategoryModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientChangeReasonModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientCodeDomainModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientDataDomainModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientDatasetBundleModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientLeiEntityModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/ClientLeiRelationshipModel.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainHistoryDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/CodeDomainMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DataDomainController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DataDomainDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DataDomainMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DatasetBundleController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DatasetBundleDetailDialog.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DatasetBundleMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/DqPlugin.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/LeiEntityController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/LeiEntityMdiWindow.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/LeiRelationshipController.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.qt/LeiRelationshipMdiWindow.hpp"
)
