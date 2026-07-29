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
set(name "ores.qt.data_management")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)
set(ORES_QT_DATA_MANAGEMENT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)

# Qt code generation (MOC for Q_OBJECT, UIC for .ui forms)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

set(files
    "CatalogController.cpp"
    "CatalogDetailDialog.cpp"
    "CatalogHistoryDialog.cpp"
    "CatalogMdiWindow.cpp"
    "ChangeReasonCategoryController.cpp"
    "ChangeReasonCategoryDetailDialog.cpp"
    "ChangeReasonCategoryHistoryDialog.cpp"
    "ChangeReasonCategoryMdiWindow.cpp"
    "ChangeReasonController.cpp"
    "ChangeReasonDetailDialog.cpp"
    "ChangeReasonHistoryDialog.cpp"
    "ChangeReasonItemDelegate.cpp"
    "ChangeReasonMdiWindow.cpp"
    "ClientCatalogModel.cpp"
    "ClientChangeReasonCategoryModel.cpp"
    "ClientChangeReasonModel.cpp"
    "ClientCodingSchemeAuthorityTypeModel.cpp"
    "ClientCodingSchemeModel.cpp"
    "ClientDataDomainModel.cpp"
    "ClientDatasetDependencyModel.cpp"
    "ClientDatasetModel.cpp"
    "ClientMethodologyModel.cpp"
    "ClientNatureDimensionModel.cpp"
    "ClientOriginDimensionModel.cpp"
    "ClientSubjectAreaModel.cpp"
    "ClientTreatmentDimensionModel.cpp"
    "CodingSchemeAuthorityTypeController.cpp"
    "CodingSchemeAuthorityTypeDetailDialog.cpp"
    "CodingSchemeAuthorityTypeHistoryDialog.cpp"
    "CodingSchemeAuthorityTypeMdiWindow.cpp"
    "CodingSchemeController.cpp"
    "CodingSchemeDetailDialog.cpp"
    "CodingSchemeHistoryDialog.cpp"
    "CodingSchemeMdiWindow.cpp"
    "DataDomainController.cpp"
    "DataDomainDetailDialog.cpp"
    "DataDomainHistoryDialog.cpp"
    "DataDomainMdiWindow.cpp"
    "DataLibrarianWindow.cpp"
    "DataManagementPlugin.cpp"
    "DatasetBundleController.cpp"
    "DatasetBundleDetailDialog.cpp"
    "DatasetBundleHistoryDialog.cpp"
    "DatasetBundleMdiWindow.cpp"
    "DatasetController.cpp"
    "DatasetDetailDialog.cpp"
    "DatasetHistoryDialog.cpp"
    "DatasetItemDelegate.cpp"
    "DatasetMdiWindow.cpp"
    "DatasetViewDialog.cpp"
    "MethodologyController.cpp"
    "MethodologyDetailDialog.cpp"
    "MethodologyHistoryDialog.cpp"
    "MethodologyMdiWindow.cpp"
    "NatureDimensionController.cpp"
    "NatureDimensionDetailDialog.cpp"
    "NatureDimensionHistoryDialog.cpp"
    "NatureDimensionMdiWindow.cpp"
    "OriginDimensionController.cpp"
    "OriginDimensionDetailDialog.cpp"
    "OriginDimensionHistoryDialog.cpp"
    "OriginDimensionMdiWindow.cpp"
    "PublicationAuditDialog.cpp"
    "PublishBundleWizard.cpp"
    "PublishDatasetsDialog.cpp"
    "SubjectAreaController.cpp"
    "SubjectAreaDetailDialog.cpp"
    "SubjectAreaHistoryDialog.cpp"
    "SubjectAreaMdiWindow.cpp"
    "TreatmentDimensionController.cpp"
    "TreatmentDimensionDetailDialog.cpp"
    "TreatmentDimensionHistoryDialog.cpp"
    "TreatmentDimensionMdiWindow.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CatalogController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CatalogDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CatalogHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CatalogMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonCategoryController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonCategoryDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonCategoryHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonCategoryMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonItemDelegate.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ChangeReasonMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientCatalogModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientChangeReasonCategoryModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientChangeReasonModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientCodingSchemeAuthorityTypeModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientCodingSchemeModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientDataDomainModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientDatasetDependencyModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientDatasetModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientMethodologyModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientNatureDimensionModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientOriginDimensionModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientSubjectAreaModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/ClientTreatmentDimensionModel.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeAuthorityTypeController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeAuthorityTypeDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeAuthorityTypeHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeAuthorityTypeMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/CodingSchemeMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DataDomainController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DataDomainDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DataDomainHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DataDomainMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DataLibrarianWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DataManagementPlugin.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetBundleController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetBundleDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetBundleHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetBundleMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetItemDelegate.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/DatasetViewDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/MethodologyController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/MethodologyDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/MethodologyHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/MethodologyMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/NatureDimensionController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/NatureDimensionDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/NatureDimensionHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/NatureDimensionMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/OriginDimensionController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/OriginDimensionDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/OriginDimensionHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/OriginDimensionMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/PublicationAuditDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/PublishBundleWizard.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/PublishDatasetsDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/SubjectAreaController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/SubjectAreaDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/SubjectAreaHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/SubjectAreaMdiWindow.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/TreatmentDimensionController.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/TreatmentDimensionDetailDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/TreatmentDimensionHistoryDialog.hpp"
    "${ORES_QT_DATA_MANAGEMENT_DIR}/include/ores.qt/TreatmentDimensionMdiWindow.hpp"
)
# UI forms for AUTOUIC.
file(GLOB FORMS "${ORES_QT_DATA_MANAGEMENT_DIR}/ui/*.ui")

add_library(${lib_target_name} SHARED ${files} ${HEADERS} ${FORMS})
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()

set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name}
    AUTOUIC_SEARCH_PATHS "${ORES_QT_DATA_MANAGEMENT_DIR}/ui;${ORES_QT_API_UI_DIR}"
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    LIBRARY_OUTPUT_DIRECTORY "${ORES_PLUGIN_OUTPUT_DIRECTORY}"
    INSTALL_RPATH "$ORIGIN/../lib"
    BUILD_RPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")

target_include_directories(${lib_target_name} PUBLIC
    ${ORES_QT_DATA_MANAGEMENT_DIR}/include)
target_include_directories(${lib_target_name} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/${lib_target_name}_autogen/include")

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.qt.api.lib
        ores.qt.headless.lib
        ores.dq.api.lib
    PRIVATE
        # Transient: many dialogs/controllers still live in ores.qt. Remove when files move.
        ores.qt.lib
        ores.qt.workflow.lib)

install(TARGETS ${lib_target_name}
    LIBRARY DESTINATION bin
    RUNTIME DESTINATION bin)
