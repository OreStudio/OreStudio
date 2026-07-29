# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
set(name "ores.dq.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "messaging/badge_definition_registrar.cpp"
    "messaging/badge_severity_registrar.cpp"
    "messaging/catalog_registrar.cpp"
    "messaging/change_reason_category_registrar.cpp"
    "messaging/change_reason_registrar.cpp"
    "messaging/code_domain_registrar.cpp"
    "messaging/data_domain_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "messaging/subject_area_registrar.cpp"
    "presentation/badge_definition_history_field_mapper.cpp"
    "presentation/badge_severity_history_field_mapper.cpp"
    "presentation/code_domain_history_field_mapper.cpp"
    "presentation/subject_area_history_field_mapper.cpp"
    "repository/artefact_type_entity.cpp"
    "repository/artefact_type_mapper.cpp"
    "repository/artefact_type_repository.cpp"
    "repository/badge_definition_entity.cpp"
    "repository/badge_definition_mapper.cpp"
    "repository/badge_definition_repository.cpp"
    "repository/badge_mapping_entity.cpp"
    "repository/badge_mapping_mapper.cpp"
    "repository/badge_mapping_repository.cpp"
    "repository/badge_severity_entity.cpp"
    "repository/badge_severity_mapper.cpp"
    "repository/badge_severity_repository.cpp"
    "repository/catalog_entity.cpp"
    "repository/catalog_mapper.cpp"
    "repository/catalog_repository.cpp"
    "repository/change_reason_category_entity.cpp"
    "repository/change_reason_category_mapper.cpp"
    "repository/change_reason_category_repository.cpp"
    "repository/change_reason_entity.cpp"
    "repository/change_reason_mapper.cpp"
    "repository/change_reason_repository.cpp"
    "repository/code_domain_entity.cpp"
    "repository/code_domain_mapper.cpp"
    "repository/code_domain_repository.cpp"
    "repository/coding_scheme_authority_type_entity.cpp"
    "repository/coding_scheme_authority_type_mapper.cpp"
    "repository/coding_scheme_authority_type_repository.cpp"
    "repository/coding_scheme_entity.cpp"
    "repository/coding_scheme_mapper.cpp"
    "repository/coding_scheme_repository.cpp"
    "repository/data_domain_entity.cpp"
    "repository/data_domain_mapper.cpp"
    "repository/data_domain_repository.cpp"
    "repository/dataset_bundle_entity.cpp"
    "repository/dataset_bundle_mapper.cpp"
    "repository/dataset_bundle_member_entity.cpp"
    "repository/dataset_bundle_member_mapper.cpp"
    "repository/dataset_bundle_member_repository.cpp"
    "repository/dataset_bundle_repository.cpp"
    "repository/dataset_dependency_entity.cpp"
    "repository/dataset_dependency_mapper.cpp"
    "repository/dataset_dependency_repository.cpp"
    "repository/dataset_entity.cpp"
    "repository/dataset_mapper.cpp"
    "repository/dataset_repository.cpp"
    "repository/fsm_state_entity.cpp"
    "repository/fsm_state_mapper.cpp"
    "repository/fsm_state_repository.cpp"
    "repository/fsm_transition_entity.cpp"
    "repository/fsm_transition_mapper.cpp"
    "repository/fsm_transition_repository.cpp"
    "repository/methodology_entity.cpp"
    "repository/methodology_mapper.cpp"
    "repository/methodology_repository.cpp"
    "repository/nature_dimension_entity.cpp"
    "repository/nature_dimension_mapper.cpp"
    "repository/nature_dimension_repository.cpp"
    "repository/origin_dimension_entity.cpp"
    "repository/origin_dimension_mapper.cpp"
    "repository/origin_dimension_repository.cpp"
    "repository/publication_mapper.cpp"
    "repository/publication_repository.cpp"
    "repository/subject_area_entity.cpp"
    "repository/subject_area_mapper.cpp"
    "repository/subject_area_repository.cpp"
    "repository/treatment_dimension_entity.cpp"
    "repository/treatment_dimension_mapper.cpp"
    "repository/treatment_dimension_repository.cpp"
    "service/badge_definition_service.cpp"
    "service/badge_service.cpp"
    "service/badge_severity_service.cpp"
    "service/catalog_service.cpp"
    "service/change_reason_category_service.cpp"
    "service/change_reason_service.cpp"
    "service/code_domain_service.cpp"
    "service/coding_scheme_service.cpp"
    "service/data_domain_service.cpp"
    "service/data_organization_service.cpp"
    "service/dataset_bundle_member_service.cpp"
    "service/dataset_bundle_service.cpp"
    "service/dataset_service.cpp"
    "service/dimension_service.cpp"
    "service/fsm_service.cpp"
    "service/publication_service.cpp"
    "service/subject_area_service.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_DQ_CORE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

if(MSVC)
    target_compile_options(${lib_target_name} PUBLIC /bigobj)
endif()

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.dq/core/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.dq.api.lib
        ores.nats.lib
        ores.database.lib
        ores.iam.api.lib
        ores.diff.lib
    PRIVATE
        ores.history.core.lib
        ores.service.lib
        ores.utility.lib
        ores.security.lib
        sqlgen::sqlgen
        libfort::fort
        Boost::graph
        Boost::boost)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
