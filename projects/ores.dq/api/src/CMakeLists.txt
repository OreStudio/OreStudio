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
set(name "ores.dq.api")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "domain/artefact_type_json_io.cpp"
    "domain/badge_definition_json_io.cpp"
    "domain/badge_definition_table.cpp"
    "domain/badge_definition_table_io.cpp"
    "domain/badge_mapping_json_io.cpp"
    "domain/badge_mapping_table.cpp"
    "domain/badge_mapping_table_io.cpp"
    "domain/badge_severity_json_io.cpp"
    "domain/badge_severity_table.cpp"
    "domain/badge_severity_table_io.cpp"
    "domain/catalog_json_io.cpp"
    "domain/catalog_table.cpp"
    "domain/catalog_table_io.cpp"
    "domain/change_reason_category_json_io.cpp"
    "domain/change_reason_category_table.cpp"
    "domain/change_reason_category_table_io.cpp"
    "domain/change_reason_json_io.cpp"
    "domain/change_reason_table.cpp"
    "domain/change_reason_table_io.cpp"
    "domain/code_domain_json_io.cpp"
    "domain/code_domain_table.cpp"
    "domain/code_domain_table_io.cpp"
    "domain/coding_scheme_authority_type_json_io.cpp"
    "domain/coding_scheme_authority_type_table.cpp"
    "domain/coding_scheme_authority_type_table_io.cpp"
    "domain/coding_scheme_json_io.cpp"
    "domain/coding_scheme_table.cpp"
    "domain/coding_scheme_table_io.cpp"
    "domain/data_domain_json_io.cpp"
    "domain/data_domain_table.cpp"
    "domain/data_domain_table_io.cpp"
    "domain/dataset_bundle_json_io.cpp"
    "domain/dataset_bundle_member_json_io.cpp"
    "domain/dataset_bundle_member_table.cpp"
    "domain/dataset_bundle_member_table_io.cpp"
    "domain/dataset_bundle_table.cpp"
    "domain/dataset_bundle_table_io.cpp"
    "domain/dataset_dependency_json_io.cpp"
    "domain/dataset_dependency_table.cpp"
    "domain/dataset_dependency_table_io.cpp"
    "domain/dataset_json_io.cpp"
    "domain/dataset_table.cpp"
    "domain/dataset_table_io.cpp"
    "domain/fsm_state_json_io.cpp"
    "domain/methodology_json_io.cpp"
    "domain/methodology_table.cpp"
    "domain/methodology_table_io.cpp"
    "domain/nature_dimension_json_io.cpp"
    "domain/nature_dimension_table.cpp"
    "domain/nature_dimension_table_io.cpp"
    "domain/origin_dimension_json_io.cpp"
    "domain/origin_dimension_table.cpp"
    "domain/origin_dimension_table_io.cpp"
    "domain/publication_mode.cpp"
    "domain/subject_area_json_io.cpp"
    "domain/subject_area_table.cpp"
    "domain/subject_area_table_io.cpp"
    "domain/treatment_dimension_json_io.cpp"
    "domain/treatment_dimension_table.cpp"
    "domain/treatment_dimension_table_io.cpp"
    "generators/badge_definition_generator.cpp"
    "generators/badge_mapping_generator.cpp"
    "generators/badge_severity_generator.cpp"
    "generators/catalog_generator.cpp"
    "generators/change_reason_category_generator.cpp"
    "generators/change_reason_generator.cpp"
    "generators/code_domain_generator.cpp"
    "generators/coding_scheme_authority_type_generator.cpp"
    "generators/coding_scheme_generator.cpp"
    "generators/data_domain_generator.cpp"
    "generators/dataset_bundle_generator.cpp"
    "generators/dataset_bundle_member_generator.cpp"
    "generators/dataset_generator.cpp"
    "generators/methodology_generator.cpp"
    "generators/nature_dimension_generator.cpp"
    "generators/origin_dimension_generator.cpp"
    "generators/subject_area_generator.cpp"
    "generators/treatment_dimension_generator.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_DQ_API_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.dq/api/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.eventing.api.lib
        ores.platform.lib
    PRIVATE
        ores.utility.lib
        reflectcpp::reflectcpp
        faker-cxx::faker-cxx
        libfort::fort
        Boost::boost)


target_link_libraries(${lib_target_name} PRIVATE ores.rfl.pch)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
