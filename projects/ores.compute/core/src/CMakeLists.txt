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
set(name "ores.compute.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "messaging/registrar.cpp"
    "messaging/report_submit_handler.cpp"
    "repository/app_entity.cpp"
    "repository/app_mapper.cpp"
    "repository/app_repository.cpp"
    "repository/app_version_entity.cpp"
    "repository/app_version_mapper.cpp"
    "repository/app_version_platform_repository.cpp"
    "repository/app_version_repository.cpp"
    "repository/batch_entity.cpp"
    "repository/batch_mapper.cpp"
    "repository/batch_repository.cpp"
    "repository/compute_telemetry_repository.cpp"
    "repository/host_entity.cpp"
    "repository/host_mapper.cpp"
    "repository/host_repository.cpp"
    "repository/platform_entity.cpp"
    "repository/platform_repository.cpp"
    "repository/result_entity.cpp"
    "repository/result_mapper.cpp"
    "repository/result_repository.cpp"
    "repository/workflow_batch_link_repository.cpp"
    "repository/workunit_entity.cpp"
    "repository/workunit_mapper.cpp"
    "repository/workunit_repository.cpp"
    "service/app_service.cpp"
    "service/app_version_service.cpp"
    "service/batch_service.cpp"
    "service/host_service.cpp"
    "service/result_service.cpp"
    "service/workunit_service.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_COMPUTE_CORE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.compute/core/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.compute.api.lib
        ores.database.lib
        ores.nats.lib
        ores.dq.api.lib
    PRIVATE
        ores.service.lib
        ores.security.lib
        ores.utility.lib
        ores.logging.lib
        ores.platform.lib
        ores.reporting.api.lib
        faker-cxx::faker-cxx
        libfort::fort)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
