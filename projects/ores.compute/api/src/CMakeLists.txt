# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
set(name "ores.compute.api")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "domain/app_json_io.cpp"
    "domain/app_table.cpp"
    "domain/app_table_io.cpp"
    "domain/app_version_json_io.cpp"
    "domain/app_version_platform_json_io.cpp"
    "domain/app_version_table.cpp"
    "domain/app_version_table_io.cpp"
    "domain/batch_json_io.cpp"
    "domain/batch_table.cpp"
    "domain/batch_table_io.cpp"
    "domain/host_json_io.cpp"
    "domain/host_table.cpp"
    "domain/host_table_io.cpp"
    "domain/result_json_io.cpp"
    "domain/result_table.cpp"
    "domain/result_table_io.cpp"
    "domain/workunit_json_io.cpp"
    "domain/workunit_table.cpp"
    "domain/workunit_table_io.cpp"
    "generators/app_generator.cpp"
    "generators/app_version_generator.cpp"
    "generators/batch_generator.cpp"
    "generators/host_generator.cpp"
    "generators/result_generator.cpp"
    "generators/workunit_generator.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_COMPUTE_API_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.compute/api/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.storage.lib
        ores.platform.lib
    PRIVATE
        ores.utility.lib
        faker-cxx::faker-cxx
        libfort::fort)


target_link_libraries(${lib_target_name} PRIVATE ores.rfl.pch)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
