# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
set(name "ores.iam.api")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "domain/account_contact_information_json_io.cpp"
    "domain/account_contact_information_table.cpp"
    "domain/account_contact_information_table_io.cpp"
    "domain/account_json.cpp"
    "domain/account_json_io.cpp"
    "domain/account_party_json_io.cpp"
    "domain/account_party_table.cpp"
    "domain/account_party_table_io.cpp"
    "domain/account_table.cpp"
    "domain/account_table_io.cpp"
    "domain/account_type_json_io.cpp"
    "domain/account_version_table.cpp"
    "domain/account_version_table_io.cpp"
    "domain/login_info_json.cpp"
    "domain/login_info_json_io.cpp"
    "domain/login_info_table.cpp"
    "domain/login_info_table_io.cpp"
    "domain/permission_json.cpp"
    "domain/permission_json_io.cpp"
    "domain/permission_table.cpp"
    "domain/permission_table_io.cpp"
    "domain/role_json.cpp"
    "domain/role_json_io.cpp"
    "domain/role_table.cpp"
    "domain/role_table_io.cpp"
    "domain/session_json_io.cpp"
    "domain/tenant_json_io.cpp"
    "domain/tenant_status_json_io.cpp"
    "domain/tenant_status_table.cpp"
    "domain/tenant_status_table_io.cpp"
    "domain/tenant_table.cpp"
    "domain/tenant_table_io.cpp"
    "domain/tenant_type_json_io.cpp"
    "domain/tenant_type_table.cpp"
    "domain/tenant_type_table_io.cpp"
    "generators/account_contact_information_generator.cpp"
    "generators/account_generator.cpp"
    "generators/account_party_generator.cpp"
    "generators/account_role_generator.cpp"
    "generators/login_info_generator.cpp"
    "generators/permission_generator.cpp"
    "generators/role_generator.cpp"
    "generators/session_generator.cpp"
    "generators/tenant_generator.cpp"
    "generators/tenant_status_generator.cpp"
    "generators/tenant_type_generator.cpp"
    "service/auth_session_service.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_IAM_API_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.iam/api/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.eventing.api.lib
        ores.platform.lib
    PRIVATE
        ores.utility.lib
        faker-cxx::faker-cxx
        libfort::fort)


target_link_libraries(${lib_target_name} PRIVATE ores.rfl.pch)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
