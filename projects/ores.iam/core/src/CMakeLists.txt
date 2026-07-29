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
find_package(Immer CONFIG REQUIRED)

set(name "ores.iam.core")
set(lib_binary_name ${name})
set(lib_target_name ${name}.lib)

set(files
    "domain/token_settings.cpp"
    "messaging/account_contact_information_registrar.cpp"
    "messaging/publish_from_dq_handler.cpp"
    "messaging/registrar.cpp"
    "presentation/account_contact_information_history_field_mapper.cpp"
    "repository/account_contact_information_entity.cpp"
    "repository/account_contact_information_mapper.cpp"
    "repository/account_contact_information_repository.cpp"
    "repository/account_entity.cpp"
    "repository/account_mapper.cpp"
    "repository/account_party_entity.cpp"
    "repository/account_party_mapper.cpp"
    "repository/account_party_repository.cpp"
    "repository/account_repository.cpp"
    "repository/account_role_entity.cpp"
    "repository/account_role_mapper.cpp"
    "repository/account_role_repository.cpp"
    "repository/auth_event_repository.cpp"
    "repository/login_info_entity.cpp"
    "repository/login_info_mapper.cpp"
    "repository/login_info_repository.cpp"
    "repository/permission_entity.cpp"
    "repository/permission_mapper.cpp"
    "repository/permission_repository.cpp"
    "repository/role_entity.cpp"
    "repository/role_mapper.cpp"
    "repository/role_permission_entity.cpp"
    "repository/role_permission_mapper.cpp"
    "repository/role_permission_repository.cpp"
    "repository/role_repository.cpp"
    "repository/session_entity.cpp"
    "repository/session_mapper.cpp"
    "repository/session_repository.cpp"
    "repository/tenant_entity.cpp"
    "repository/tenant_mapper.cpp"
    "repository/tenant_repository.cpp"
    "repository/tenant_status_entity.cpp"
    "repository/tenant_status_mapper.cpp"
    "repository/tenant_status_repository.cpp"
    "repository/tenant_type_entity.cpp"
    "repository/tenant_type_mapper.cpp"
    "repository/tenant_type_repository.cpp"
    "service/account_contact_information_service.cpp"
    "service/account_party_service.cpp"
    "service/account_service.cpp"
    "service/account_setup_service.cpp"
    "service/authorization_service.cpp"
    "service/bootstrap_mode_service.cpp"
    "service/database_name_service.cpp"
    "service/service_session_service.cpp"
    "service/signup_service.cpp"
    "service/tenant_status_service.cpp"
    "service/tenant_type_service.cpp"
)

set(lib_files ${files})
add_library(${lib_target_name} ${lib_files})
target_compile_definitions(${lib_target_name} PRIVATE ORES_IAM_CORE_LIBRARY)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(${lib_target_name} PRIVATE
        -fvisibility=hidden -fvisibility-inlines-hidden)
endif()
set_target_properties(${lib_target_name} PROPERTIES
    OUTPUT_NAME ${lib_binary_name})
set_target_properties(${lib_target_name}
    PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR})

target_include_directories(${lib_target_name} PUBLIC
    ${CMAKE_SOURCE_DIR}/projects/ores.iam/core/include)

target_link_libraries(${lib_target_name}
    PUBLIC
        ores.iam.api.lib
        ores.variability.core.lib
        ores.database.lib
        ores.dq.api.lib
        ores.eventing.api.lib
        ores.eventing.core.lib
        ores.geo.lib
        ores.platform.lib
        ores.security.lib
        ores.nats.lib
    PRIVATE
        ores.iam.client.lib
        ores.service.lib
        ores.utility.lib
        ores.refdata.api.lib
        faker-cxx::faker-cxx
        libfort::fort
        immer)

install(TARGETS ${lib_target_name} LIBRARY DESTINATION lib)
