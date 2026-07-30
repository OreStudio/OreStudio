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
    "crypto/encryption.cpp"
    "crypto/password_hasher.cpp"
    "jwt/jwt_authenticator.cpp"
    "validation/email_validator.cpp"
    "validation/password_validator.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/crypto/encryption.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/crypto/password_hasher.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/jwt/boost_json_traits.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/jwt/jwt_authenticator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/jwt/jwt_claims.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/jwt/jwt_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/ores.security.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/validation/email_validator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/validation/password_validator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.security/validation/validation_result.hpp"
)
