/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SECURITY_JWT_JWT_ERROR_HPP
#define ORES_SECURITY_JWT_JWT_ERROR_HPP

#include <string>

namespace ores::security::jwt {

/**
 * @brief Error type for JWT validation and signing failures.
 */
enum class jwt_error {
    invalid_token,
    expired_token,
    invalid_signature,
    missing_claims,
    invalid_issuer,
    invalid_audience
};

/**
 * @brief Returns a string description for a JWT error.
 */
std::string to_string(jwt_error error);

}

#endif
