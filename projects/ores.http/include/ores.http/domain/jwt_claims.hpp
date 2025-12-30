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
#ifndef ORES_HTTP_DOMAIN_JWT_CLAIMS_HPP
#define ORES_HTTP_DOMAIN_JWT_CLAIMS_HPP

#include <string>
#include <vector>
#include <chrono>
#include <optional>

namespace ores::http::domain {

/**
 * @brief Represents the claims extracted from a JWT token.
 */
struct jwt_claims final {
    /**
     * @brief Subject claim - typically the user ID.
     */
    std::string subject;

    /**
     * @brief Issuer of the token.
     */
    std::string issuer;

    /**
     * @brief Intended audience for the token.
     */
    std::string audience;

    /**
     * @brief Time when the token expires.
     */
    std::chrono::system_clock::time_point expires_at;

    /**
     * @brief Time when the token was issued.
     */
    std::chrono::system_clock::time_point issued_at;

    /**
     * @brief User roles/permissions.
     */
    std::vector<std::string> roles;

    /**
     * @brief Optional username claim.
     */
    std::optional<std::string> username;

    /**
     * @brief Optional email claim.
     */
    std::optional<std::string> email;
};

}

#endif
