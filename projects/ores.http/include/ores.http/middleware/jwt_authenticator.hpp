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
#ifndef ORES_HTTP_MIDDLEWARE_JWT_AUTHENTICATOR_HPP
#define ORES_HTTP_MIDDLEWARE_JWT_AUTHENTICATOR_HPP

#include <string>
#include <optional>
#include <expected>
#include "ores.http/domain/jwt_claims.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::http::middleware {

/**
 * @brief Error type for JWT validation failures.
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
 * @brief JWT authentication middleware.
 */
class jwt_authenticator final {
public:
    /**
     * @brief Creates an authenticator using a symmetric secret (HS256).
     */
    static jwt_authenticator create_hs256(const std::string& secret,
        const std::string& issuer = "",
        const std::string& audience = "");

    /**
     * @brief Creates an authenticator using an RSA public key (RS256).
     */
    static jwt_authenticator create_rs256(const std::string& public_key_pem,
        const std::string& issuer = "",
        const std::string& audience = "");

    /**
     * @brief Validates a JWT token and extracts claims.
     */
    std::expected<domain::jwt_claims, jwt_error> validate(
        const std::string& token) const;

    /**
     * @brief Creates a new JWT token with the given claims (HS256 only).
     */
    std::optional<std::string> create_token(const domain::jwt_claims& claims) const;

    /**
     * @brief Checks if the authenticator is properly configured.
     */
    bool is_configured() const { return configured_; }

private:
    jwt_authenticator() = default;

    inline static std::string_view logger_name = "ores.http.middleware.jwt_authenticator";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    bool configured_ = false;
    bool use_rsa_ = false;
    std::string secret_;
    std::string public_key_;
    std::string issuer_;
    std::string audience_;
};

/**
 * @brief Returns a string description for a JWT error.
 */
std::string to_string(jwt_error error);

}

#endif
