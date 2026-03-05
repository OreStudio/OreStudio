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
#ifndef ORES_SECURITY_JWT_JWT_AUTHENTICATOR_HPP
#define ORES_SECURITY_JWT_JWT_AUTHENTICATOR_HPP

#include <string>
#include <optional>
#include <expected>
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.security/jwt/jwt_error.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::security::jwt {

/**
 * @brief JWT authentication primitive.
 *
 * Supports HS256 (symmetric) for backward compatibility and RS256 (asymmetric)
 * for distributed service authentication. In RS256 mode, only the IAM service
 * holds the private key and mints tokens; all other services use the public key
 * to verify tokens independently.
 *
 * Factory methods:
 *   - create_hs256()         — symmetric signing and verification
 *   - create_rs256_signer()  — RS256 signing (IAM service only)
 *   - create_rs256_verifier()— RS256 verification (all other services)
 */
class jwt_authenticator final {
public:
    /**
     * @brief Creates an authenticator using a symmetric secret (HS256).
     *
     * Supports both token creation and validation.
     */
    static jwt_authenticator create_hs256(const std::string& secret,
        const std::string& issuer = "",
        const std::string& audience = "");

    /**
     * @brief Creates an RS256 signer using an RSA private key (PEM).
     *
     * Used by the IAM service to mint tokens. The private key must never
     * leave the IAM service configuration.
     */
    static jwt_authenticator create_rs256_signer(
        const std::string& private_key_pem,
        const std::string& issuer = "",
        const std::string& audience = "");

    /**
     * @brief Creates an RS256 verifier using an RSA public key (PEM).
     *
     * Used by all services other than IAM to validate tokens. Token creation
     * is not supported on this instance.
     */
    static jwt_authenticator create_rs256_verifier(
        const std::string& public_key_pem,
        const std::string& issuer = "",
        const std::string& audience = "");

    /**
     * @brief Validates a JWT token and extracts claims.
     */
    std::expected<jwt_claims, jwt_error> validate(const std::string& token) const;

    /**
     * @brief Creates a new JWT token with the given claims.
     *
     * Supported for HS256 and RS256 signer instances.
     * Returns nullopt if the authenticator is not configured for signing.
     */
    std::optional<std::string> create_token(const jwt_claims& claims) const;

    /**
     * @brief Checks if the authenticator is properly configured.
     */
    bool is_configured() const { return configured_; }

private:
    enum class algorithm_type { hs256, rs256_signer, rs256_verifier };

    jwt_authenticator() = default;

    inline static std::string_view logger_name =
        "ores.security.jwt.jwt_authenticator";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    bool configured_ = false;
    algorithm_type algorithm_ = algorithm_type::hs256;
    std::string secret_;         // HS256
    std::string private_key_;    // RS256 signer
    std::string public_key_;     // RS256 verifier
    std::string issuer_;
    std::string audience_;
};

}

#endif
