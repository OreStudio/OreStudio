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
#include "ores.http/middleware/jwt_authenticator.hpp"
#include "ores.http/middleware/boost_json_traits.hpp"

#include <jwt-cpp/jwt.h>
#include <boost/json.hpp>

namespace ores::http::middleware {

using json_traits = jwt::traits::boost_json;

using namespace ores::telemetry::log;

jwt_authenticator jwt_authenticator::create_hs256(const std::string& secret,
    const std::string& issuer,
    const std::string& audience) {

    jwt_authenticator auth;
    auth.secret_ = secret;
    auth.issuer_ = issuer;
    auth.audience_ = audience;
    auth.use_rsa_ = false;
    auth.configured_ = !secret.empty();

    BOOST_LOG_SEV(lg(), info) << "JWT authenticator configured with HS256"
        << ", issuer: " << (issuer.empty() ? "(none)" : issuer)
        << ", audience: " << (audience.empty() ? "(none)" : audience);

    return auth;
}

jwt_authenticator jwt_authenticator::create_rs256(const std::string& public_key_pem,
    const std::string& issuer,
    const std::string& audience) {

    jwt_authenticator auth;
    auth.public_key_ = public_key_pem;
    auth.issuer_ = issuer;
    auth.audience_ = audience;
    auth.use_rsa_ = true;
    auth.configured_ = !public_key_pem.empty();

    BOOST_LOG_SEV(lg(), info) << "JWT authenticator configured with RS256"
        << ", issuer: " << (issuer.empty() ? "(none)" : issuer)
        << ", audience: " << (audience.empty() ? "(none)" : audience);

    return auth;
}

std::expected<domain::jwt_claims, jwt_error> jwt_authenticator::validate(
    const std::string& token) const {

    if (!configured_) {
        BOOST_LOG_SEV(lg(), error) << "JWT authenticator not configured";
        return std::unexpected(jwt_error::invalid_token);
    }

    BOOST_LOG_SEV(lg(), trace) << "Validating JWT token";

    try {
        auto decoded = jwt::decode<json_traits>(token);

        // Build verifier
        auto verifier = jwt::verify<json_traits>();

        if (use_rsa_) {
            verifier = verifier.allow_algorithm(jwt::algorithm::rs256(public_key_, "", "", ""));
        } else {
            verifier = verifier.allow_algorithm(jwt::algorithm::hs256{secret_});
        }

        // Add issuer verification if configured
        if (!issuer_.empty()) {
            verifier = verifier.with_issuer(issuer_);
        }

        // Add audience verification if configured
        if (!audience_.empty()) {
            verifier = verifier.with_audience(audience_);
        }

        // Verify token
        verifier.verify(decoded);

        BOOST_LOG_SEV(lg(), trace) << "JWT token verified successfully";

        // Extract claims
        domain::jwt_claims claims;

        if (decoded.has_subject()) {
            claims.subject = decoded.get_subject();
        }

        if (decoded.has_issuer()) {
            claims.issuer = decoded.get_issuer();
        }

        if (decoded.has_audience()) {
            auto aud_set = decoded.get_audience();
            if (!aud_set.empty()) {
                claims.audience = *aud_set.begin();
            }
        }

        if (decoded.has_expires_at()) {
            claims.expires_at = decoded.get_expires_at();
        }

        if (decoded.has_issued_at()) {
            claims.issued_at = decoded.get_issued_at();
        }

        // Extract custom claims
        if (decoded.has_payload_claim("roles")) {
            auto roles_claim = decoded.get_payload_claim("roles");
            auto roles_array = roles_claim.as_array();
            for (const auto& role : roles_array) {
                claims.roles.push_back(std::string(role.as_string()));
            }
        }

        if (decoded.has_payload_claim("username")) {
            claims.username = decoded.get_payload_claim("username").as_string();
        }

        if (decoded.has_payload_claim("email")) {
            claims.email = decoded.get_payload_claim("email").as_string();
        }

        if (decoded.has_payload_claim("session_id")) {
            claims.session_id = decoded.get_payload_claim("session_id").as_string();
        }

        if (decoded.has_payload_claim("session_start")) {
            auto start_ts = decoded.get_payload_claim("session_start").as_integer();
            claims.session_start_time = std::chrono::system_clock::from_time_t(start_ts);
        }

        BOOST_LOG_SEV(lg(), debug) << "JWT claims extracted, subject: " << claims.subject
            << ", roles: " << claims.roles.size();

        return claims;

    } catch (const jwt::error::token_verification_exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "JWT verification failed: " << e.what();

        // Determine specific error type
        std::string msg = e.what();
        if (msg.find("expired") != std::string::npos) {
            return std::unexpected(jwt_error::expired_token);
        } else if (msg.find("signature") != std::string::npos) {
            return std::unexpected(jwt_error::invalid_signature);
        } else if (msg.find("issuer") != std::string::npos) {
            return std::unexpected(jwt_error::invalid_issuer);
        } else if (msg.find("audience") != std::string::npos) {
            return std::unexpected(jwt_error::invalid_audience);
        }
        return std::unexpected(jwt_error::invalid_token);

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "JWT decode error: " << e.what();
        return std::unexpected(jwt_error::invalid_token);
    }
}

std::optional<std::string> jwt_authenticator::create_token(
    const domain::jwt_claims& claims) const {

    if (!configured_ || use_rsa_) {
        BOOST_LOG_SEV(lg(), error) << "Token creation only supported with HS256";
        return std::nullopt;
    }

    BOOST_LOG_SEV(lg(), trace) << "Creating JWT token for subject: " << claims.subject;

    try {
        auto token = jwt::create<json_traits>()
            .set_type("JWT")
            .set_subject(claims.subject)
            .set_issued_at(claims.issued_at)
            .set_expires_at(claims.expires_at);

        if (!claims.issuer.empty()) {
            token = token.set_issuer(claims.issuer);
        } else if (!issuer_.empty()) {
            token = token.set_issuer(issuer_);
        }

        if (!claims.audience.empty()) {
            token = token.set_audience(claims.audience);
        } else if (!audience_.empty()) {
            token = token.set_audience(audience_);
        }

        // Add custom claims
        if (!claims.roles.empty()) {
            boost::json::array roles_array;
            for (const auto& role : claims.roles) {
                roles_array.push_back(boost::json::value(role));
            }
            token = token.set_payload_claim("roles",
                jwt::basic_claim<json_traits>(roles_array));
        }

        if (claims.username) {
            token = token.set_payload_claim("username",
                jwt::basic_claim<json_traits>(std::string(*claims.username)));
        }

        if (claims.email) {
            token = token.set_payload_claim("email",
                jwt::basic_claim<json_traits>(std::string(*claims.email)));
        }

        if (claims.session_id) {
            token = token.set_payload_claim("session_id",
                jwt::basic_claim<json_traits>(std::string(*claims.session_id)));
        }

        if (claims.session_start_time) {
            auto start_ts = std::chrono::system_clock::to_time_t(*claims.session_start_time);
            token = token.set_payload_claim("session_start",
                jwt::basic_claim<json_traits>(boost::json::value(start_ts)));
        }

        auto signed_token = token.sign(jwt::algorithm::hs256{secret_});

        BOOST_LOG_SEV(lg(), debug) << "JWT token created successfully";
        return signed_token;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "JWT token creation failed: " << e.what();
        return std::nullopt;
    }
}

std::string to_string(jwt_error error) {
    switch (error) {
        case jwt_error::invalid_token: return "Invalid token";
        case jwt_error::expired_token: return "Token has expired";
        case jwt_error::invalid_signature: return "Invalid signature";
        case jwt_error::missing_claims: return "Missing required claims";
        case jwt_error::invalid_issuer: return "Invalid issuer";
        case jwt_error::invalid_audience: return "Invalid audience";
        default: return "Unknown error";
    }
}

}
