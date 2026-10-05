/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_TESTING_HANDLER_TEST_SUPPORT_HPP
#define ORES_TESTING_HANDLER_TEST_SUPPORT_HPP

#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

/**
 * @file handler_test_support.hpp
 * @brief Drives a service handler over real NATS with a token the test mints.
 *
 * A test subscribes the handler on its own subjects, under a verifier with a
 * test secret, so the running service, which subscribes the real subjects with
 * its own secret, never answers the test's requests.
 */
namespace ores::testing {

struct handler_test_keys {
    std::string secret = "handler-test-secret";
    std::string issuer = "ores.iam.test";
    std::string audience = "ores.test";

    ores::security::jwt::jwt_authenticator verifier() const {
        return ores::security::jwt::jwt_authenticator::create_hs256(secret, issuer, audience);
    }

    /**
     * @brief A token for a tenant, acting for @p party and seeing only it,
     * whose roles are the permission codes it holds.
     */
    std::string token(const std::string& tenant_id,
                      const boost::uuids::uuid& party,
                      const std::vector<std::string>& permissions) const {
        auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
        claims.subject = "handler-test";
        claims.issuer = issuer;
        claims.audience = audience;
        claims.tenant_id = tenant_id;
        claims.party_id = boost::uuids::to_string(party);
        claims.visible_party_ids = {boost::uuids::to_string(party)};
        claims.roles = permissions;
        return *verifier().create_token(claims);
    }

    /**
     * @brief A token for a tenant that acts for no party, as a workflow step's
     * service session does.
     */
    std::string token_without_party(const std::string& tenant_id,
                                    const std::vector<std::string>& permissions) const {
        auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
        claims.subject = "handler-test";
        claims.issuer = issuer;
        claims.audience = audience;
        claims.tenant_id = tenant_id;
        claims.roles = permissions;
        return *verifier().create_token(claims);
    }
};

/**
 * @brief Sends a request with a bearer token and returns the raw reply.
 */
template <typename Req>
ores::nats::message request(ores::nats::service::client& nats,
                            const std::string& subject,
                            const std::string& token,
                            const Req& req) {
    return nats.request_sync(subject,
                             ores::nats::default_wire_codec().encode(req),
                             {{std::string(ores::nats::headers::authorization),
                               std::string(ores::nats::headers::bearer_prefix) + token}});
}

/**
 * @brief The reply decoded, or nothing when the handler answered with an error
 * header instead.
 */
template <typename Resp>
std::optional<Resp> decode_reply(const ores::nats::message& reply) {
    if (reply.headers.contains(std::string(ores::nats::headers::x_error)))
        return std::nullopt;
    auto decoded = ores::nats::default_wire_codec().decode<Resp>(reply.data);
    if (!decoded)
        return std::nullopt;
    return *decoded;
}

/**
 * @brief The error header a handler replied with, or empty.
 */
inline std::string error_of(const ores::nats::message& reply) {
    const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
    return it == reply.headers.end() ? std::string() : it->second;
}

}

#endif
