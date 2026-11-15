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
#include "ores.http/routes/iam/session_renewal.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/timeouts.hpp"
#include <exception>
#include <unordered_map>
#include <utility>

namespace ores::http::routes::iam {

using ores::http::domain::token_renewal;

token_renewal session_renewal::outcome_of(const std::string& x_error,
                                          bool success,
                                          const std::string& token,
                                          std::optional<ores::security::jwt::jwt_claims> claims) {
    using outcome = token_renewal::outcome;
    // IAM states a session past its maximum, or a token it will not renew, as a
    // refusal: either in the X-Error header or as an answer that is not a success.
    if (!x_error.empty() || !success)
        return token_renewal{outcome::refused, {}, std::nullopt};
    // A success that carries no token is a malformed answer, not a refusal.
    if (token.empty() || !claims)
        return token_renewal{outcome::unavailable, {}, std::nullopt};
    return token_renewal{outcome::renewed, token, std::move(claims)};
}

ores::http::domain::token_renewer
session_renewal::renewer(ores::nats::service::nats_client& session, verifier_finder find_verifier) {
    // The client is the application's, which outlives the server that calls this.
    return [&session, find_verifier = std::move(find_verifier)](const std::string& token) {
        using request_type = ores::iam::messaging::refresh_request;
        try {
            const auto wire = ores::nats::default_wire_codec().encode(request_type{});
            std::unordered_map<std::string, std::string> headers{
                {std::string(ores::nats::headers::authorization),
                 std::string(ores::nats::headers::bearer_prefix) + token}};
            const auto reply = session.transport().request_sync(
                request_type::nats_subject, wire, std::move(headers));
            const auto error = reply.headers.find(std::string(ores::nats::headers::x_error));
            if (error != reply.headers.end()) {
                using namespace ores::logging;
                BOOST_LOG_SEV(lg(), info) << "IAM refused to renew a session: " << error->second;
                return outcome_of(error->second, false, {}, std::nullopt);
            }
            const auto decoded =
                ores::nats::default_wire_codec().decode<request_type::response_type>(reply.data);
            if (!decoded)
                return token_renewal{};
            std::optional<ores::security::jwt::jwt_claims> claims;
            if (const auto verifier = find_verifier ? find_verifier() : nullptr;
                verifier && verifier->is_configured() && !decoded->token.empty()) {
                if (auto checked = verifier->validate(decoded->token))
                    claims = std::move(checked.value());
            }
            return outcome_of({}, decoded->success, decoded->token, std::move(claims));
        } catch (const std::exception&) {
            return token_renewal{};
        }
    };
}

}
