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
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.logging/logger.hpp"
#include "ores.security/authorization/grants.hpp"
#include <array>
#include <string>
#include <utility>
#include <vector>

namespace ores::http::forwarding {

std::optional<std::string> caller_session_id(const ores::http::domain::http_request& req) {
    const auto& user = req.authenticated_user;
    if (!user || !user->session_id || user->session_id->empty())
        return std::nullopt;
    return user->session_id;
}

ores::nats::service::nats_client caller_client(const ores::http::domain::http_request& req,
                                               ores::nats::service::nats_client& session) {
    auto delegated = session.with_delegation(req.get_bearer_token().value_or(std::string{}));
    if (const auto id = caller_session_id(req))
        return delegated.with_session_id(*id);
    return delegated;
}

using namespace ores::logging;

/// The one code that says a session acts on the deployment rather than on a tenant.
constexpr std::string_view deployment_read = "installation::deployment:read";
using ores::http::domain::http_response;
using ores::http::domain::http_status;
namespace rr = ores::http::net::record_routes;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.http.forwarding");
    return instance;
}

/// A service refusal code, and the HTTP status and code the browser reads for it.
struct refusal_mapping final {
    std::string_view service_code;
    http_status status;
    std::string_view code;
};

constexpr std::array refusal_mappings{
    refusal_mapping{"forbidden", http_status::forbidden, "forbidden"},
    refusal_mapping{"bad_request", http_status::bad_request, "invalid-request"},
    refusal_mapping{"unauthorized", http_status::unauthorized, "not-authenticated"},
};

}

rr::refusal refusal_for(std::string_view code) {
    for (const auto& m : refusal_mappings) {
        if (m.service_code == code)
            return rr::refusal{m.status, std::string(m.code), std::string(code)};
    }
    ORES_LOG_SEV(lg(), warn) << "Unknown service refusal: " << code;
    return rr::refusal{
        http_status::bad_gateway, "upstream-unavailable", std::string(rr::unanswered)};
}

void refuse_refused(const ores::nats::service::request_refused_error& e) {
    throw refusal_for(e.code());
}

void refuse_undecodable(std::string_view subject, const std::string& detail) {
    ORES_LOG_SEV(lg(), error) << "Failed to decode the reply on " << subject << ": " << detail;
    rr::refuse(http_status::bad_gateway, "internal", std::string(rr::unanswered));
}

void refuse_lapsed(const ores::nats::service::session_expired_error& e) {
    rr::refuse(http_status::unauthorized, "session-expired", e.what());
}

void require_deployment_read(const ores::http::domain::http_request& req,
                             ores::nats::service::nats_client& session,
                             std::string_view what) {
    const auto reply = forward(req, session, ores::iam::messaging::get_my_roles_request{});
    rr::require_done(reply.result);

    std::vector<std::string> granted;
    for (const auto& role : reply.roles)
        granted.insert(granted.end(), role.permission_codes.begin(), role.permission_codes.end());
    if (ores::security::authorization::grants(granted, deployment_read))
        return;
    rr::refuse(http_status::forbidden,
               "forbidden",
               std::string(what) + " needs the read of the deployment as a whole.");
}

http_response unanswered(std::string_view subject, const std::exception& e) {
    ORES_LOG_SEV(lg(), error) << "No answer on " << subject << ": " << e.what();
    return http_response::api_error(
        http_status::bad_gateway, "internal", std::string(rr::unanswered));
}

}
