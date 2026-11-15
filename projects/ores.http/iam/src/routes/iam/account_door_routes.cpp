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
#include "ores.http/routes/iam/account_door_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/rate_limiter.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_screen_shapes.hpp"
#include "ores.http/routes/iam/bootstrap_screen_routes.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <map>
#include <utility>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
namespace rr = ores::http::net::record_routes;

rr::refusal account_door_routes::signup_refusal(const std::string& code,
                                                const std::string& message) {
    static const std::map<std::string, std::pair<http_status, std::string>> known{
        {"username_taken", {http_status::conflict, "username-taken"}},
        {"email_taken", {http_status::conflict, "email-taken"}},
        {"weak_password", {http_status::bad_request, "weak-password"}},
        {"invalid_request", {http_status::bad_request, "invalid-request"}},
        {"signup_disabled", {http_status::forbidden, "signups-disabled"}},
        {"signup_requires_authorization",
         {http_status::forbidden, "signup-requires-authorization"}},
        {"no_registration_destination", {http_status::forbidden, "no-registration-destination"}},
        {"no_default_role", {http_status::forbidden, "no-default-role"}}};
    const auto found = known.find(code);
    using namespace ores::logging;
    if (found == known.end())
        BOOST_LOG_SEV(lg(), warn) << "A registration was refused with a code this build does not "
                                  << "map, answered as signup-refused: " << code;
    const auto& [status, api_code] =
        found != known.end() ? found->second :
                               std::pair{http_status::forbidden, std::string("signup-refused")};
    return rr::refusal{
        status, api_code, message.empty() ? "The registration was refused." : message};
}

std::string account_door_routes::signup_json(const messaging::signup_response& r) {
    return "{\"success\":" + json::write(r.success) + ",\"message\":" + json::write(r.message) +
           ",\"errorCode\":" + json::write(r.error_code) +
           ",\"accountId\":" + json::write(r.account_id) +
           ",\"accountStatus\":" + json::write(r.account_status) +
           ",\"partyId\":" + json::write(r.party_id) + ",\"roleId\":" + json::write(r.role_id) +
           "}";
}

std::string account_door_routes::policy_json(const messaging::get_password_policy_response& r) {
    return "{\"success\":" + json::write(r.success) + ",\"message\":" + json::write(r.message) +
           ",\"minLength\":" + std::to_string(r.min_length) +
           ",\"requireUppercase\":" + json::write(r.require_uppercase) +
           ",\"requireLowercase\":" + json::write(r.require_lowercase) +
           ",\"requireDigit\":" + json::write(r.require_digit) +
           ",\"requireSpecial\":" + json::write(r.require_special) +
           ",\"specialChars\":" + json::write(r.special_chars) + "}";
}

void account_door_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using ores::http::forwarding::guarded;

    const auto signup =
        router->post("/api/signup")
            .summary("Register an account")
            .tags({"auth"})
            .auth_optional()
            .rate_limit(ores::http::net::rate_policies::credential)
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                const auto body = json::read_expected<signup_body>(req.body);
                if (!body || body->principal.empty() || body->password.empty() ||
                    body->email.empty())
                    co_return rr::refused(
                        http_status::bad_request,
                        "invalid-request",
                        "A username, an email address and a password are required.");
                co_return guarded<messaging::signup_request>([&] {
                    const auto reply = ores::http::forwarding::forward(
                        req,
                        session,
                        messaging::signup_request{
                            body->principal,
                            body->password,
                            body->email,
                            bootstrap_screen_routes::hostname_of(req.get_header("host"))});
                    if (!reply.success)
                        return rr::answer(signup_refusal(reply.error_code, reply.message));
                    return http_response::json(signup_json(reply));
                });
            })
            .build();
    router->add_route(signup);
    registry->register_route(signup);

    const auto policy =
        router->get("/api/password-policy")
            .summary("Read the rules a password must keep")
            .tags({"auth"})
            .auth_optional()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::get_password_policy_request>([&] {
                    return http_response::json(policy_json(ores::http::forwarding::forward(
                        req, session, messaging::get_password_policy_request{})));
                });
            })
            .build();
    router->add_route(policy);
    registry->register_route(policy);
}

}
