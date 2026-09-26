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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_http_route_operation_implementation.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http/routes/iam/authorization_routes.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <exception>
#include <memory>
#include <rfl/json.hpp>
#include <string>
#include <utility>

namespace ores::http::routes::iam {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;

namespace {

/**
 * @brief The HTTP response a service error names.
 *
 * The service answers a refused caller with a status in its reply header
 * rather than a body, so the gateway reads that status and maps it. An error
 * it does not know is the caller's to see as unauthorised, which is what the
 * service means by any code it does not spell out here.
 */
http_response error_response(const std::string& error) {
    if (error == "forbidden") {
        return http_response::forbidden(error);
    }
    if (error == "bad_request") {
        return http_response::bad_request(error);
    }
    return http_response::unauthorized(error);
}

/**
 * @brief Forward one canonical request and map the reply to a response.
 *
 * The caller's own bearer token is taken from the HTTP request and delegated
 * on the NATS call, so the service validates that caller and its own
 * permission check governs. The gateway authorises nobody and decides
 * nothing; it states the subject the request already carries.
 */
template <typename Request>
boost::asio::awaitable<http_response>
forward(const http_request& req, nats_client& session, const Request& msg) {
    auto delegated = session.with_delegation(req.get_bearer_token().value_or(std::string{}));
    try {
        const auto reply =
            delegated.authenticated_request(msg.nats_subject,
                                            ores::nats::default_wire_codec().encode(msg),
                                            ores::nats::service::default_request_timeout);
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end()) {
            co_return error_response(it->second);
        }
        const auto decoded =
            ores::nats::default_wire_codec().decode<typename Request::response_type>(reply.data);
        if (!decoded) {
            co_return http_response::internal_error(std::string("Failed to decode response: ") +
                                                    decoded.error().what());
        }
        co_return http_response::json(rfl::json::write(*decoded));
    } catch (const ores::nats::service::session_expired_error& e) {
        co_return http_response::unauthorized(e.what());
    } catch (const std::exception& e) {
        co_return http_response::internal_error(e.what());
    }
}

} // namespace

boost::asio::awaitable<http_response>
authorization_routes::handle_assign_role(const http_request& req, nats_client& session) {
    using request_type = messaging::assign_role_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
authorization_routes::handle_revoke_role(const http_request& req, nats_client& session) {
    using request_type = messaging::revoke_role_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
authorization_routes::handle_get_account_roles(const http_request& req, nats_client& session) {
    using request_type = messaging::get_account_roles_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
authorization_routes::handle_get_account_permissions(const http_request& req,
                                                     nats_client& session) {
    using request_type = messaging::get_account_permissions_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
authorization_routes::handle_get_role_permissions(const http_request& req, nats_client& session) {
    using request_type = messaging::get_role_permissions_request;
    try {
        const auto parsed = rfl::json::read<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

void authorization_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering authorization routes";

    auto assign_role_route = router->post("/api/v1/iam/roles/assign")
                                 .summary("Assign role")
                                 .description("Forwards to the iam.v1.roles.assign operation.")
                                 .tags({"iam"})
                                 .auth_required()
                                 .body<messaging::assign_role_request>()
                                 .response<messaging::assign_role_response>()
                                 .handler([&session](const http_request& req) {
                                     return handle_assign_role(req, session);
                                 });
    router->add_route(assign_role_route.build());
    registry->register_route(assign_role_route.build());

    auto revoke_role_route = router->post("/api/v1/iam/roles/revoke")
                                 .summary("Revoke role")
                                 .description("Forwards to the iam.v1.roles.revoke operation.")
                                 .tags({"iam"})
                                 .auth_required()
                                 .body<messaging::revoke_role_request>()
                                 .response<messaging::revoke_role_response>()
                                 .handler([&session](const http_request& req) {
                                     return handle_revoke_role(req, session);
                                 });
    router->add_route(revoke_role_route.build());
    registry->register_route(revoke_role_route.build());

    auto get_account_roles_route =
        router->post("/api/v1/iam/roles/by-account")
            .summary("Get account roles")
            .description("Forwards to the iam.v1.roles.by-account operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::get_account_roles_request>()
            .response<messaging::get_account_roles_response>()
            .handler([&session](const http_request& req) {
                return handle_get_account_roles(req, session);
            });
    router->add_route(get_account_roles_route.build());
    registry->register_route(get_account_roles_route.build());

    auto get_account_permissions_route =
        router->post("/api/v1/iam/roles/permissions-by-account")
            .summary("Get account permissions")
            .description("Forwards to the iam.v1.roles.permissions-by-account operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::get_account_permissions_request>()
            .response<messaging::get_account_permissions_response>()
            .handler([&session](const http_request& req) {
                return handle_get_account_permissions(req, session);
            });
    router->add_route(get_account_permissions_route.build());
    registry->register_route(get_account_permissions_route.build());

    auto get_role_permissions_route =
        router->post("/api/v1/iam/roles/permissions")
            .summary("Get role permissions")
            .description("Forwards to the iam.v1.roles.permissions operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::get_role_permissions_request>()
            .response<messaging::get_role_permissions_response>()
            .handler([&session](const http_request& req) {
                return handle_get_role_permissions(req, session);
            });
    router->add_route(get_role_permissions_route.build());
    registry->register_route(get_role_permissions_route.build());

    BOOST_LOG_SEV(lg(), info) << "authorization routes registered: " << 5 << " endpoint(s)";
}

}
