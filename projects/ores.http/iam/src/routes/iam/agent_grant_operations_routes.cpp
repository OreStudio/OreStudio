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
#include "ores.http/routes/iam/agent_grant_operations_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/agent_grant_operations_protocol.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <exception>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

namespace ores::http::routes::iam {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;

namespace {

/**
 * @brief The request a body states, or nothing when it states none.
 *
 * The request's own codec reads it, so this unit needs no shape of the types the
 * request holds.
 */
template <typename T>
std::optional<T> agent_grant_operations_read_body(std::string_view body) {
    try {
        return ores::utility::codec::wire_traits<T>::read_json(body);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_create_agent_grant(const http_request& req,
                                                         nats_client& session) {
    using request_type = messaging::create_agent_grant_request;
    try {
        const auto parsed = agent_grant_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_create_agent_key(const http_request& req,
                                                       nats_client& session) {
    using request_type = messaging::create_agent_key_request;
    try {
        const auto parsed = agent_grant_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_revoke_agent_grant(const http_request& req,
                                                         nats_client& session) {
    using request_type = messaging::revoke_agent_grant_request;
    try {
        const auto parsed = agent_grant_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_revoke_agent_key(const http_request& req,
                                                       nats_client& session) {
    using request_type = messaging::revoke_agent_key_request;
    try {
        const auto parsed = agent_grant_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_list_own_agent_grants(const http_request& req,
                                                            nats_client& session) {
    using request_type = messaging::list_own_agent_grants_request;
    try {
        co_return ores::http::forwarding::respond(req, session, request_type{});
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_list_every_agent_grant(const http_request& req,
                                                             nats_client& session) {
    using request_type = messaging::list_every_agent_grant_request;
    try {
        co_return ores::http::forwarding::respond(req, session, request_type{});
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_list_own_agent_activity(const http_request& req,
                                                              nats_client& session) {
    using request_type = messaging::list_own_agent_activity_request;
    try {
        const auto parsed = agent_grant_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
agent_grant_operations_routes::handle_get_agent_held_write(const http_request& req,
                                                           nats_client& session) {
    using request_type = messaging::get_agent_held_write_request;
    try {
        const auto parsed = agent_grant_operations_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

void agent_grant_operations_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    ORES_LOG_SEV(lg(), info) << "Registering agent_grant_operations routes";

    auto create_agent_grant_route =
        router->post("/api/v1/iam/agent_grants/put")
            .summary("Create agent grant")
            .description("Forwards to the iam.v1.agent_grants.put operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "create_agent_grant")
            .auth_required()
            .body<messaging::create_agent_grant_request>()
            .response<messaging::create_agent_grant_response>()
            .handler([&session](const http_request& req) {
                return handle_create_agent_grant(req, session);
            });
    const auto create_agent_grant_built = create_agent_grant_route.build();
    router->add_route(create_agent_grant_built);
    registry->register_route(create_agent_grant_built);

    auto create_agent_key_route =
        router->post("/api/v1/iam/ops/create_agent_key")
            .summary("Create agent key")
            .description("Forwards to the iam.v1.ops.create_agent_key operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "create_agent_key")
            .auth_required()
            .body<messaging::create_agent_key_request>()
            .response<messaging::create_agent_key_response>()
            .handler([&session](const http_request& req) {
                return handle_create_agent_key(req, session);
            });
    const auto create_agent_key_built = create_agent_key_route.build();
    router->add_route(create_agent_key_built);
    registry->register_route(create_agent_key_built);

    auto revoke_agent_grant_route =
        router->post("/api/v1/iam/ops/revoke_agent_grant")
            .summary("Revoke agent grant")
            .description("Forwards to the iam.v1.ops.revoke_agent_grant operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "revoke_agent_grant")
            .auth_required()
            .body<messaging::revoke_agent_grant_request>()
            .response<messaging::revoke_agent_grant_response>()
            .handler([&session](const http_request& req) {
                return handle_revoke_agent_grant(req, session);
            });
    const auto revoke_agent_grant_built = revoke_agent_grant_route.build();
    router->add_route(revoke_agent_grant_built);
    registry->register_route(revoke_agent_grant_built);

    auto revoke_agent_key_route =
        router->post("/api/v1/iam/ops/revoke_agent_key")
            .summary("Revoke agent key")
            .description("Forwards to the iam.v1.ops.revoke_agent_key operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "revoke_agent_key")
            .auth_required()
            .body<messaging::revoke_agent_key_request>()
            .response<messaging::revoke_agent_key_response>()
            .handler([&session](const http_request& req) {
                return handle_revoke_agent_key(req, session);
            });
    const auto revoke_agent_key_built = revoke_agent_key_route.build();
    router->add_route(revoke_agent_key_built);
    registry->register_route(revoke_agent_key_built);

    auto list_own_agent_grants_route =
        router->post("/api/v1/iam/ops/list_own_agent_grants")
            .summary("List own agent grants")
            .description("Forwards to the iam.v1.ops.list_own_agent_grants operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "list_own_agent_grants")
            .auth_required()
            .body<messaging::list_own_agent_grants_request>()
            .response<messaging::list_own_agent_grants_response>()
            .handler([&session](const http_request& req) {
                return handle_list_own_agent_grants(req, session);
            });
    const auto list_own_agent_grants_built = list_own_agent_grants_route.build();
    router->add_route(list_own_agent_grants_built);
    registry->register_route(list_own_agent_grants_built);

    auto list_every_agent_grant_route =
        router->post("/api/v1/iam/ops/list_every_agent_grant")
            .summary("List every agent grant")
            .description("Forwards to the iam.v1.ops.list_every_agent_grant operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "list_every_agent_grant")
            .auth_required()
            .body<messaging::list_every_agent_grant_request>()
            .response<messaging::list_every_agent_grant_response>()
            .handler([&session](const http_request& req) {
                return handle_list_every_agent_grant(req, session);
            });
    const auto list_every_agent_grant_built = list_every_agent_grant_route.build();
    router->add_route(list_every_agent_grant_built);
    registry->register_route(list_every_agent_grant_built);

    auto list_own_agent_activity_route =
        router->post("/api/v1/iam/ops/list_own_agent_activity")
            .summary("List own agent activity")
            .description("Forwards to the iam.v1.ops.list_own_agent_activity operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "list_own_agent_activity")
            .auth_required()
            .body<messaging::list_own_agent_activity_request>()
            .response<messaging::list_own_agent_activity_response>()
            .handler([&session](const http_request& req) {
                return handle_list_own_agent_activity(req, session);
            });
    const auto list_own_agent_activity_built = list_own_agent_activity_route.build();
    router->add_route(list_own_agent_activity_built);
    registry->register_route(list_own_agent_activity_built);

    auto get_agent_held_write_route =
        router->post("/api/v1/iam/ops/get_agent_held_write")
            .summary("Get agent held write")
            .description("Forwards to the iam.v1.ops.get_agent_held_write operation.")
            .tags({"iam"})
            .operation_catalogue("iam", "agent_grant_operations", "get_agent_held_write")
            .auth_required()
            .body<messaging::get_agent_held_write_request>()
            .response<messaging::get_agent_held_write_response>()
            .handler([&session](const http_request& req) {
                return handle_get_agent_held_write(req, session);
            });
    const auto get_agent_held_write_built = get_agent_held_write_route.build();
    router->add_route(get_agent_held_write_built);
    registry->register_route(get_agent_held_write_built);

    ORES_LOG_SEV(lg(), info) << "agent_grant_operations routes registered: " << 8 << " endpoint(s)";
}

}
