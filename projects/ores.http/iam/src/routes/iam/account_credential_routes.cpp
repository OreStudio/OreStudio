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
 * Template: cpp_http_route_impl.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.http/routes/iam/account_credential_routes.hpp"
#include "ores.iam.api/messaging/account_credential_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <exception>
#include <memory>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>

namespace ores::http::routes::iam {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;

namespace {

/**
 * @brief Read one path or query parameter as the member's own type.
 *
 * A string member takes the parameter verbatim; every other member goes
 * through the lexical conversion its own type defines. The parameter's name
 * travels with the value so a malformed one names the member it belongs to
 * rather than the exception's own text.
 */
template <typename T>
T read_param(const std::string& raw, const std::string& name) {
    try {
        if constexpr (std::is_same_v<T, std::string>) {
            return raw;
        } else if constexpr (std::is_same_v<T, bool>) {
            return raw == "true" || raw == "1";
        } else {
            return boost::lexical_cast<T>(raw);
        }
    } catch (const std::exception&) {
        throw std::runtime_error("Invalid value for " + name + ": " + raw);
    }
}

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

/**
 * @brief Apply the page and the order a caller stated.
 *
 * A parameter the caller left out leaves the request's own default standing,
 * so a route that documents a page cannot silently change a request that
 * stated none.
 */
template <typename Request>
void apply_page(Request& msg, const http_request& req) {
    if (const auto raw = req.get_query_param("offset"); !raw.empty()) {
        msg.offset = read_param<std::uint32_t>(raw, "offset");
    }
    if (const auto raw = req.get_query_param("limit"); !raw.empty()) {
        msg.limit = read_param<std::uint32_t>(raw, "limit");
    }
    if (const auto raw = req.get_query_param("order"); !raw.empty()) {
        msg.order.field = raw;
    }
    msg.order.descending = req.get_query_param("desc") == "true";
}

}

boost::asio::awaitable<http_response>
account_credential_routes::handle_list(const http_request& req, nats_client& session) {
    using request_type = messaging::list_account_credentials_request;
    try {
        request_type msg;
        apply_page(msg, req);
        co_return co_await forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> account_credential_routes::handle_get(const http_request& req,
                                                                            nats_client& session) {
    using request_type = messaging::get_account_credential_request;
    try {
        request_type msg;
        msg.key.id = read_param<boost::uuids::uuid>(req.get_path_param("id"), "id");
        co_return co_await forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_credential_routes::handle_get_many(const http_request& req, nats_client& session) {
    using request_type = messaging::get_many_account_credentials_request;
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
account_credential_routes::handle_by_account_id(const http_request& req, nats_client& session) {
    using request_type = messaging::list_by_account_id_account_credentials_request;
    try {
        request_type msg;
        msg.account_id =
            read_param<boost::uuids::uuid>(req.get_path_param("account_id"), "account_id");
        if (const auto raw = req.get_query_param("scope"); !raw.empty()) {
            msg.scope = raw == "subtree" ? ores::utility::domain::scope::subtree :
                                           ores::utility::domain::scope::direct;
        }
        apply_page(msg, req);
        co_return co_await forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_credential_routes::handle_versions(const http_request& req, nats_client& session) {
    using request_type = messaging::list_account_credential_versions_request;
    try {
        request_type msg;
        msg.key.id = read_param<boost::uuids::uuid>(req.get_path_param("id"), "id");
        apply_page(msg, req);
        co_return co_await forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
account_credential_routes::handle_version(const http_request& req, nats_client& session) {
    using request_type = messaging::get_account_credential_version_request;
    try {
        request_type msg;
        msg.key.account_credential.id =
            read_param<boost::uuids::uuid>(req.get_path_param("id"), "id");
        msg.key.version = read_param<std::uint32_t>(req.get_path_param("version"), "version");
        co_return co_await forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

void account_credential_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering account_credentials routes";

    auto list_route =
        router->get("/api/v1/iam/account_credentials")
            .summary("List account credentials")
            .description("Forwards to the iam.v1.account_credentials.list operation.")
            .tags({"iam"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("order", "string", "", false, "Column to order")
            .query_param("desc", "boolean", "", false, "Descending")
            .response<messaging::list_account_credentials_response>()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto get_route =
        router->get("/api/v1/iam/account_credentials/{id}")
            .summary("Get one account credential")
            .description("Forwards to the iam.v1.account_credentials.get operation.")
            .tags({"iam"})
            .auth_required()
            .response<messaging::get_account_credential_response>()
            .handler([&session](const http_request& req) { return handle_get(req, session); });
    const auto get_built = get_route.build();
    router->add_route(get_built);
    registry->register_route(get_built);

    auto get_many_route =
        router->post("/api/v1/iam/account_credentials/get-many")
            .summary("Get many account credentials")
            .description("Forwards to the iam.v1.account_credentials.get_many operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::get_many_account_credentials_request>()
            .response<messaging::get_many_account_credentials_response>()
            .handler([&session](const http_request& req) { return handle_get_many(req, session); });
    const auto get_many_built = get_many_route.build();
    router->add_route(get_many_built);
    registry->register_route(get_many_built);

    auto by_account_id_route =
        router->get("/api/v1/iam/account_credentials/by-account-id/{account_id}")
            .summary("List the account credentials that reference one account id")
            .description("Forwards to the iam.v1.account_credentials.list_by_account_id operation.")
            .tags({"iam"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("order", "string", "", false, "Column to order")
            .query_param("desc", "boolean", "", false, "Descending")
            .query_param("scope", "string", "", false, "direct or subtree")
            .response<messaging::list_by_account_id_account_credentials_response>()
            .handler(
                [&session](const http_request& req) { return handle_by_account_id(req, session); });
    const auto by_account_id_built = by_account_id_route.build();
    router->add_route(by_account_id_built);
    registry->register_route(by_account_id_built);

    auto versions_route =
        router->get("/api/v1/iam/account_credentials/{id}/versions")
            .summary("List the recorded versions of one account credential")
            .description("Forwards to the iam.v1.account_credentials_versions.list operation.")
            .tags({"iam"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("order", "string", "", false, "Column to order")
            .query_param("desc", "boolean", "", false, "Descending")
            .response<messaging::list_account_credential_versions_response>()
            .handler([&session](const http_request& req) { return handle_versions(req, session); });
    const auto versions_built = versions_route.build();
    router->add_route(versions_built);
    registry->register_route(versions_built);

    auto version_route =
        router->get("/api/v1/iam/account_credentials/{id}/versions/{version}")
            .summary("Get one recorded version of one account credential")
            .description("Forwards to the iam.v1.account_credentials_versions.get operation.")
            .tags({"iam"})
            .auth_required()
            .response<messaging::get_account_credential_version_response>()
            .handler([&session](const http_request& req) { return handle_version(req, session); });
    const auto version_built = version_route.build();
    router->add_route(version_built);
    registry->register_route(version_built);

    BOOST_LOG_SEV(lg(), info) << "account_credentials routes registered: " << 6 << " endpoint(s)";
}

}
