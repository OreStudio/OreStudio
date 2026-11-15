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
#include "ores.http/routes/iam/tenant_status_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/tenant_status_protocol.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <exception>
#include <initializer_list>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

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
std::optional<T> tenant_status_read_body(std::string_view body) {
    try {
        return ores::utility::codec::wire_traits<T>::read_json(body);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

/**
 * @brief Read one path or query parameter as the member's own type.
 *
 * A string member takes the parameter verbatim; every other member goes
 * through the lexical conversion its own type defines. The parameter's name
 * travels with the value so a malformed one names the member it belongs to
 * rather than the exception's own text.
 */
template <typename T>
T tenant_status_read_param(const std::string& raw, const std::string& name) {
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
 * @brief Apply the page and the order a caller stated.
 *
 * A parameter the caller left out leaves the request's own default standing,
 * so a route that documents a page cannot silently change a request that
 * stated none.
 */
template <typename Request>
void tenant_status_apply_page(Request& msg, const http_request& req) {
    if (const auto raw = req.get_query_param("offset"); !raw.empty()) {
        msg.offset = tenant_status_read_param<std::uint32_t>(raw, "offset");
    }
    if (const auto raw = req.get_query_param("limit"); !raw.empty()) {
        msg.limit = tenant_status_read_param<std::uint32_t>(raw, "limit");
    }
    if (const auto raw = req.get_query_param("sort"); !raw.empty()) {
        msg.order.field = raw;
    }
    msg.order.descending = req.get_query_param("descending") == "true";
}

/**
 * @brief Read a comma-separated parameter as a list of the member's own type.
 *
 * An empty item is refused, because it would otherwise match the empty value.
 */
template <typename T>
std::vector<T> tenant_status_read_list(const std::string& raw, const std::string& name) {
    std::vector<T> values;
    std::string::size_type begin = 0;
    while (true) {
        const auto end = raw.find(',', begin);
        const auto item = raw.substr(begin, end == std::string::npos ? end : end - begin);
        if (item.empty()) {
            throw std::runtime_error("Invalid value for " + name + ": " + raw);
        }
        values.push_back(tenant_status_read_param<T>(item, name));
        if (end == std::string::npos) {
            return values;
        }
        begin = end + 1;
    }
}

/**
 * @brief Refuse a filter this route does not declare.
 *
 * A filter that narrowed nothing would read as an answer, so the route names it
 * rather than ignoring it.
 */
[[maybe_unused]] void
tenant_status_refuse_undeclared_filters(const http_request& req,
                                        std::initializer_list<std::string_view> declared) {
    for (const auto& [name, value] : req.query_params) {
        if (name.starts_with("f.") &&
            std::find(declared.begin(), declared.end(), name) == declared.end()) {
            throw std::runtime_error("This list cannot be filtered by " + name.substr(2) + ".");
        }
    }
}

}

boost::asio::awaitable<http_response> tenant_status_routes::handle_list(const http_request& req,
                                                                        nats_client& session) {
    using request_type = messaging::list_tenant_statuses_request;
    try {
        request_type msg;
        tenant_status_refuse_undeclared_filters(req,
                                                {
                                                    "f.status_one_of",
                                                });
        tenant_status_apply_page(msg, req);
        if (const auto raw = req.get_query_param("f.status_one_of"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::tenant_statuses_filter{};
            msg.filter->status_one_of =
                tenant_status_read_list<std::string>(raw, "f.status_one_of");
        }
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_get(const http_request& req,
                                                                       nats_client& session) {
    using request_type = messaging::get_tenant_status_request;
    try {
        request_type msg;
        msg.key.status =
            tenant_status_read_param<std::string>(req.get_path_param("status"), "status");
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_get_many(const http_request& req,
                                                                            nats_client& session) {
    using request_type = messaging::get_many_tenant_statuses_request;
    try {
        const auto parsed = tenant_status_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_add(const http_request& req,
                                                                       nats_client& session) {
    using request_type = messaging::put_tenant_status_request;
    try {
        auto parsed = tenant_status_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        // The route states whether the row may exist; the body states the rest.
        parsed->change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_set(const http_request& req,
                                                                       nats_client& session) {
    using request_type = messaging::put_tenant_status_request;
    try {
        auto parsed = tenant_status_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        // The route states whether the row may exist; the body states the rest.
        parsed->change.precondition.kind = ores::utility::domain::precondition_kind::any;
        if (const auto raw = req.get_query_param("version"); !raw.empty()) {
            parsed->change.precondition.kind =
                ores::utility::domain::precondition_kind::must_match_version;
            parsed->change.precondition.version =
                tenant_status_read_param<std::uint32_t>(raw, "version");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_put_many(const http_request& req,
                                                                            nats_client& session) {
    using request_type = messaging::put_many_tenant_statuses_request;
    try {
        const auto parsed = tenant_status_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_delete(const http_request& req,
                                                                          nats_client& session) {
    using request_type = messaging::delete_tenant_status_request;
    try {
        request_type msg;
        msg.removal.key.status =
            tenant_status_read_param<std::string>(req.get_path_param("status"), "status");
        // A delete carries no body, so its intent arrives as query parameters.
        msg.intent.reason_code = req.get_query_param("reason");
        msg.intent.commentary = req.get_query_param("commentary");
        if (msg.intent.reason_code.empty()) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "reason is required");
        }
        if (msg.intent.commentary.empty()) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "commentary is required");
        }
        if (const auto raw = req.get_query_param("version"); !raw.empty()) {
            msg.removal.precondition.kind =
                ores::utility::domain::precondition_kind::must_match_version;
            msg.removal.precondition.version =
                tenant_status_read_param<std::uint32_t>(raw, "version");
        }
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response>
tenant_status_routes::handle_delete_many(const http_request& req, nats_client& session) {
    using request_type = messaging::delete_many_tenant_statuses_request;
    try {
        const auto parsed = tenant_status_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_versions(const http_request& req,
                                                                            nats_client& session) {
    using request_type = messaging::list_tenant_status_versions_request;
    try {
        request_type msg;
        msg.key.status =
            tenant_status_read_param<std::string>(req.get_path_param("status"), "status");
        tenant_status_apply_page(msg, req);
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> tenant_status_routes::handle_version(const http_request& req,
                                                                           nats_client& session) {
    using request_type = messaging::get_tenant_status_version_request;
    try {
        request_type msg;
        msg.key.tenant_status.status =
            tenant_status_read_param<std::string>(req.get_path_param("status"), "status");
        msg.key.version =
            tenant_status_read_param<std::uint32_t>(req.get_path_param("version"), "version");
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

void tenant_status_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering tenant_statuses routes";

    auto list_route =
        router->get("/api/v1/tenancy/tenant_statuses")
            .summary("List tenant statuses")
            .description("Forwards to the iam.v1.tenant_statuses.list operation.")
            .tags({"iam"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("sort", "string", "", false, "Column to order")
            .query_param("descending", "boolean", "", false, "Descending")
            .query_param("f.status_one_of",
                         "string",
                         "",
                         false,
                         "Rows whose status is one of these comma-separated values")
            .response<messaging::list_tenant_statuses_response>()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto get_route =
        router->get("/api/v1/tenancy/tenant_statuses/{status}")
            .summary("Get one tenant status")
            .description("Forwards to the iam.v1.tenant_statuses.get operation.")
            .tags({"iam"})
            .auth_required()
            .response<messaging::get_tenant_status_response>()
            .handler([&session](const http_request& req) { return handle_get(req, session); });
    const auto get_built = get_route.build();
    router->add_route(get_built);
    registry->register_route(get_built);

    auto get_many_route =
        router->post("/api/v1/tenancy/tenant_statuses/get-many")
            .summary("Get many tenant statuses")
            .description("Forwards to the iam.v1.tenant_statuses.get_many operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::get_many_tenant_statuses_request>()
            .response<messaging::get_many_tenant_statuses_response>()
            .handler([&session](const http_request& req) { return handle_get_many(req, session); });
    const auto get_many_built = get_many_route.build();
    router->add_route(get_many_built);
    registry->register_route(get_many_built);

    auto add_route =
        router->post("/api/v1/tenancy/tenant_statuses")
            .summary("Create one tenant status")
            .description("Forwards to the iam.v1.tenant_statuses.put operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::put_tenant_status_request>()
            .response<messaging::put_tenant_status_response>()
            .handler([&session](const http_request& req) { return handle_add(req, session); });
    const auto add_built = add_route.build();
    router->add_route(add_built);
    registry->register_route(add_built);

    auto set_route =
        router->put("/api/v1/tenancy/tenant_statuses")
            .summary("Replace one tenant status")
            .description("Forwards to the iam.v1.tenant_statuses.put operation.")
            .tags({"iam"})
            .auth_required()
            .query_param("version", "integer", "", false, "Version")
            .body<messaging::put_tenant_status_request>()
            .response<messaging::put_tenant_status_response>()
            .handler([&session](const http_request& req) { return handle_set(req, session); });
    const auto set_built = set_route.build();
    router->add_route(set_built);
    registry->register_route(set_built);

    auto put_many_route =
        router->post("/api/v1/tenancy/tenant_statuses/put-many")
            .summary("Write many tenant statuses")
            .description("Forwards to the iam.v1.tenant_statuses.put_many operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::put_many_tenant_statuses_request>()
            .response<messaging::put_many_tenant_statuses_response>()
            .handler([&session](const http_request& req) { return handle_put_many(req, session); });
    const auto put_many_built = put_many_route.build();
    router->add_route(put_many_built);
    registry->register_route(put_many_built);

    auto delete_route =
        router->delete_("/api/v1/tenancy/tenant_statuses/{status}")
            .summary("Delete one tenant status")
            .description("Forwards to the iam.v1.tenant_statuses.delete operation.")
            .tags({"iam"})
            .auth_required()
            .query_param("reason", "string", "", true, "Change reason code")
            .query_param("commentary", "string", "", true, "Commentary")
            .query_param("version", "integer", "", false, "Version")
            .response<messaging::delete_tenant_status_response>()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto delete_built = delete_route.build();
    router->add_route(delete_built);
    registry->register_route(delete_built);

    auto delete_many_route =
        router->post("/api/v1/tenancy/tenant_statuses/delete-many")
            .summary("Delete many tenant statuses")
            .description("Forwards to the iam.v1.tenant_statuses.delete_many operation.")
            .tags({"iam"})
            .auth_required()
            .body<messaging::delete_many_tenant_statuses_request>()
            .response<messaging::delete_many_tenant_statuses_response>()
            .handler(
                [&session](const http_request& req) { return handle_delete_many(req, session); });
    const auto delete_many_built = delete_many_route.build();
    router->add_route(delete_many_built);
    registry->register_route(delete_many_built);

    auto versions_route =
        router->get("/api/v1/tenancy/tenant_statuses/{status}/versions")
            .summary("List the recorded versions of one tenant status")
            .description("Forwards to the iam.v1.tenant_statuses_versions.list operation.")
            .tags({"iam"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("sort", "string", "", false, "Column to order")
            .query_param("descending", "boolean", "", false, "Descending")
            .response<messaging::list_tenant_status_versions_response>()
            .handler([&session](const http_request& req) { return handle_versions(req, session); });
    const auto versions_built = versions_route.build();
    router->add_route(versions_built);
    registry->register_route(versions_built);

    auto version_route =
        router->get("/api/v1/tenancy/tenant_statuses/{status}/versions/{version}")
            .summary("Get one recorded version of one tenant status")
            .description("Forwards to the iam.v1.tenant_statuses_versions.get operation.")
            .tags({"iam"})
            .auth_required()
            .response<messaging::get_tenant_status_version_response>()
            .handler([&session](const http_request& req) { return handle_version(req, session); });
    const auto version_built = version_route.build();
    router->add_route(version_built);
    registry->register_route(version_built);

    BOOST_LOG_SEV(lg(), info) << "tenant_statuses routes registered: " << 10 << " endpoint(s)";
}

}
