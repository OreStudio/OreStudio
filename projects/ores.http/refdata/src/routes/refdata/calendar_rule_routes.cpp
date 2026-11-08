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
#include "ores.http/routes/refdata/calendar_rule_routes.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.refdata.api/messaging/calendar_rule_protocol.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <exception>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

namespace ores::http::routes::refdata {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::refdata::messaging;

namespace {

/**
 * @brief The request a body states, or nothing when it states none.
 *
 * The request's own codec reads it, so this unit needs no shape of the types the
 * request holds.
 */
template <typename T>
std::optional<T> calendar_rule_read_body(std::string_view body) {
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
T calendar_rule_read_param(const std::string& raw, const std::string& name) {
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
http_response calendar_rule_error_response(const std::string& error) {
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
calendar_rule_forward(const http_request& req, nats_client& session, const Request& msg) {
    auto delegated = session.with_delegation(req.get_bearer_token().value_or(std::string{}));
    try {
        const auto reply =
            delegated.authenticated_request(msg.nats_subject,
                                            ores::nats::default_wire_codec().encode(msg),
                                            ores::nats::service::default_request_timeout);
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end()) {
            co_return calendar_rule_error_response(it->second);
        }
        const auto decoded =
            ores::nats::default_wire_codec().decode<typename Request::response_type>(reply.data);
        if (!decoded) {
            co_return http_response::internal_error(std::string("Failed to decode response: ") +
                                                    decoded.error().what());
        }
        co_return http_response::json(
            ores::utility::codec::wire_traits<typename Request::response_type>::write_json(
                *decoded));
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
void calendar_rule_apply_page(Request& msg, const http_request& req) {
    if (const auto raw = req.get_query_param("offset"); !raw.empty()) {
        msg.offset = calendar_rule_read_param<std::uint32_t>(raw, "offset");
    }
    if (const auto raw = req.get_query_param("limit"); !raw.empty()) {
        msg.limit = calendar_rule_read_param<std::uint32_t>(raw, "limit");
    }
    if (const auto raw = req.get_query_param("order"); !raw.empty()) {
        msg.order.field = raw;
    }
    msg.order.descending = req.get_query_param("desc") == "true";
}

}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_list(const http_request& req,
                                                                        nats_client& session) {
    using request_type = messaging::list_calendar_rules_request;
    try {
        request_type msg;
        calendar_rule_apply_page(msg, req);
        if (const auto raw = req.get_query_param("f.calendar_code"); !raw.empty()) {
            if (!msg.filter)
                msg.filter = messaging::calendar_rules_filter{};
            msg.filter->calendar_code =
                calendar_rule_read_param<std::string>(raw, "f.calendar_code");
        }
        co_return co_await calendar_rule_forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_get(const http_request& req,
                                                                       nats_client& session) {
    using request_type = messaging::get_calendar_rule_request;
    try {
        request_type msg;
        msg.key.id = calendar_rule_read_param<boost::uuids::uuid>(req.get_path_param("id"), "id");
        co_return co_await calendar_rule_forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_get_many(const http_request& req,
                                                                            nats_client& session) {
    using request_type = messaging::get_many_calendar_rules_request;
    try {
        const auto parsed = calendar_rule_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await calendar_rule_forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_add(const http_request& req,
                                                                       nats_client& session) {
    using request_type = messaging::put_calendar_rule_request;
    try {
        auto parsed = calendar_rule_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        // The route states whether the row may exist; the body states the rest.
        parsed->change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
        co_return co_await calendar_rule_forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_set(const http_request& req,
                                                                       nats_client& session) {
    using request_type = messaging::put_calendar_rule_request;
    try {
        auto parsed = calendar_rule_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        // The route states whether the row may exist; the body states the rest.
        parsed->change.precondition.kind = ores::utility::domain::precondition_kind::any;
        if (const auto raw = req.get_query_param("version"); !raw.empty()) {
            parsed->change.precondition.kind =
                ores::utility::domain::precondition_kind::must_match_version;
            parsed->change.precondition.version =
                calendar_rule_read_param<std::uint32_t>(raw, "version");
        }
        co_return co_await calendar_rule_forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_put_many(const http_request& req,
                                                                            nats_client& session) {
    using request_type = messaging::put_many_calendar_rules_request;
    try {
        const auto parsed = calendar_rule_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await calendar_rule_forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_delete(const http_request& req,
                                                                          nats_client& session) {
    using request_type = messaging::delete_calendar_rule_request;
    try {
        request_type msg;
        msg.removal.key.id =
            calendar_rule_read_param<boost::uuids::uuid>(req.get_path_param("id"), "id");
        // A delete carries no body, so its intent arrives as query parameters.
        msg.intent.reason_code = req.get_query_param("reason");
        msg.intent.commentary = req.get_query_param("commentary");
        if (msg.intent.reason_code.empty()) {
            co_return http_response::bad_request("reason is required");
        }
        if (msg.intent.commentary.empty()) {
            co_return http_response::bad_request("commentary is required");
        }
        if (const auto raw = req.get_query_param("version"); !raw.empty()) {
            msg.removal.precondition.kind =
                ores::utility::domain::precondition_kind::must_match_version;
            msg.removal.precondition.version =
                calendar_rule_read_param<std::uint32_t>(raw, "version");
        }
        co_return co_await calendar_rule_forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
calendar_rule_routes::handle_delete_many(const http_request& req, nats_client& session) {
    using request_type = messaging::delete_many_calendar_rules_request;
    try {
        const auto parsed = calendar_rule_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::bad_request("Invalid request body");
        }
        co_return co_await calendar_rule_forward(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response>
calendar_rule_routes::handle_by_calendar_code(const http_request& req, nats_client& session) {
    using request_type = messaging::list_by_calendar_code_calendar_rules_request;
    try {
        request_type msg;
        msg.calendar_code = calendar_rule_read_param<std::string>(
            req.get_path_param("calendar_code"), "calendar_code");
        if (const auto raw = req.get_query_param("scope"); !raw.empty()) {
            msg.scope = raw == "subtree" ? ores::utility::domain::scope::subtree :
                                           ores::utility::domain::scope::direct;
        }
        calendar_rule_apply_page(msg, req);
        co_return co_await calendar_rule_forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_versions(const http_request& req,
                                                                            nats_client& session) {
    using request_type = messaging::list_calendar_rule_versions_request;
    try {
        request_type msg;
        msg.key.id = calendar_rule_read_param<boost::uuids::uuid>(req.get_path_param("id"), "id");
        calendar_rule_apply_page(msg, req);
        co_return co_await calendar_rule_forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

boost::asio::awaitable<http_response> calendar_rule_routes::handle_version(const http_request& req,
                                                                           nats_client& session) {
    using request_type = messaging::get_calendar_rule_version_request;
    try {
        request_type msg;
        msg.key.calendar_rule.id =
            calendar_rule_read_param<boost::uuids::uuid>(req.get_path_param("id"), "id");
        msg.key.version =
            calendar_rule_read_param<std::uint32_t>(req.get_path_param("version"), "version");
        co_return co_await calendar_rule_forward(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::bad_request(e.what());
    }
}

void calendar_rule_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering calendar_rules routes";

    auto list_route =
        router->get("/api/v1/reference-data/calendar_rules")
            .summary("List calendar rules")
            .description("Forwards to the refdata.v1.calendar_rules.list operation.")
            .tags({"refdata"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("order", "string", "", false, "Column to order")
            .query_param("desc", "boolean", "", false, "Descending")
            .query_param(
                "f.calendar_code", "string", "", false, "Rows whose calendar_code is this value")
            .response<messaging::list_calendar_rules_response>()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto get_route =
        router->get("/api/v1/reference-data/calendar_rules/{id}")
            .summary("Get one calendar rule")
            .description("Forwards to the refdata.v1.calendar_rules.get operation.")
            .tags({"refdata"})
            .auth_required()
            .response<messaging::get_calendar_rule_response>()
            .handler([&session](const http_request& req) { return handle_get(req, session); });
    const auto get_built = get_route.build();
    router->add_route(get_built);
    registry->register_route(get_built);

    auto get_many_route =
        router->post("/api/v1/reference-data/calendar_rules/get-many")
            .summary("Get many calendar rules")
            .description("Forwards to the refdata.v1.calendar_rules.get_many operation.")
            .tags({"refdata"})
            .auth_required()
            .body<messaging::get_many_calendar_rules_request>()
            .response<messaging::get_many_calendar_rules_response>()
            .handler([&session](const http_request& req) { return handle_get_many(req, session); });
    const auto get_many_built = get_many_route.build();
    router->add_route(get_many_built);
    registry->register_route(get_many_built);

    auto add_route =
        router->post("/api/v1/reference-data/calendar_rules")
            .summary("Create one calendar rule")
            .description("Forwards to the refdata.v1.calendar_rules.put operation.")
            .tags({"refdata"})
            .auth_required()
            .body<messaging::put_calendar_rule_request>()
            .response<messaging::put_calendar_rule_response>()
            .handler([&session](const http_request& req) { return handle_add(req, session); });
    const auto add_built = add_route.build();
    router->add_route(add_built);
    registry->register_route(add_built);

    auto set_route =
        router->put("/api/v1/reference-data/calendar_rules")
            .summary("Replace one calendar rule")
            .description("Forwards to the refdata.v1.calendar_rules.put operation.")
            .tags({"refdata"})
            .auth_required()
            .query_param("version", "integer", "", false, "Version")
            .body<messaging::put_calendar_rule_request>()
            .response<messaging::put_calendar_rule_response>()
            .handler([&session](const http_request& req) { return handle_set(req, session); });
    const auto set_built = set_route.build();
    router->add_route(set_built);
    registry->register_route(set_built);

    auto put_many_route =
        router->post("/api/v1/reference-data/calendar_rules/put-many")
            .summary("Write many calendar rules")
            .description("Forwards to the refdata.v1.calendar_rules.put_many operation.")
            .tags({"refdata"})
            .auth_required()
            .body<messaging::put_many_calendar_rules_request>()
            .response<messaging::put_many_calendar_rules_response>()
            .handler([&session](const http_request& req) { return handle_put_many(req, session); });
    const auto put_many_built = put_many_route.build();
    router->add_route(put_many_built);
    registry->register_route(put_many_built);

    auto delete_route =
        router->delete_("/api/v1/reference-data/calendar_rules/{id}")
            .summary("Delete one calendar rule")
            .description("Forwards to the refdata.v1.calendar_rules.delete operation.")
            .tags({"refdata"})
            .auth_required()
            .query_param("reason", "string", "", true, "Change reason code")
            .query_param("commentary", "string", "", true, "Commentary")
            .query_param("version", "integer", "", false, "Version")
            .response<messaging::delete_calendar_rule_response>()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto delete_built = delete_route.build();
    router->add_route(delete_built);
    registry->register_route(delete_built);

    auto delete_many_route =
        router->post("/api/v1/reference-data/calendar_rules/delete-many")
            .summary("Delete many calendar rules")
            .description("Forwards to the refdata.v1.calendar_rules.delete_many operation.")
            .tags({"refdata"})
            .auth_required()
            .body<messaging::delete_many_calendar_rules_request>()
            .response<messaging::delete_many_calendar_rules_response>()
            .handler(
                [&session](const http_request& req) { return handle_delete_many(req, session); });
    const auto delete_many_built = delete_many_route.build();
    router->add_route(delete_many_built);
    registry->register_route(delete_many_built);

    auto by_calendar_code_route =
        router->get("/api/v1/reference-data/calendar_rules/by-calendar-code/{calendar_code}")
            .summary("List the calendar rules that reference one calendar code")
            .description(
                "Forwards to the refdata.v1.calendar_rules.list_by_calendar_code operation.")
            .tags({"refdata"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("order", "string", "", false, "Column to order")
            .query_param("desc", "boolean", "", false, "Descending")
            .query_param("scope", "string", "", false, "direct or subtree")
            .response<messaging::list_by_calendar_code_calendar_rules_response>()
            .handler([&session](const http_request& req) {
                return handle_by_calendar_code(req, session);
            });
    const auto by_calendar_code_built = by_calendar_code_route.build();
    router->add_route(by_calendar_code_built);
    registry->register_route(by_calendar_code_built);

    auto versions_route =
        router->get("/api/v1/reference-data/calendar_rules/{id}/versions")
            .summary("List the recorded versions of one calendar rule")
            .description("Forwards to the refdata.v1.calendar_rules_versions.list operation.")
            .tags({"refdata"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("order", "string", "", false, "Column to order")
            .query_param("desc", "boolean", "", false, "Descending")
            .response<messaging::list_calendar_rule_versions_response>()
            .handler([&session](const http_request& req) { return handle_versions(req, session); });
    const auto versions_built = versions_route.build();
    router->add_route(versions_built);
    registry->register_route(versions_built);

    auto version_route =
        router->get("/api/v1/reference-data/calendar_rules/{id}/versions/{version}")
            .summary("Get one recorded version of one calendar rule")
            .description("Forwards to the refdata.v1.calendar_rules_versions.get operation.")
            .tags({"refdata"})
            .auth_required()
            .response<messaging::get_calendar_rule_version_response>()
            .handler([&session](const http_request& req) { return handle_version(req, session); });
    const auto version_built = version_route.build();
    router->add_route(version_built);
    registry->register_route(version_built);

    BOOST_LOG_SEV(lg(), info) << "calendar_rules routes registered: " << 11 << " endpoint(s)";
}

}
