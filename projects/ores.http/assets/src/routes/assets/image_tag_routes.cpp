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
#include "ores.http/routes/assets/image_tag_routes.hpp"
#include "ores.assets.api/messaging/image_tag_protocol.hpp"
#include "ores.http.nats/forwarding.hpp"
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

namespace ores::http::routes::assets {

using namespace logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::assets::messaging;

namespace {

/**
 * @brief The request a body states, or nothing when it states none.
 *
 * The request's own codec reads it, so this unit needs no shape of the types the
 * request holds.
 */
template <typename T>
std::optional<T> image_tag_read_body(std::string_view body) {
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
T image_tag_read_param(const std::string& raw, const std::string& name) {
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
void image_tag_apply_page(Request& msg, const http_request& req) {
    if (const auto raw = req.get_query_param("offset"); !raw.empty()) {
        msg.offset = image_tag_read_param<std::uint32_t>(raw, "offset");
    }
    if (const auto raw = req.get_query_param("limit"); !raw.empty()) {
        msg.limit = image_tag_read_param<std::uint32_t>(raw, "limit");
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
std::vector<T> image_tag_read_list(const std::string& raw, const std::string& name) {
    std::vector<T> values;
    std::string::size_type begin = 0;
    while (true) {
        const auto end = raw.find(',', begin);
        const auto item = raw.substr(begin, end == std::string::npos ? end : end - begin);
        if (item.empty()) {
            throw std::runtime_error("Invalid value for " + name + ": " + raw);
        }
        values.push_back(image_tag_read_param<T>(item, name));
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
image_tag_refuse_undeclared_filters(const http_request& req,
                                    std::initializer_list<std::string_view> declared) {
    for (const auto& [name, value] : req.query_params) {
        if (name.starts_with("f.") &&
            std::find(declared.begin(), declared.end(), name) == declared.end()) {
            throw std::runtime_error("This list cannot be filtered by " + name.substr(2) + ".");
        }
    }
}

}

boost::asio::awaitable<http_response> image_tag_routes::handle_list(const http_request& req,
                                                                    nats_client& session) {
    using request_type = messaging::list_image_tags_request;
    try {
        request_type msg;
        image_tag_refuse_undeclared_filters(req, {});
        image_tag_apply_page(msg, req);
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> image_tag_routes::handle_get(const http_request& req,
                                                                   nats_client& session) {
    using request_type = messaging::get_image_tag_request;
    try {
        request_type msg;
        msg.key.image_id =
            image_tag_read_param<boost::uuids::uuid>(req.get_path_param("image_id"), "image_id");
        msg.key.tag_id =
            image_tag_read_param<boost::uuids::uuid>(req.get_path_param("tag_id"), "tag_id");
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> image_tag_routes::handle_get_many(const http_request& req,
                                                                        nats_client& session) {
    using request_type = messaging::get_many_image_tags_request;
    try {
        const auto parsed = image_tag_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> image_tag_routes::handle_add(const http_request& req,
                                                                   nats_client& session) {
    using request_type = messaging::put_image_tag_request;
    try {
        auto parsed = image_tag_read_body<request_type>(req.body);
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

boost::asio::awaitable<http_response> image_tag_routes::handle_set(const http_request& req,
                                                                   nats_client& session) {
    using request_type = messaging::put_image_tag_request;
    try {
        auto parsed = image_tag_read_body<request_type>(req.body);
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
                image_tag_read_param<std::uint32_t>(raw, "version");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> image_tag_routes::handle_put_many(const http_request& req,
                                                                        nats_client& session) {
    using request_type = messaging::put_many_image_tags_request;
    try {
        const auto parsed = image_tag_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> image_tag_routes::handle_delete(const http_request& req,
                                                                      nats_client& session) {
    using request_type = messaging::delete_image_tag_request;
    try {
        request_type msg;
        msg.removal.key.image_id =
            image_tag_read_param<boost::uuids::uuid>(req.get_path_param("image_id"), "image_id");
        msg.removal.key.tag_id =
            image_tag_read_param<boost::uuids::uuid>(req.get_path_param("tag_id"), "tag_id");
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
            msg.removal.precondition.version = image_tag_read_param<std::uint32_t>(raw, "version");
        }
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> image_tag_routes::handle_delete_many(const http_request& req,
                                                                           nats_client& session) {
    using request_type = messaging::delete_many_image_tags_request;
    try {
        const auto parsed = image_tag_read_body<request_type>(req.body);
        if (!parsed) {
            co_return http_response::api_error(
                http_status::bad_request, "invalid-request", "Invalid request body");
        }
        co_return ores::http::forwarding::respond(req, session, *parsed);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

boost::asio::awaitable<http_response> image_tag_routes::handle_by_image_id(const http_request& req,
                                                                           nats_client& session) {
    using request_type = messaging::list_by_image_id_image_tags_request;
    try {
        request_type msg;
        msg.image_id =
            image_tag_read_param<boost::uuids::uuid>(req.get_path_param("image_id"), "image_id");
        if (const auto raw = req.get_query_param("scope"); !raw.empty()) {
            msg.scope = raw == "subtree" ? ores::utility::domain::scope::subtree :
                                           ores::utility::domain::scope::direct;
        }
        image_tag_apply_page(msg, req);
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(http_status::bad_request, "invalid-request", e.what());
    }
}

void image_tag_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    BOOST_LOG_SEV(lg(), info) << "Registering image_tags routes";

    auto list_route =
        router->get("/api/v1/assets/image_tags")
            .summary("List image tags")
            .description("Forwards to the assets.v1.image_tags.list operation.")
            .tags({"assets"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("sort", "string", "", false, "Column to order")
            .query_param("descending", "boolean", "", false, "Descending")
            .response<messaging::list_image_tags_response>()
            .handler([&session](const http_request& req) { return handle_list(req, session); });
    const auto list_built = list_route.build();
    router->add_route(list_built);
    registry->register_route(list_built);

    auto get_route =
        router->get("/api/v1/assets/image_tags/{image_id}/{tag_id}")
            .summary("Get one image tag")
            .description("Forwards to the assets.v1.image_tags.get operation.")
            .tags({"assets"})
            .auth_required()
            .response<messaging::get_image_tag_response>()
            .handler([&session](const http_request& req) { return handle_get(req, session); });
    const auto get_built = get_route.build();
    router->add_route(get_built);
    registry->register_route(get_built);

    auto get_many_route =
        router->post("/api/v1/assets/image_tags/get-many")
            .summary("Get many image tags")
            .description("Forwards to the assets.v1.image_tags.get_many operation.")
            .tags({"assets"})
            .auth_required()
            .body<messaging::get_many_image_tags_request>()
            .response<messaging::get_many_image_tags_response>()
            .handler([&session](const http_request& req) { return handle_get_many(req, session); });
    const auto get_many_built = get_many_route.build();
    router->add_route(get_many_built);
    registry->register_route(get_many_built);

    auto add_route =
        router->post("/api/v1/assets/image_tags")
            .summary("Create one image tag")
            .description("Forwards to the assets.v1.image_tags.put operation.")
            .tags({"assets"})
            .auth_required()
            .body<messaging::put_image_tag_request>()
            .response<messaging::put_image_tag_response>()
            .handler([&session](const http_request& req) { return handle_add(req, session); });
    const auto add_built = add_route.build();
    router->add_route(add_built);
    registry->register_route(add_built);

    auto set_route =
        router->put("/api/v1/assets/image_tags")
            .summary("Replace one image tag")
            .description("Forwards to the assets.v1.image_tags.put operation.")
            .tags({"assets"})
            .auth_required()
            .query_param("version", "integer", "", false, "Version")
            .body<messaging::put_image_tag_request>()
            .response<messaging::put_image_tag_response>()
            .handler([&session](const http_request& req) { return handle_set(req, session); });
    const auto set_built = set_route.build();
    router->add_route(set_built);
    registry->register_route(set_built);

    auto put_many_route =
        router->post("/api/v1/assets/image_tags/put-many")
            .summary("Write many image tags")
            .description("Forwards to the assets.v1.image_tags.put_many operation.")
            .tags({"assets"})
            .auth_required()
            .body<messaging::put_many_image_tags_request>()
            .response<messaging::put_many_image_tags_response>()
            .handler([&session](const http_request& req) { return handle_put_many(req, session); });
    const auto put_many_built = put_many_route.build();
    router->add_route(put_many_built);
    registry->register_route(put_many_built);

    auto delete_route =
        router->delete_("/api/v1/assets/image_tags/{image_id}/{tag_id}")
            .summary("Delete one image tag")
            .description("Forwards to the assets.v1.image_tags.delete operation.")
            .tags({"assets"})
            .auth_required()
            .query_param("reason", "string", "", true, "Change reason code")
            .query_param("commentary", "string", "", true, "Commentary")
            .query_param("version", "integer", "", false, "Version")
            .response<messaging::delete_image_tag_response>()
            .handler([&session](const http_request& req) { return handle_delete(req, session); });
    const auto delete_built = delete_route.build();
    router->add_route(delete_built);
    registry->register_route(delete_built);

    auto delete_many_route =
        router->post("/api/v1/assets/image_tags/delete-many")
            .summary("Delete many image tags")
            .description("Forwards to the assets.v1.image_tags.delete_many operation.")
            .tags({"assets"})
            .auth_required()
            .body<messaging::delete_many_image_tags_request>()
            .response<messaging::delete_many_image_tags_response>()
            .handler(
                [&session](const http_request& req) { return handle_delete_many(req, session); });
    const auto delete_many_built = delete_many_route.build();
    router->add_route(delete_many_built);
    registry->register_route(delete_many_built);

    auto by_image_id_route =
        router->get("/api/v1/assets/image_tags/by-image-id/{image_id}")
            .summary("List the image tags that reference one image id")
            .description("Forwards to the assets.v1.image_tags.list_by_image_id operation.")
            .tags({"assets"})
            .auth_required()
            .query_param("offset", "integer", "", false, "Rows to skip")
            .query_param("limit", "integer", "", false, "Rows to return")
            .query_param("sort", "string", "", false, "Column to order")
            .query_param("descending", "boolean", "", false, "Descending")
            .query_param("scope", "string", "", false, "direct or subtree")
            .response<messaging::list_by_image_id_image_tags_response>()
            .handler(
                [&session](const http_request& req) { return handle_by_image_id(req, session); });
    const auto by_image_id_built = by_image_id_route.build();
    router->add_route(by_image_id_built);
    registry->register_route(by_image_id_built);

    BOOST_LOG_SEV(lg(), info) << "image_tags routes registered: " << 9 << " endpoint(s)";
}

}
