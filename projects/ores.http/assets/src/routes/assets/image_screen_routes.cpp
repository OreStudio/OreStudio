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
#include "ores.http/routes/assets/image_screen_routes.hpp"
#include "ores.assets.api/messaging/image_operations_protocol.hpp"
#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/image_answer.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <charconv>
#include <exception>
#include <functional>
#include <limits>
#include <regex>
#include <tuple>
#include <type_traits>
#include <vector>

namespace ores::http::routes::assets {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::assets::messaging;
namespace rr = ores::http::net::record_routes;
using ores::http::forwarding::guarded;
using ores::http::net::record_routes::characters_of;
using ores::http::net::record_routes::refused;

namespace {

struct upload_body final {
    std::string mimeType;
    std::string data;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<upload_body>) {
    using ores::utility::codec::field;
    return std::tuple{field{"mimeType", &upload_body::mimeType}, field{"data", &upload_body::data}};
}

bool is_uuid(const std::string& text) {
    static const std::regex form(
        "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");
    return std::regex_match(text, form);
}

}

std::optional<image_screen_routes::image_page>
image_screen_routes::read_page(const http_request& req) {
    const auto number = [&](const std::string& name,
                            std::uint32_t fallback,
                            std::uint32_t least,
                            std::uint32_t most) -> std::optional<std::uint32_t> {
        const auto text = req.get_query_param(name);
        if (text.empty())
            return fallback;
        std::uint32_t value = 0;
        const auto [end, failed] = std::from_chars(text.data(), text.data() + text.size(), value);
        if (failed != std::errc{} || end != text.data() + text.size() || value < least ||
            value > most)
            return std::nullopt;
        return value;
    };
    const auto offset = number("offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
    const auto limit = number("limit", 100, 1, 500);
    const auto search = rr::trimmed(req.get_query_param("search"));
    if (!offset || !limit || characters_of(search) > 256)
        return std::nullopt;
    return image_page{*offset, *limit, search};
}

std::optional<image_screen_routes::upload>
image_screen_routes::read_upload(const std::string& body) {
    const auto parsed = json::read_expected<upload_body>(body);
    if (!parsed)
        return std::nullopt;
    const auto type = characters_of(parsed->mimeType);
    if (type < 1 || type > 100 || parsed->data.empty())
        return std::nullopt;
    return upload{parsed->mimeType, parsed->data};
}

void image_screen_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"assets"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/images"),
        "Read a page of the tenant's images",
        [&session](const http_request& req) {
            const auto page = read_page(req);
            if (!page)
                return refused(
                    http_status::bad_request,
                    "invalid-request",
                    "A page of images names an offset, a limit of 1 to 500, and a search.");
            return guarded<messaging::list_images_request>([&] {
                messaging::list_images_request list;
                list.offset = page->offset;
                list.limit = page->limit;
                if (!page->search.empty())
                    list.filter = messaging::images_filter{std::nullopt, page->search};
                const auto reply = ores::http::forwarding::forward(req, session, list);
                rr::require_done(reply.result);
                std::string out = "{\"images\":[";
                for (std::size_t i = 0; i < reply.images.size(); ++i) {
                    const auto& image = reply.images[i];
                    out += (i == 0 ? "{" : ",{");
                    out += "\"imageId\":" + json::write(boost::uuids::to_string(image.id));
                    out += ",\"code\":" + json::write(image.code);
                    out += ",\"description\":" + json::write(image.description) + "}";
                }
                return http_response::json(out + "],\"total\":" + std::to_string(reply.total) +
                                           "}");
            });
        });

    add(router->get("/api/images/{id}"),
        "Read one image's bytes",
        [&session](const http_request& req) {
            const auto id = req.get_path_param("id");
            if (!is_uuid(id))
                return refused(
                    http_status::not_found, "not-found", "No image has this identifier.");
            return guarded<messaging::list_images_request>([&] {
                messaging::list_images_request list;
                list.limit = 1;
                list.filter = messaging::images_filter{
                    std::vector<boost::uuids::uuid>{boost::uuids::string_generator()(id)},
                    std::nullopt};
                const auto reply = ores::http::forwarding::forward(req, session, list);
                rr::require_done(reply.result);
                if (reply.images.empty())
                    return refused(
                        http_status::not_found, "not-found", "No image has this identifier.");
                const auto& image = reply.images.front();
                return ores::http::net::image_answer(
                    image.mime_type, std::string(image.data.begin(), image.data.end()));
            });
        });

    add(router->get("/api/image-upload-policy"),
        "Read the policy an image upload must meet",
        [&session](const http_request& req) {
            return guarded<messaging::get_image_upload_policy_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req, session, messaging::get_image_upload_policy_request{});
                rr::require_done(reply.result);
                return http_response::json(
                    "{\"formats\":" + json::write(reply.formats) +
                    ",\"maxSizeBytes\":" + std::to_string(reply.max_size_bytes) +
                    ",\"minWidth\":" + std::to_string(reply.min_width) +
                    ",\"minHeight\":" + std::to_string(reply.min_height) + "}");
            });
        });

    add(router->post("/api/images").max_body(upload_body_bytes),
        "Upload an image",
        [&session](const http_request& req) {
            const auto image = read_upload(req.body);
            if (!image)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "An upload needs a media type and the image bytes.");
            // The upload answers 200 with the outcome in its result, as the BFF did:
            // the screen reads a refused upload's reason from there.
            return guarded<messaging::upload_image_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req, session, messaging::upload_image_request{image->mime_type, image->data});
                return http_response::json("{\"result\":" + json::write(reply.result) +
                                           ",\"imageId\":" + json::write(reply.image_id) + "}");
            });
        });
}

}
