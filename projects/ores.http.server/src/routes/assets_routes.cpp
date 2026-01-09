/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.server/routes/assets_routes.hpp"

#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.assets/messaging/assets_protocol.hpp"
#include "ores.assets/service/assets_service.hpp"

namespace ores::http_server::routes {

using namespace ores::logging;
using namespace ores::http::domain;
namespace asio = boost::asio;

assets_routes::assets_routes(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : ctx_(std::move(ctx))
    , sessions_(std::move(sessions)) {
    BOOST_LOG_SEV(lg(), debug) << "Assets routes initialized";
}

void assets_routes::register_routes(std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> registry) {

    BOOST_LOG_SEV(lg(), info) << "Registering Assets routes";

    auto get_images = router->post("/api/v1/assets/images")
        .summary("Get images")
        .description("Retrieve images by ID (batch, max 100)")
        .tags({"assets"})
        .auth_required()
        .body<assets::messaging::get_images_request>()
        .response<assets::messaging::get_images_response>()
        .handler([this](const http_request& req) { return handle_get_images(req); });
    router->add_route(get_images.build());
    registry->register_route(get_images.build());

    BOOST_LOG_SEV(lg(), info) << "Assets routes registered: 1 endpoint";
}

asio::awaitable<http_response> assets_routes::handle_get_images(const http_request& req) {
    BOOST_LOG_SEV(lg(), debug) << "Handling get images request";

    try {
        auto get_req = rfl::json::read<assets::messaging::get_images_request>(req.body);
        if (!get_req) {
            co_return http_response::bad_request("Invalid request body");
        }

        if (get_req->image_ids.size() > 100) {
            co_return http_response::bad_request("Maximum 100 images per request");
        }

        assets::service::assets_service service(ctx_);
        auto images = service.get_images(get_req->image_ids);

        assets::messaging::get_images_response resp;
        resp.images = images;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get images error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

}
