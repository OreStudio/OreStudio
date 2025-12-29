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
#include "ores.http.server/routes/variability_routes.hpp"

#include <rfl/json.hpp>
#include "ores.variability/messaging/feature_flags_protocol.hpp"

namespace ores::http_server::routes {

using namespace ores::telemetry::log;
using namespace ores::http::domain;
namespace asio = boost::asio;

variability_routes::variability_routes(
    std::shared_ptr<variability::service::system_flags_service> system_flags,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : system_flags_(std::move(system_flags))
    , sessions_(std::move(sessions)) {
    BOOST_LOG_SEV(lg(), debug) << "Variability routes initialized";
}

void variability_routes::register_routes(std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> registry) {

    BOOST_LOG_SEV(lg(), info) << "Registering Variability routes";

    auto list_flags = router->get("/api/v1/feature-flags")
        .summary("List feature flags")
        .description("Retrieve all feature flags in the system")
        .tags({"feature-flags"})
        .auth_required()
        .handler([this](const http_request& req) { return handle_list_feature_flags(req); });
    router->add_route(list_flags.build());
    registry->register_route(list_flags.build());

    BOOST_LOG_SEV(lg(), info) << "Variability routes registered: 1 endpoint";
}

asio::awaitable<http_response> variability_routes::handle_list_feature_flags(const http_request&) {
    BOOST_LOG_SEV(lg(), debug) << "Handling list feature flags request";

    try {
        auto flags = system_flags_->list_all_flags();

        variability::messaging::list_feature_flags_response resp;
        resp.flags = flags;

        co_return http_response::json(rfl::json::write(resp));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List feature flags error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

}
