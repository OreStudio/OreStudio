/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
#include "ores.http/routes/dq/bundle_roster_routes.hpp"
#include "ores.dq.api/messaging/bundle_roster_protocol.hpp"
#include "ores.http.nats/forwarding.hpp"

namespace ores::http::routes::dq {

using namespace ores::logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::dq::messaging;

void bundle_roster_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    ORES_LOG_SEV(lg(), debug) << "Registering the bundle roster route";

    auto roster_route =
        router->get("/api/v1/dq/bundle_roster")
            .summary("Read the bundle roster")
            .description("Forwards to the dq.v1.ops.bundle_roster operation. Names a bundle "
                         "to read its members with what each has staged; names none and the "
                         "answer is the roster alone.")
            .tags({"dq"})
            .operation_catalogue("dq", "bundle_roster", "read")
            .auth_required()
            .query_param("bundle_code",
                         "string",
                         "",
                         false,
                         "The bundle whose members to read, or empty for the roster alone")
            .handler([&session](const http_request& req) { return handle_roster(req, session); });
    const auto built = roster_route.build();
    router->add_route(built);
    registry->register_route(built);
}

boost::asio::awaitable<http_response> bundle_roster_routes::handle_roster(const http_request& req,
                                                                          nats_client& session) {
    try {
        messaging::bundle_roster_request msg;
        msg.bundle_code = req.get_query_param("bundle_code");
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(
            ores::http::domain::http_status::bad_request, "invalid-request", e.what());
    }
}

} // namespace ores::http::routes::dq
