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
#include "ores.http/routes/dq/dataset_publication_routes.hpp"
#include "ores.dq.api/messaging/dataset_publications_protocol.hpp"
#include "ores.http.nats/forwarding.hpp"

namespace ores::http::routes::dq {

using namespace ores::logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::dq::messaging;

void dataset_publication_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    ORES_LOG_SEV(lg(), debug) << "Registering the dataset publications route";

    auto publications_route =
        router->get("/api/v1/dq/dataset_publications")
            .summary("Read the runs that published one dataset")
            .description("Forwards to the dq.v1.ops.dataset_publications operation: every run "
                         "that published the named dataset, newest first, with what each wrote "
                         "and what it said.")
            .tags({"dq"})
            // A tool names one operation, and the roster already holds "read" in
            // this area, so this route states a name of its own.
            .operation_catalogue("dq", "dataset_publications", "dataset_publications_read")
            .auth_required()
            .query_param(
                "dataset_code", "string", "", true, "The dataset whose publication runs to read")
            .handler(
                [&session](const http_request& req) { return handle_publications(req, session); });
    const auto built = publications_route.build();
    router->add_route(built);
    registry->register_route(built);
}

boost::asio::awaitable<http_response>
dataset_publication_routes::handle_publications(const http_request& req, nats_client& session) {
    try {
        messaging::dataset_publications_request msg;
        msg.dataset_code = req.get_query_param("dataset_code");
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(
            ores::http::domain::http_status::bad_request, "invalid-request", e.what());
    }
}

} // namespace ores::http::routes::dq
