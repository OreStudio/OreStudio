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
#include "ores.http/routes/workflow/progress_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <chrono>
#include <string>
#include <string_view>
#include <utility>

namespace ores::http::routes::workflow {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace workflow_messaging = ores::workflow::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The sentence the view states when its address names no run.
constexpr std::string_view progress_run_refused = "A run is named by its identifier.";

/// How long the answer stays fresh, which is the interval a page asks again.
constexpr std::chrono::seconds progress_stale_after{5};

}

http_response progress_routes::progress_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req, std::move(body), ores::http::net::view_cache_policy::fresh_for(progress_stale_after));
}

void progress_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/workflow/{instanceId}")
            .summary("A run's progress, as a journey's rail renders it")
            .description("A composed view owned by ores.workflow, answered from one read: the "
                         "run's status, the step it is executing and one summary per step, so a "
                         "page follows the run by asking this again rather than by holding the "
                         "run's state. There is no single current step, because a run whose "
                         "shape is a graph may have several in flight at once. The answer "
                         "carries an ETag and stays fresh for 5 seconds, which is the interval "
                         "the page asks again.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<workflow_messaging::get_workflow_steps_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto instance_id = req.get_path_param("instanceId");
                    if (!rr::is_uuid(instance_id))
                        rr::invalid(std::string(progress_run_refused));

                    workflow_messaging::get_workflow_steps_request msg;
                    msg.workflow_instance_id = instance_id;
                    /*
                     * No outcome check, deliberately. A refusal arrives as a
                     * transport refusal, which the forward raises before this
                     * line, and everything the engine answers in band is the
                     * page's own news: `success` and `message` are what the rail
                     * renders when a run could not be read. The BFF passed the
                     * reply through the same way, and turning the engine's words
                     * into a 502 here would hide the state the screen is for.
                     */
                    const auto reply = forwarding::forward(req, session, msg);
                    return progress_answer(
                        req,
                        ores::utility::codec::wire_traits<
                            workflow_messaging::get_workflow_steps_response>::write_json(reply));
                });
            }));
}

}
