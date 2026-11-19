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
#include "ores.http/routes/workflow/retry_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::http::routes::workflow {

namespace retries {

/// A retry as the page states it: the step to resume from, or none.
struct retry_body final {
    std::optional<std::string> step_name;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<retry_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"stepName", &retry_body::step_name}};
}

}

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace workflow_messaging = ores::workflow::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The sentence a retry states when its address names no run.
constexpr std::string_view retry_run_refused = "A run is named by its identifier.";

/// The sentence a retry states when its body is not the shape it takes.
constexpr std::string_view retry_body_refused =
    "A retry names the step to resume from, or no step at all.";

/**
 * The retry the page states.
 *
 * The body is the one member a page may leave out, and it leaves the whole body
 * out when it resumes where the run stopped: the engine reads an empty step as
 * that step, so a missing body and a body naming no step are the same retry.
 */
retries::retry_body retry_of(const http_request& req) {
    if (rr::trimmed(req.body).empty())
        return retries::retry_body{};
    return rr::lenient_body_of<retries::retry_body>(
        req, {"stepName"}, std::string(retry_body_refused));
}

}

void retry_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                   std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                   nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/workflow/{instanceId}/retry")
            .summary("Resume a stopped run from the step the page chose")
            .description("The engine decides: it re-dispatches the step the body names, or the "
                         "step the run stopped at when the body names none, and an answer that "
                         "did not resume says why. The page re-reads the run's progress either "
                         "way, so a refusal is an answer and not a fault.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<workflow_messaging::retry_workflow_instance_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto instance_id = req.get_path_param("instanceId");
                    if (!rr::is_uuid(instance_id))
                        rr::invalid(std::string(retry_run_refused));
                    const auto body = retry_of(req);

                    workflow_messaging::retry_workflow_instance_request msg;
                    msg.workflow_instance_id = instance_id;
                    msg.step_name = body.step_name.value_or(std::string());
                    const auto reply = forwarding::forward(req, session, msg);
                    return http_response::json(
                        "{\"success\":" + json::write(reply.success) +
                        ",\"message\":" + json::write(reply.message) +
                        ",\"instanceId\":" + json::write(reply.workflow_instance_id) +
                        ",\"stepIndex\":" + std::to_string(reply.step_index) +
                        ",\"stepName\":" + json::write(reply.step_name) + "}");
                });
            }));
}

}
