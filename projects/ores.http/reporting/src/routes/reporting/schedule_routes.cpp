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
#include "ores.http/routes/reporting/schedule_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <optional>
#include <sstream>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
using ores::utility::codec::field;

namespace schedule {

/// The most definitions one schedule call may name, as the shell allows.
constexpr std::size_t ids_limit = 100;

/// The body a schedule sets: the definitions, and the one expression it writes.
struct schedule_body final {
    std::vector<std::string> ids;
    std::optional<std::string> schedule_expression;
};

constexpr auto wire_fields(std::type_identity<schedule_body>) {
    return ores::utility::codec::field_list{field{"ids", &schedule_body::ids},
                                            field{"schedule_expression",
                                                  &schedule_body::schedule_expression}};
}

/// The body an unschedule sends.
struct unschedule_body final {
    std::vector<std::string> ids;
};

constexpr auto wire_fields(std::type_identity<unschedule_body>) {
    return ores::utility::codec::field_list{field{"ids", &unschedule_body::ids}};
}

/// The ids a body names, refused by the sentence when they are not one to a
/// hundred uuids.
std::vector<std::string> checked_ids(const std::vector<std::string>& ids,
                                     const std::string& sentence) {
    if (ids.empty() || ids.size() > ids_limit)
        rr::invalid(sentence);
    for (const auto& id : ids)
        if (!rr::is_uuid(id))
            rr::invalid(sentence);
    return ids;
}

/// What the operation did, as the screen reads it.
std::string outcome_json(int changed,
                         const std::vector<std::string>& failed_ids,
                         const std::string& message) {
    std::vector<std::string> written;
    written.reserve(failed_ids.size());
    for (const auto& id : failed_ids)
        written.push_back(json::write(id));
    return "{\"changed\":" + std::to_string(changed) +
           ",\"failedIds\":" + rr::array_of(written) + ",\"message\":" + json::write(message) +
           "}";
}

}

}

using namespace schedule;

void schedule_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/reporting/reports/schedule")
            .summary("Set the schedule of one or more report definitions")
            .description("A body that states an expression names one definition, and the "
                         "service writes the expression and makes the job in one handler, so an "
                         "expression no job honours cannot be left standing. The answer states "
                         "how many definitions it changed, which it refused and why.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::schedule_report_definitions_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body = rr::body_of<schedule_body>(req,
                                                                 {"ids", "schedule_expression"},
                                                                 "A schedule names the definitions "
                                                                 "and, for one of them, the "
                                                                 "expression it fires on.");
                    const auto ids = checked_ids(body.ids,
                                                 "A schedule names 1 to 100 definitions by their "
                                                 "ids.");

                    std::string expression;
                    if (body.schedule_expression) {
                        expression = rr::trimmed(*body.schedule_expression);
                        if (expression.empty() || expression.size() > 200)
                            rr::invalid("A schedule expression is 1 to 200 characters.");
                        if (ids.size() != 1)
                            rr::invalid("A schedule expression names one definition.");
                    }

                    reporting_messaging::schedule_report_definitions_request msg;
                    msg.ids = ids;
                    msg.schedule_expression = expression;
                    const auto reply = forwarding::forward(req, session, msg);
                    // A refusal of some definitions is the operation's answer and
                    // not a failed call: the screen draws which and why.
                    return http_response::json(outcome_json(reply.scheduled_count,
                                                            reply.failed_ids,
                                                            reply.message));
                });
            }));

    add(router->post("/api/reporting/reports/unschedule")
            .summary("Clear the schedule of one or more report definitions")
            .description("The answer states how many definitions it changed, which it refused "
                         "and the operation's own words for the first refusal.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::unschedule_report_definitions_request>(
                    [&] {
                        rr::refuse_unknown_params(req, {});
                        const auto body = rr::body_of<unschedule_body>(
                            req, {"ids"}, "An unschedule names the definitions.");
                        const auto ids = checked_ids(
                            body.ids,
                            "An unschedule names 1 to 100 definitions by their ids.");

                        reporting_messaging::unschedule_report_definitions_request msg;
                        msg.ids = ids;
                        const auto reply = forwarding::forward(req, session, msg);
                        return http_response::json(outcome_json(reply.unscheduled_count,
                                                                reply.failed_ids,
                                                                reply.message));
                    });
            }));
}

}
