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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/reporting_routes.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
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
using ores::nats::service::nats_client;

namespace health {

/// The definitions of one party, counted by the health word each carries.
std::string tally_of(const http_request& req, nats_client& session) {
    const auto party = req.get_query_param("party_id");
    reporting_messaging::count_report_definitions_by_health_request msg;
    if (!party.empty()) {
        if (!rr::is_uuid(party))
            rr::invalid("A health tally names a party by its id.");
        msg.party_id = boost::uuids::string_generator()(party);
    }
    const auto search = rr::trimmed(req.get_query_param("search"));
    if (search.size() > 256)
        rr::invalid("A health tally names a search of at most 256 characters.");
    if (!search.empty())
        msg.search = search;

    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);

    std::vector<std::string> counts;
    counts.reserve(reply.counts.size());
    for (const auto& entry : reply.counts)
        counts.push_back("{\"health\":" + json::write(entry.health) +
                         ",\"count\":" + std::to_string(entry.count) + "}");
    return "{\"counts\":" + rr::array_of(counts) + ",\"total\":" + std::to_string(reply.total) +
           "}";
}

}

}

void reporting_routes::register_health_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto built =
        router->get("/api/reporting/report-health")
            .summary("The report definitions of a party, counted by their health")
            .description("Each health word with the number of definitions that "
                         "carry it, in the service's own order, and the total it "
                         "counted.")
            .tags({"reporting"})
            .auth_required()
            .query_param("party_id", "string", "", false, "The party to count")
            .query_param("search",
                         "string",
                         "",
                         false,
                         "A text the definition's "
                         "name holds")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::count_report_definitions_by_health_request>(
                    [&] {
                        rr::refuse_unknown_params(req, {"party_id", "search"});
                        return http_response::json(health::tally_of(req, session));
                    });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
