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
#include "ores.http/routes/refdata/curve_configuration_screen_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/curve_configuration_protocol.hpp"
#include "ores.refdata.api/messaging/curve_definition_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace ores::http::routes::refdata {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace refdata_messaging = ores::refdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// How many curves one configuration holds, and how many sections they name.
struct curve_tally final {
    std::set<std::string> sections;
    std::size_t curves = 0;
};

/// The tallies of every configuration, by its id.
std::map<std::string, curve_tally> tallies_of(const http_request& req, nats_client& session) {
    std::map<std::string, curve_tally> tallies;
    for (const auto& definition :
         forwarding::forward_all(req,
                                 session,
                                 refdata_messaging::list_curve_definitions_request{},
                                 &refdata_messaging::list_curve_definitions_response::curves)) {
        auto& tally = tallies[boost::uuids::to_string(definition.curve_configuration_id)];
        tally.sections.insert(definition.section_code);
        ++tally.curves;
    }
    return tallies;
}

}

void curve_configuration_screen_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"refdata"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/refdata/curve-configurations")
            .summary("The curve configurations a party holds, in name order")
            .description("One row per configuration with how many curves it holds and how many "
                         "sections they name, so a person reaches a configuration without a "
                         "report that binds it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::list_curve_configurations_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto configurations = forwarding::forward_all(
                        req,
                        session,
                        refdata_messaging::list_curve_configurations_request{},
                        &refdata_messaging::list_curve_configurations_response::configurations);
                    const auto tallies = tallies_of(req, session);

                    struct row final {
                        std::string id;
                        std::string configuration_id;
                        std::string name;
                        std::string description;
                        int version = 0;
                        std::size_t sections = 0;
                        std::size_t curves = 0;
                    };
                    std::vector<row> rows;
                    rows.reserve(configurations.size());
                    for (const auto& configuration : configurations) {
                        const auto id = boost::uuids::to_string(configuration.id);
                        const auto tally = tallies.find(id);
                        rows.push_back({id,
                                        boost::uuids::to_string(configuration.configuration_id),
                                        configuration.name,
                                        configuration.description.value_or(std::string()),
                                        configuration.version,
                                        tally == tallies.end() ? 0 : tally->second.sections.size(),
                                        tally == tallies.end() ? 0 : tally->second.curves});
                    }
                    std::ranges::stable_sort(rows, {}, [](const row& one) { return one.name; });

                    std::vector<std::string> written;
                    written.reserve(rows.size());
                    for (const auto& one : rows)
                        written.push_back(
                            "{\"configurationId\":" + json::write(one.configuration_id) +
                            ",\"id\":" + json::write(one.id) +
                            ",\"name\":" + json::write(one.name) +
                            ",\"description\":" + json::write(one.description) +
                            ",\"version\":" + std::to_string(one.version) +
                            ",\"sections\":" + std::to_string(one.sections) +
                            ",\"curves\":" + std::to_string(one.curves) + "}");
                    return http_response::json("{\"rows\":" + rr::array_of(written) +
                                               ",\"total\":" + std::to_string(rows.size()) + "}");
                });
            }));
}

}
