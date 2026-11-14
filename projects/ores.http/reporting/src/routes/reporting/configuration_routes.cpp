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
#include "ores.http/routes/reporting/configuration_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/messaging/analytic_type_protocol.hpp"
#include "ores.reporting.api/messaging/configuration_analytic_protocol.hpp"
#include "ores.reporting.api/messaging/configuration_protocol.hpp"
#include "ores.reporting.api/messaging/configuration_type_protocol.hpp"
#include "ores.reporting.api/messaging/report_configuration_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <map>
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
namespace iam_messaging = ores::iam::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

auto& configuration_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.configuration_routes");
    return instance;
}

namespace configurations {

/// The party each of the caller's parties is, by the name a person reads.
std::map<std::string, std::string> party_names_of(const http_request& req, nats_client& session) {
    iam_messaging::get_my_parties_request msg;
    const auto reply = forwarding::forward(req, session, msg);
    std::map<std::string, std::string> names;
    for (const auto& party : reply.parties)
        names.emplace(party.party_id, party.name);
    return names;
}

/// One configuration and what its five keys point at.
std::string detail_of(const http_request& req,
                      nats_client& session,
                      const boost::uuids::uuid& id) {
    reporting_messaging::list_configurations_request asked;
    asked.limit = 1;
    asked.filter.emplace();
    asked.filter->id_one_of = std::vector{id};
    const auto found = forwarding::forward(req, session, asked);
    if (found.configurations.empty())
        rr::refuse(http_status::not_found, "not-found", "There is no such configuration.");
    const auto& configuration = found.configurations.front();

    const auto types = forwarding::forward_all(req,
                                               session,
                                               reporting_messaging::list_configuration_types_request{},
                                               &reporting_messaging::list_configuration_types_response::
                                                   types);
    std::map<std::string, std::string> type_names;
    for (const auto& type : types)
        type_names.emplace(type.code, type.name);

    const auto parties = party_names_of(req, session);

    reporting_messaging::list_configuration_analytics_request analytics_ask;
    analytics_ask.limit = 1000;
    analytics_ask.filter.emplace();
    analytics_ask.filter->configuration_id = id;
    const auto analytics = forwarding::forward(req, session, analytics_ask);

    // A refused read of the analytic types costs their names, not the page.
    std::map<std::string, std::string> analytic_names;
    try {
        const auto types_read = forwarding::forward_all(
            req,
            session,
            reporting_messaging::list_analytic_types_request{},
            &reporting_messaging::list_analytic_types_response::types);
        for (const auto& type : types_read)
            analytic_names.emplace(type.code, type.name);
    } catch (const rr::refusal& refused) {
        if (refused.status != http_status::forbidden)
            throw;
        BOOST_LOG_SEV(configuration_lg(), ores::logging::debug)
            << "The analytic types are not readable by this account, so the analytics are "
               "named by their codes.";
    }

    reporting_messaging::list_report_configurations_request bindings_ask;
    bindings_ask.limit = 1000;
    bindings_ask.filter.emplace();
    bindings_ask.filter->configuration_id = id;
    const auto bindings = forwarding::forward(req, session, bindings_ask);

    std::vector<boost::uuids::uuid> definition_ids;
    for (const auto& binding : bindings.report_configurations)
        definition_ids.push_back(binding.report_definition_id);

    std::vector<ores::reporting::domain::report_definition> definitions;
    if (!definition_ids.empty()) {
        reporting_messaging::list_report_definitions_request definitions_ask;
        definitions_ask.limit = 1000;
        definitions_ask.filter.emplace();
        definitions_ask.filter->id_one_of = definition_ids;
        definitions = forwarding::forward(req, session, definitions_ask).definitions;
    }

    auto ordered = analytics.analytics;
    std::ranges::sort(ordered, [](const auto& left, const auto& right) {
        return left.display_order < right.display_order;
    });

    std::vector<std::string> analytic_rows;
    for (const auto& analytic : ordered) {
        const auto named = analytic_names.find(analytic.analytic_type_code);
        analytic_rows.push_back(
            "{\"code\":" + json::write(analytic.analytic_type_code) +
            ",\"name\":" +
            json::write(named == analytic_names.end() ? analytic.analytic_type_code :
                                                        named->second) +
            ",\"active\":" + (analytic.active == "Y" ? "true" : "false") + "}");
    }

    std::ranges::sort(definitions, [](const auto& left, const auto& right) {
        return left.name < right.name;
    });
    std::vector<std::string> report_rows;
    for (const auto& definition : definitions) {
        const auto party = parties.find(boost::uuids::to_string(definition.party_id));
        report_rows.push_back(
            "{\"id\":" + json::write(boost::uuids::to_string(definition.id)) +
            ",\"name\":" + json::write(definition.name) +
            ",\"partyName\":" +
            json::write(party == parties.end() ? std::string() : party->second) +
            ",\"regulatoryBookType\":" +
            (definition.regulatory_book_type ? json::write(*definition.regulatory_book_type) :
                                               std::string("null")) +
            "}");
    }

    const auto type = type_names.find(configuration.configuration_type_code);
    const auto party = parties.find(boost::uuids::to_string(configuration.party_id));

    std::ostringstream body;
    body << "{\"configuration\":{\"id\":" << json::write(boost::uuids::to_string(configuration.id))
         << ",\"name\":" << json::write(configuration.name)
         << ",\"typeCode\":" << json::write(configuration.configuration_type_code)
         << ",\"typeName\":"
         << json::write(type == type_names.end() ? configuration.configuration_type_code :
                                                   type->second)
         << ",\"owningComponent\":" << json::write(configuration.owning_component)
         << ",\"partyId\":" << json::write(boost::uuids::to_string(configuration.party_id))
         << ",\"partyName\":"
         << json::write(party == parties.end() ? std::string() : party->second)
         << ",\"version\":" << configuration.version
         << ",\"modified_by\":" << json::write(configuration.modified_by)
         << ",\"performed_by\":" << json::write(configuration.performed_by)
         << ",\"change_reason_code\":" << json::write(configuration.change_reason_code)
         << ",\"change_commentary\":" << json::write(configuration.change_commentary)
         << ",\"recorded_at\":" << json::write(configuration.recorded_at) << "}"
         << ",\"analytics\":" << rr::array_of(analytic_rows)
         << ",\"reports\":" << rr::array_of(report_rows) << '}';
    return body.str();
}

}

}

using namespace configurations;

void configuration_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/configurations/{id}")
            .summary("One configuration with what its keys point at")
            .description("The configuration, the name of its type and of its party, the "
                         "analytics it runs and the reports that bind it, so the page shows "
                         "names rather than ids.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_configurations_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("A configuration is named by its id.");
                    return http_response::json(
                        detail_of(req, session, boost::uuids::string_generator()(raw)));
                });
            }));
}

}
