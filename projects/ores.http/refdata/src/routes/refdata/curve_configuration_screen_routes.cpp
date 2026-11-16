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
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/ore_value_set_tables.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/refdata/curve_configuration_view.hpp"
#include "ores.http/routes/refdata/curve_document_cells.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/curve_configuration_protocol.hpp"
#include "ores.refdata.api/messaging/curve_definition_protocol.hpp"
#include "ores.refdata.api/messaging/curve_section_protocol.hpp"
#include "ores.refdata.api/messaging/curve_segment_type_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_protocol.hpp"
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
namespace document = ores::http::routes::refdata::document;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
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

/**
 * @brief The choices one catalogue relation offers, read from the collection
 * that holds it.
 *
 * A relation names a view in another component, so the part that answers a route
 * states which relations it reads. The two the value set data names for a curve
 * configuration are the tenors and the currencies; a row naming another one is a
 * seed error, and it is refused with a sentence rather than drawn as an empty
 * list.
 */
std::vector<ores::http::routes::value_choice>
curve_catalogue_choices(const http_request& req, nats_client& session, std::string_view relation) {
    if (relation == "ores_refdata_tenor_catalogue_v") {
        std::vector<ores::http::routes::value_choice> choices;
        for (const auto& tenor : forwarding::forward_all(
                 req,
                 session,
                 refdata_messaging::list_tenors_request{},
                 &refdata_messaging::list_tenors_response::tenors))
            choices.push_back({tenor.code, tenor.display_name});
        return choices;
    }
    if (relation == "ores_refdata_currency_catalogue_v") {
        std::vector<ores::http::routes::value_choice> choices;
        for (const auto& currency : forwarding::forward_all(
                 req,
                 session,
                 refdata_messaging::list_currencies_request{},
                 &refdata_messaging::list_currencies_response::currencies))
            choices.push_back({currency.iso_code, currency.name});
        return choices;
    }
    throw ores::http::routes::value_set_error(
        "The value set tables name the catalogue relation \"" + std::string(relation) +
        "\", which this build does not read.");
}

/// The schema the curve configuration's leaves are documented under.
constexpr std::string_view curve_schema = "curveconfig.xsd";

/// The identifier of a curve configuration the address names.
std::string configuration_id_of(const http_request& req) {
    const auto id = req.get_path_param("id");
    if (!rr::is_uuid(id))
        rr::invalid("A curve configuration is named by its identifier.");
    return id;
}

/// The party the address names, or nothing when it names none.
std::string party_of(const http_request& req) {
    const auto party = req.get_query_param("party");
    if (!party.empty() && !rr::is_uuid(party))
        rr::invalid("A party is named by its identifier.");
    return party;
}

/**
 * @brief Points the contract at the rows of the curve configuration schema.
 *
 * The rows outlive the contract in the caller, which owns them for the length of
 * one answer.
 */
void fill_contract(const std::vector<ores::reporting::domain::ore_parameter>& parameters,
                   curve_view::contract& contract) {
    for (const auto& parameter : parameters)
        if (parameter.schema_name == curve_schema)
            contract.emplace(parameter.xpath, &parameter);
}

/// The ORE element each segment type code is written as.
std::map<std::string, std::string> segment_kinds_of(const http_request& req, nats_client& session) {
    std::map<std::string, std::string> kinds;
    for (const auto& type :
         forwarding::forward_all(req,
                                 session,
                                 refdata_messaging::list_curve_segment_types_request{},
                                 &refdata_messaging::list_curve_segment_types_response::segment_types))
        kinds.emplace(type.code, type.segment_kind);
    return kinds;
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

    add(router->get("/api/refdata/curve-configurations/{id}")
            .summary("One curve configuration, as its screen reads it")
            .description("The configuration a reporting configuration is filled by, drawn as the "
                         "sections the schema declares, each with its entries, the leaves they "
                         "hold, the words the ORE parameter contract gives each leaf and the "
                         "values the value sets accept. A configuration no document fills is a "
                         "404, so the page states the missing case rather than drawing an empty "
                         "screen.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<refdata_messaging::get_curve_configuration_document_request>([&] {
                    rr::refuse_unknown_params(req, {"party"});
                    refdata_messaging::get_curve_configuration_document_request msg;
                    msg.configuration_id = configuration_id_of(req);
                    msg.party_id = party_of(req);
                    const auto answer = forwarding::forward(req, session, msg);
                    if (!answer.success)
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   answer.message.empty()
                                       ? "No curve configuration document fills the configuration "
                                             "the address names."
                                       : answer.message);

                    const auto sections = forwarding::forward_all(
                        req,
                        session,
                        refdata_messaging::list_curve_sections_request{},
                        &refdata_messaging::list_curve_sections_response::sections);
                    const auto parameters = forwarding::forward_all(
                        req,
                        session,
                        ores::reporting::messaging::list_ore_parameters_request{},
                        &ores::reporting::messaging::list_ore_parameters_response::parameters);
                    curve_view::contract contract;
                    fill_contract(parameters, contract);
                    const auto kinds = segment_kinds_of(req, session);
                    const auto values = value_set_reader(
                        read_value_set_tables(req, session), [&req, &session](
                                                                 std::string_view relation) {
                            return curve_catalogue_choices(req, session, relation);
                        });
                    const auto flattened = document::cells(answer.document);
                    return http_response::json(curve_view::view_json_of(
                        flattened, msg.configuration_id, sections, {contract, values, kinds}));
                });
            }));
}

}
