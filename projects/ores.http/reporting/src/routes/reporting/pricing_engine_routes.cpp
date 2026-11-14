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
#include "ores.http/routes/reporting/pricing_engine_routes.hpp"
#include "ores.analytics.api/messaging/pricing_model_config_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.http/routes/reporting/json_shape.hpp"
#include "ores.http/routes/reporting/pricing_engine_rows.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using shape::member;
using shape::object;
using ores::nats::service::nats_client;

auto& pricing_engine_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.pricing_engine");
    return instance;
}

/// The party a configuration belongs to, and the names of its parties.
std::vector<std::string> party_members(std::string_view party_id,
                                       const std::map<std::string, engine_party_name>& parties) {
    const auto party = parties.find(std::string(party_id));
    return {member("partyId", json::write(std::string(party_id))),
            member("partyName",
                   json::write(party == parties.end() ? std::string() : party->second.name)),
            member("partyCode",
                   json::write(party == parties.end() ? std::string() : party->second.short_code))};
}

/// One configuration as the page of engines states it.
std::string summary_json_of(const ores::analytics::domain::pricing_model_config& row,
                            const pricing_engine_rows::document_counts& counts,
                            const std::map<std::string, engine_party_name>& parties) {
    const auto id = boost::uuids::to_string(row.id);
    const auto products = counts.products.find(id);
    const auto parameters = counts.parameters.find(id);
    auto members = party_members(boost::uuids::to_string(row.party_id), parties);
    members.push_back(member("id", json::write(id)));
    members.push_back(member("version", std::to_string(row.version)));
    members.push_back(member("name", json::write(row.name)));
    members.push_back(member("description", json::write(row.description)));
    members.push_back(member("variant", json::write(row.config_variant)));
    members.push_back(
        member("configurationId", json::write(boost::uuids::to_string(row.configuration_id))));
    members.push_back(member(
        "products", std::to_string(products == counts.products.end() ? 0 : products->second)));
    members.push_back(
        member("parameters",
               std::to_string(parameters == counts.parameters.end() ? 0 : parameters->second)));
    members.push_back(member("modifiedBy", json::write(row.modified_by)));
    members.push_back(member("recordedAt", json::write(row.recorded_at)));
    return object(members);
}

/// One product of a document.
std::string product_json_of(const pricing_product_row& row) {
    return object({member("id", json::write(row.id)),
                   member("version", std::to_string(row.version)),
                   member("typeCode", json::write(row.type_code)),
                   member("model", json::write(row.model)),
                   member("engine", json::write(row.engine)),
                   member("position", std::to_string(row.position))});
}

/// One parameter of a document.
std::string parameter_json_of(const pricing_parameter_row& row) {
    return object(
        {member("id", json::write(row.id)),
         member("version", std::to_string(row.version)),
         member("productId", row.product_id ? json::write(*row.product_id) : std::string("null")),
         member("scope", json::write(row.scope)),
         member("name", json::write(row.name)),
         member("value", json::write(row.value)),
         member("position", std::to_string(row.position))});
}

/// The reports that bind one configuration, or null when they cannot be read.
std::string reports_json_of(const http_request& req,
                            nats_client& session,
                            const boost::uuids::uuid& configuration_id) {
    std::vector<engine_bound_report> reports;
    try {
        reports = engine_settings_rows::read_bound_reports(
            req, session, pricing_engine_rows::configuration_type, configuration_id);
    } catch (const std::exception&) {
        // A binding that cannot be read must not hide the document.
        BOOST_LOG_SEV(pricing_engine_lg(), ores::logging::debug)
            << "The reports that bind configuration " << boost::uuids::to_string(configuration_id)
            << " could not be read, so the page states them as unknown.";
        return "null";
    }
    std::vector<std::string> rows;
    rows.reserve(reports.size());
    for (const auto& report : reports)
        rows.push_back(object(
            {member("id", json::write(report.id)), member("name", json::write(report.name))}));
    return rr::array_of(rows);
}

/// One configuration with its products, its parameters and the reports that bind it.
std::string document_json_of(const pricing_document_row& row,
                             const std::map<std::string, engine_party_name>& parties,
                             const std::string& reports) {
    auto members = party_members(row.party_id, parties);
    members.push_back(member("id", json::write(row.id)));
    members.push_back(member("version", std::to_string(row.version)));
    members.push_back(member("name", json::write(row.name)));
    members.push_back(member("description", json::write(row.description)));
    members.push_back(member("variant", json::write(row.variant)));
    members.push_back(member("configurationId", json::write(row.configuration_id)));
    members.push_back(member("modifiedBy", json::write(row.modified_by)));
    members.push_back(member("recordedAt", json::write(row.recorded_at)));
    members.push_back(member("changeReason", json::write(row.change_reason)));
    members.push_back(member("changeCommentary", json::write(row.change_commentary)));
    std::vector<std::string> products;
    products.reserve(row.products.size());
    for (const auto& product : row.products)
        products.push_back(product_json_of(product));
    members.push_back(member("products", rr::array_of(products)));
    std::vector<std::string> parameters;
    parameters.reserve(row.parameters.size());
    for (const auto& parameter : row.parameters)
        parameters.push_back(parameter_json_of(parameter));
    members.push_back(member("parameters", rr::array_of(parameters)));
    members.push_back(member("reports", reports));
    return object(members);
}

}

void pricing_engine_routes::register_read_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/pricing-engines")
            .summary("The pricing engine configurations a party holds")
            .description("One row per configuration: the party that holds it, the version, the "
                         "variant, and how many products and parameters its document holds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ores::analytics::messaging::list_pricing_model_configs_request>(
                    [&] {
                        rr::refuse_unknown_params(req, {});
                        const auto configs = pricing_engine_rows::read_configs(req, session);
                        const auto counts = pricing_engine_rows::read_counts(req, session);
                        const auto parties = engine_settings_rows::read_party_names(req, session);
                        std::vector<std::string> rows;
                        rows.reserve(configs.size());
                        for (const auto& config : configs)
                            rows.push_back(summary_json_of(config, counts, parties));
                        return http_response::json(object({member("rows", rr::array_of(rows))}));
                    });
            }));

    add(router->get("/api/reporting/pricing-engines/{id}")
            .summary("One pricing engine document and the reports that bind it")
            .description("The configuration, its products and its parameters as one document, "
                         "and the reports that bind it through its reporting configuration.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<ores::analytics::messaging::list_pricing_model_configs_request>(
                    [&] {
                        rr::refuse_unknown_params(req, {});
                        const auto raw = req.get_path_param("id");
                        if (!rr::is_uuid(raw))
                            rr::invalid("A pricing engine is named by its id.");
                        const auto id = boost::uuids::string_generator()(raw);
                        const auto document = pricing_engine_rows::read_document(req, session, id);
                        const auto parties = engine_settings_rows::read_party_names(req, session);
                        const auto reports = reports_json_of(
                            req,
                            session,
                            boost::uuids::string_generator()(document.configuration_id));
                        return http_response::json(document_json_of(document, parties, reports));
                    });
            }));
}

}
