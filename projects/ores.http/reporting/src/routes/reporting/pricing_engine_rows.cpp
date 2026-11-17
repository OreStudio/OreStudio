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
#include "ores.http/routes/reporting/pricing_engine_rows.hpp"
#include "ores.analytics.api/messaging/pricing_model_config_protocol.hpp"
#include "ores.analytics.api/messaging/pricing_model_product_parameter_protocol.hpp"
#include "ores.analytics.api/messaging/pricing_model_product_protocol.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/json_shape.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace forwarding = ores::http::forwarding;
namespace analytics_messaging = ores::analytics::messaging;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
using ores::http::net::shape::member;
using ores::http::net::shape::object;

auto& pricing_rows_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.pricing_engine_rows");
    return instance;
}

}

namespace pricing_engine_rows {

std::vector<ores::analytics::domain::pricing_model_config> read_configs(const http_request& req,
                                                                        nats_client& session) {
    return forwarding::forward_all(
        req,
        session,
        analytics_messaging::list_pricing_model_configs_request{},
        &analytics_messaging::list_pricing_model_configs_response::configs);
}

std::vector<pricing_product_row> read_products(const http_request& req,
                                               nats_client& session,
                                               const boost::uuids::uuid& configuration_id) {
    // The product table states no filter by configuration, so the rows are read
    // and the document's own are kept.
    const auto all = forwarding::forward_all(
        req,
        session,
        analytics_messaging::list_pricing_model_products_request{},
        &analytics_messaging::list_pricing_model_products_response::products);
    std::vector<pricing_product_row> products;
    for (const auto& row : all) {
        if (row.pricing_model_config_id != configuration_id)
            continue;
        products.push_back({boost::uuids::to_string(row.id),
                            row.version,
                            row.pricing_engine_type_code,
                            row.model,
                            row.engine,
                            row.position});
    }
    std::ranges::stable_sort(products, {}, &pricing_product_row::position);
    return products;
}

std::vector<pricing_parameter_row> read_parameters(const http_request& req,
                                                   nats_client& session,
                                                   const boost::uuids::uuid& configuration_id) {
    const auto all = forwarding::forward_all(
        req,
        session,
        analytics_messaging::list_pricing_model_product_parameters_request{},
        &analytics_messaging::list_pricing_model_product_parameters_response::parameters);
    std::vector<pricing_parameter_row> parameters;
    for (const auto& row : all) {
        if (row.pricing_model_config_id != configuration_id)
            continue;
        parameters.push_back({boost::uuids::to_string(row.id),
                              row.version,
                              row.pricing_model_product_id ?
                                  std::optional<std::string>(
                                      boost::uuids::to_string(*row.pricing_model_product_id)) :
                                  std::nullopt,
                              row.parameter_scope,
                              row.parameter_name,
                              row.parameter_value,
                              row.position});
    }
    std::ranges::stable_sort(parameters, {}, &pricing_parameter_row::position);
    return parameters;
}

pricing_document_row
read_document(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    analytics_messaging::list_pricing_model_configs_request ask;
    ask.limit = 1;
    ask.filter.emplace();
    ask.filter->id_one_of = std::vector<boost::uuids::uuid>{id};
    const auto found = forwarding::forward(req, session, ask);
    rr::require_ok(found.result);
    if (found.configs.empty())
        rr::refuse(
            http_status::not_found, "not-found", "No pricing engine configuration has this id.");

    const auto& config = found.configs.front();
    pricing_document_row document;
    document.id = boost::uuids::to_string(config.id);
    document.party_id = boost::uuids::to_string(config.party_id);
    document.version = config.version;
    document.name = config.name;
    document.description = config.description;
    document.variant = config.config_variant;
    document.configuration_id = boost::uuids::to_string(config.configuration_id);
    document.modified_by = config.modified_by;
    document.recorded_at = config.recorded_at;
    document.change_reason = config.change_reason_code;
    document.change_commentary = config.change_commentary;
    document.products = read_products(req, session, id);
    document.parameters = read_parameters(req, session, id);
    return document;
}

std::string bound_reports_json_of(const http_request& req,
                                  nats_client& session,
                                  const std::string& configuration_id) {
    std::vector<engine_bound_report> reports;
    try {
        reports = engine_settings_rows::read_bound_reports(
            req, session, configuration_type, boost::uuids::string_generator()(configuration_id));
    } catch (const std::exception&) {
        // A binding that cannot be read must not hide the document.
        BOOST_LOG_SEV(pricing_rows_lg(), ores::logging::debug)
            << "The reports that bind configuration " << configuration_id
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

document_counts read_counts(const http_request& req, nats_client& session) {
    document_counts counts;
    for (const auto& row : forwarding::forward_all(
             req,
             session,
             analytics_messaging::list_pricing_model_products_request{},
             &analytics_messaging::list_pricing_model_products_response::products))
        ++counts.products[boost::uuids::to_string(row.pricing_model_config_id)];
    for (const auto& row : forwarding::forward_all(
             req,
             session,
             analytics_messaging::list_pricing_model_product_parameters_request{},
             &analytics_messaging::list_pricing_model_product_parameters_response::parameters))
        ++counts.parameters[boost::uuids::to_string(row.pricing_model_config_id)];
    return counts;
}

}

}
