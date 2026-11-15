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
#ifndef ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_ROWS_HPP
#define ORES_HTTP_ROUTES_REPORTING_PRICING_ENGINE_ROWS_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

/**
 * @brief One product of a pricing engine document.
 */
struct pricing_product_row final {
    std::string id;
    int version = 0;
    std::string type_code;
    std::string model;
    std::string engine;
    int position = 0;
};

/**
 * @brief One parameter of a pricing engine document.
 */
struct pricing_parameter_row final {
    std::string id;
    int version = 0;
    std::optional<std::string> product_id;
    std::string scope;
    std::string name;
    std::string value;
    int position = 0;
};

/**
 * @brief A pricing engine document: the header, its products and its parameters.
 *
 * The analytics service keeps a configuration as three tables. The screens never
 * see the split, so a read here answers the whole document.
 */
struct pricing_document_row final {
    std::string id;
    std::string party_id;
    int version = 0;
    std::string name;
    std::string description;
    std::string variant;
    std::string configuration_id;
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
    std::string change_reason;
    std::string change_commentary;
    std::vector<pricing_product_row> products;
    std::vector<pricing_parameter_row> parameters;
};

/**
 * @brief The pricing engine documents a party holds, read from the analytics
 * service as the screens state them.
 */
namespace pricing_engine_rows {

/// The reporting configuration type a report binds a pricing engine through.
constexpr std::string_view configuration_type = "pricing_engines";

/**
 * @brief Every configuration the caller's parties hold, in the service's order.
 */
[[nodiscard]] std::vector<ores::analytics::domain::pricing_model_config>
read_configs(const ores::http::domain::http_request& req,
             ores::nats::service::nats_client& session);

/**
 * @brief The products of one configuration, in the order the data places them.
 */
[[nodiscard]] std::vector<pricing_product_row>
read_products(const ores::http::domain::http_request& req,
              ores::nats::service::nats_client& session,
              const boost::uuids::uuid& configuration_id);

/**
 * @brief The parameters of one configuration, in the order the data places them.
 */
[[nodiscard]] std::vector<pricing_parameter_row>
read_parameters(const ores::http::domain::http_request& req,
                ores::nats::service::nats_client& session,
                const boost::uuids::uuid& configuration_id);

/**
 * @brief How many products and parameters each configuration holds, by its id.
 */
struct document_counts final {
    std::map<std::string, std::size_t> products;
    std::map<std::string, std::size_t> parameters;
};

/**
 * @brief The counts the page states, read once for every configuration.
 */
[[nodiscard]] document_counts read_counts(const ores::http::domain::http_request& req,
                                          ores::nats::service::nats_client& session);

/**
 * @brief One configuration with its products and parameters, or a 404 when the
 * service holds none with this id.
 */
[[nodiscard]] pricing_document_row read_document(const ores::http::domain::http_request& req,
                                                 ores::nats::service::nats_client& session,
                                                 const boost::uuids::uuid& id);

}

}

#endif
