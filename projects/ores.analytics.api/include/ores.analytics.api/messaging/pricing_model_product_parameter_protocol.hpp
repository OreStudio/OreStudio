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
#ifndef ORES_ANALYTICS_MESSAGING_PRICING_MODEL_PRODUCT_PARAMETER_PROTOCOL_HPP
#define ORES_ANALYTICS_MESSAGING_PRICING_MODEL_PRODUCT_PARAMETER_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"

namespace ores::analytics::messaging {

struct get_pricing_model_product_parameters_request {
    using response_type = struct get_pricing_model_product_parameters_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.list";
    std::string config_id;
};

struct get_pricing_model_product_parameters_response {
    std::vector<ores::analytics::domain::pricing_model_product_parameter>
        parameters;
    int total_available_count = 0;
    bool success = true;
    std::string message;
};

struct save_pricing_model_product_parameter_request {
    using response_type = struct save_pricing_model_product_parameter_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.save";
    ores::analytics::domain::pricing_model_product_parameter data;
};

struct save_pricing_model_product_parameter_response {
    bool success = false;
    std::string message;
};

struct delete_pricing_model_product_parameter_request {
    using response_type =
        struct delete_pricing_model_product_parameter_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.delete";
    std::vector<std::string> ids;
};

struct delete_pricing_model_product_parameter_response {
    bool success = false;
    std::string message;
};

struct get_pricing_model_product_parameter_history_request {
    using response_type =
        struct get_pricing_model_product_parameter_history_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.history";
    std::string id;
};

struct get_pricing_model_product_parameter_history_response {
    std::vector<ores::analytics::domain::pricing_model_product_parameter>
        parameters;
    bool success = false;
    std::string message;
};

struct get_pricing_model_product_parameters_for_config_request {
    using response_type =
        struct get_pricing_model_product_parameters_for_config_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.list_for_config";
    std::string config_id;
};

struct get_pricing_model_product_parameters_for_config_response {
    std::vector<ores::analytics::domain::pricing_model_product_parameter>
        parameters;
    bool success = true;
    std::string message;
};

struct get_pricing_model_product_parameters_for_product_request {
    using response_type =
        struct get_pricing_model_product_parameters_for_product_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_model_product_parameters.list_for_product";
    std::string product_id;
};

struct get_pricing_model_product_parameters_for_product_response {
    std::vector<ores::analytics::domain::pricing_model_product_parameter>
        parameters;
    bool success = true;
    std::string message;
};

}

#endif
