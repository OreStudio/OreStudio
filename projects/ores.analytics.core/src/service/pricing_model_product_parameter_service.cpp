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
#include "ores.analytics.core/service/pricing_model_product_parameter_service.hpp"

#include <stdexcept>

namespace ores::analytics::service {

using namespace ores::logging;

pricing_model_product_parameter_service::
pricing_model_product_parameter_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::list_parameters(
    const std::string& config_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing pricing model product parameters for config: " << config_id;
    return repo_.read_latest(ctx_, config_id);
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::list_parameters_for_product(
    const std::string& product_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing pricing model product parameters for product: "
        << product_id;
    return repo_.read_latest_for_product(ctx_, product_id);
}

std::optional<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::find_parameter(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Finding pricing model product parameter: " << id;
    auto results = repo_.read_latest_by_id(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void pricing_model_product_parameter_service::save_parameter(
    const domain::pricing_model_product_parameter& v) {
    if (v.id.is_nil())
        throw std::invalid_argument(
            "Pricing model product parameter id cannot be nil.");
    BOOST_LOG_SEV(lg(), debug)
        << "Saving pricing model product parameter: " << v.id;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info)
        << "Saved pricing model product parameter: " << v.id;
}

void pricing_model_product_parameter_service::save_parameters(
    const std::vector<domain::pricing_model_product_parameter>& v) {
    for (const auto& e : v) {
        if (e.id.is_nil())
            throw std::invalid_argument(
                "Pricing model product parameter id cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << v.size()
        << " pricing model product parameters";
    repo_.write(ctx_, v);
}

void pricing_model_product_parameter_service::remove_parameter(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing pricing model product parameter: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info)
        << "Removed pricing model product parameter: " << id;
}

void pricing_model_product_parameter_service::remove_parameters_for_config(
    const std::string& config_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing pricing model product parameters for config: " << config_id;
    repo_.remove_for_config(ctx_, config_id);
    BOOST_LOG_SEV(lg(), info)
        << "Removed pricing model product parameters for config: " << config_id;
}

void pricing_model_product_parameter_service::remove_parameters_for_product(
    const std::string& product_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing pricing model product parameters for product: "
        << product_id;
    repo_.remove_for_product(ctx_, product_id);
    BOOST_LOG_SEV(lg(), info)
        << "Removed pricing model product parameters for product: "
        << product_id;
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::get_parameter_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting history for pricing model product parameter: " << id;
    return repo_.read_all(ctx_, id);
}

}
