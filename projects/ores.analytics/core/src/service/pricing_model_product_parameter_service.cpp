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
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::analytics::service {

using namespace ores::logging;

pricing_model_product_parameter_service::pricing_model_product_parameter_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::list_parameters(std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all pricing model product parameters";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t pricing_model_product_parameter_service::count_parameters() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total pricing model product parameters count";
    return repo_.get_total_parameter_count(ctx_);
}


std::optional<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::get_parameter_at_version(const std::string& id,
                                                                  std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting pricing model product parameter at version. "
                               << "id: " << id << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::get_parameter(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting pricing model product parameter. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::find_parameter(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding pricing model product parameter. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void pricing_model_product_parameter_service::save_parameter(
    const domain::pricing_model_product_parameter& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Pricing Model Product Parameter id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving pricing model product parameter. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved pricing model product parameter. " << "id: " << v.id;
}

void pricing_model_product_parameter_service::save_parameters(
    const std::vector<domain::pricing_model_product_parameter>& parameters) {
    for (const auto& e : parameters)
        if (e.id.is_nil())
            throw std::invalid_argument("Pricing Model Product Parameter id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << parameters.size()
                               << " pricing model product parameters";
    auto ts = parameters;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void pricing_model_product_parameter_service::delete_parameter(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing pricing model product parameter. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed pricing model product parameter. " << "id: " << id;
}

void pricing_model_product_parameter_service::remove_parameter(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing pricing model product parameter. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed pricing model product parameter. " << "id: " << id;
}

void pricing_model_product_parameter_service::delete_parameters(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::get_parameter_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for pricing model product parameter. "
                               << "id: " << id;
    return repo_.read_all(ctx_, id);
}

std::vector<domain::pricing_model_product_parameter>
pricing_model_product_parameter_service::get_parameter_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for pricing model product parameter. "
                               << "id: " << id;
    return repo_.read_all(ctx_, boost::uuids::to_string(id));
}

}
