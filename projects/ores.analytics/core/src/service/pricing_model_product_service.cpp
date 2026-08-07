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
#include "ores.analytics.core/service/pricing_model_product_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::analytics::service {

using namespace ores::logging;

pricing_model_product_service::pricing_model_product_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::pricing_model_product>
pricing_model_product_service::list_products(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all pricing model products";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t pricing_model_product_service::count_products() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total pricing model products count";
    return repo_.get_total_product_count(ctx_);
}


std::optional<domain::pricing_model_product>
pricing_model_product_service::get_product_at_version(const std::string& id,
                                                      std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting pricing model product at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::pricing_model_product>
pricing_model_product_service::get_product(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting pricing model product. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::pricing_model_product>
pricing_model_product_service::find_product(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding pricing model product. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, boost::uuids::to_string(id));
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void pricing_model_product_service::save_product(const domain::pricing_model_product& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Pricing Model Product id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving pricing model product. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved pricing model product. " << "id: " << v.id;
}

void pricing_model_product_service::save_products(
    const std::vector<domain::pricing_model_product>& products) {
    for (const auto& e : products)
        if (e.id.is_nil())
            throw std::invalid_argument("Pricing Model Product id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << products.size() << " pricing model products";
    auto ts = products;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void pricing_model_product_service::delete_product(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing pricing model product. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed pricing model product. " << "id: " << id;
}

void pricing_model_product_service::remove_product(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing pricing model product. " << "id: " << id;
    repo_.remove(ctx_, boost::uuids::to_string(id));
    BOOST_LOG_SEV(lg(), info) << "Removed pricing model product. " << "id: " << id;
}

void pricing_model_product_service::delete_products(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::pricing_model_product>
pricing_model_product_service::get_product_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for pricing model product. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

std::vector<domain::pricing_model_product>
pricing_model_product_service::get_product_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for pricing model product. " << "id: " << id;
    return repo_.read_all(ctx_, boost::uuids::to_string(id));
}

}
