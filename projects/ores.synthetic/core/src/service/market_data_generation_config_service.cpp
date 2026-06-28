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
#include "ores.synthetic.core/service/market_data_generation_config_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

market_data_generation_config_service::market_data_generation_config_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_{} {}

std::vector<domain::market_data_generation_config>
market_data_generation_config_service::list_configs(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing configs with offset=" << offset << " limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t market_data_generation_config_service::count_configs() {
    BOOST_LOG_SEV(lg(), debug) << "Counting configs";
    return repo_.get_total_config_count(ctx_);
}

void market_data_generation_config_service::save_config(
    const domain::market_data_generation_config& config) {
    if (config.name.empty()) {
        throw std::invalid_argument("Market data generation config name cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving config: " << config.name;
    auto c = config;
    stamp(c, ctx_);
    repo_.write(ctx_, c);
}

void market_data_generation_config_service::save_configs(
    const std::vector<domain::market_data_generation_config>& configs) {
    for (const auto& c : configs) {
        if (c.name.empty())
            throw std::invalid_argument("Market data generation config name cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << configs.size() << " configs";
    auto stamped = configs;
    for (auto& c : stamped)
        stamp(c, ctx_);
    repo_.write(ctx_, stamped);
}

void market_data_generation_config_service::delete_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Deleting config: " << id;
    repo_.remove(ctx_, id);
}

void market_data_generation_config_service::delete_configs(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::optional<domain::market_data_generation_config>
market_data_generation_config_service::get_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting config: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::vector<domain::market_data_generation_config>
market_data_generation_config_service::get_config_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting config history for: " << id;
    return repo_.read_all(ctx_, id);
}

}
