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
#include "ores.synthetic.core/service/fx_spot_generation_config_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

fx_spot_generation_config_service::fx_spot_generation_config_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::fx_spot_generation_config>
fx_spot_generation_config_service::list_fx_spot_generation_configs(std::uint32_t offset,
                                                                   std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all FX spot generation configs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t fx_spot_generation_config_service::count_fx_spot_generation_configs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total FX spot generation configs count";
    return repo_.get_total_fx_spot_generation_config_count(ctx_);
}

std::optional<domain::fx_spot_generation_config>
fx_spot_generation_config_service::get_fx_spot_generation_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting FX spot generation config: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void fx_spot_generation_config_service::save_fx_spot_generation_config(
    const domain::fx_spot_generation_config& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("FX Spot Generation Config id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving FX spot generation config: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved FX spot generation config: " << v.id;
}

void fx_spot_generation_config_service::save_fx_spot_generation_configs(
    const std::vector<domain::fx_spot_generation_config>& fx_spot_generation_configs) {
    for (const auto& e : fx_spot_generation_configs)
        if (e.id.is_nil())
            throw std::invalid_argument("FX Spot Generation Config id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << fx_spot_generation_configs.size()
                               << " FX spot generation configs";
    auto ts = fx_spot_generation_configs;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void fx_spot_generation_config_service::delete_fx_spot_generation_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing FX spot generation config: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed FX spot generation config: " << id;
}

void fx_spot_generation_config_service::delete_fx_spot_generation_configs(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::fx_spot_generation_config>
fx_spot_generation_config_service::get_fx_spot_generation_config_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for FX spot generation config: " << id;
    return repo_.read_all(ctx_, id);
}

}
