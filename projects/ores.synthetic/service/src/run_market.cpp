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
#include "run_market.hpp"
#include "ores.analytics.quant/domain/fx_vol_surface_block.hpp"
#include <algorithm>
#include <stdexcept>
#include <string>

namespace ores::synthetic::service {

using ores::analytics::quant::domain::block_spec;
using ores::analytics::quant::domain::fx_vol_surface_block;
using ores::analytics::quant::domain::latent_driver;
using ores::analytics::quant::domain::latent_market;
using ores::analytics::quant::domain::make_latent_market;
using ores::analytics::quant::math::series_key;
using ores::analytics::quant::service::tick_scheduler;

void run_market::add_surface(std::uint64_t spot_series,
                             const std::string& surface_name,
                             std::size_t segments,
                             double spot_variance,
                             double spot_skew,
                             double neighbour) {
    if (built_)
        throw std::invalid_argument(
            "run_market: a surface cannot be added once the market is built, because a block's "
            "loadings are written in one pass while the market is made");
    if (has_surface(surface_name))
        throw std::invalid_argument("run_market: the surface '" + surface_name +
                                    "' already has a block");
    auto block = fx_vol_surface_block(
        surface_name, spot_series, segments, spot_variance, spot_skew, neighbour);
    if (!block)
        throw std::invalid_argument("run_market: the surface '" + surface_name +
                                    "' has no block: " + block.error());
    config_.blocks.push_back(std::move(*block));
}

std::size_t run_market::driver_count_of(std::uint64_t series) const {
    const auto known = drivers_.find(series);
    return known == drivers_.end() ? 0 : known->second.size();
}

bool run_market::has_surface(const std::string& surface_name) const {
    return std::ranges::any_of(config_.blocks,
                               [&](const block_spec& block) { return block.name == surface_name; });
}

void run_market::build(tick_scheduler& scheduler,
                       std::uint64_t run_seed,
                       double cell_width,
                       double start,
                       double fastest_gap) {
    if (built_)
        throw std::invalid_argument("run_market: the market is already built");
    config_.run_seed = run_seed;
    config_.cell_width = cell_width;
    config_.start = start;
    config_.fastest_gap = fastest_gap;
    market_ = make_latent_market(config_);

    // Where each feed's drivers are in the market: the anchor for the spot's own feed, and
    // the members for the surface's.
    const auto index_of = [this](std::uint64_t id) -> std::uint32_t {
        const auto known = std::ranges::find(market_.series_id, id);
        if (known == market_.series_id.end())
            throw std::invalid_argument("run_market: the market holds no series for the id " +
                                        std::to_string(id));
        return static_cast<std::uint32_t>(known - market_.series_id.begin());
    };
    for (const block_spec& block : config_.blocks) {
        drivers_[block.anchor] = {index_of(block.anchor)};
        std::vector<std::uint32_t> drivers;
        drivers.reserve(block.members.size());
        for (std::size_t i = 0; i < block.members.size(); ++i)
            drivers.push_back(index_of(block.members[i]));
        drivers_[series_key(block.name)] = std::move(drivers);
    }

    built_ = true;
    scheduler.set_driver_source(
        [this](std::uint64_t series, double at, std::span<double> out) -> std::size_t {
            return drive(series, at, out);
        });
}

std::size_t run_market::drive(std::uint64_t series, double at, std::span<double> out) {
    const auto known = drivers_.find(series);
    if (known == drivers_.end())
        return 0;
    const auto& indices = known->second;
    const auto count = std::min(indices.size(), out.size());
    for (std::size_t i = 0; i < count; ++i)
        out[i] = latent_driver(market_, indices[i], at);
    return count;
}

}
