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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/service/run_market.hpp"
#include "ores.analytics.quant/domain/fx_vol_surface_block.hpp"
#include <algorithm>
#include <stdexcept>
#include <string>

namespace ores::analytics::quant::service {

using domain::block_spec;
using domain::fx_vol_surface_block;
using domain::latent_driver;
using domain::make_latent_market;

run_market::run_market(i_run_clock& clock,
                       const domain::market_config& pace,
                       std::uint64_t run_seed,
                       std::size_t board_capacity)
    : run_seed_(run_seed)
    , loop_(clock, pace, run_seed)
    , values_(board_capacity) {}

void run_market::add_block(std::uint64_t anchor_series,
                           const std::string& name,
                           std::size_t segments,
                           double anchor_variance,
                           double anchor_skew,
                           double neighbour) {
    if (built_)
        throw std::invalid_argument(
            "run_market: a block cannot be added once the market is built, because its "
            "loadings are written in one pass while the market is made");
    if (has_block(name))
        throw std::invalid_argument("run_market: the block '" + name + "' already exists");
    auto block = fx_vol_surface_block(
        name, anchor_series, segments, anchor_variance, anchor_skew, neighbour);
    if (!block)
        throw std::invalid_argument("run_market: the block '" + name +
                                    "' cannot be made: " + block.error());
    config_.blocks.push_back(std::move(*block));
}

bool run_market::has_block(const std::string& name) const {
    return std::ranges::any_of(config_.blocks,
                               [&](const block_spec& block) { return block.name == name; });
}

void run_market::build(double cell_width, double start, double fastest_gap) {
    if (built_)
        throw std::invalid_argument("run_market: the market is already built");
    config_.run_seed = run_seed_;
    config_.cell_width = cell_width;
    config_.start = start;
    config_.fastest_gap = fastest_gap;
    market_ = make_latent_market(config_);

    // Where each feed's drivers are in the market: the anchor for the anchor's own feed,
    // and the members for the block's.
    const auto index_of = [this](std::uint64_t id) -> std::uint32_t {
        const auto known = std::ranges::find(market_.series_id, id);
        if (known == market_.series_id.end())
            throw std::invalid_argument("run_market: the market holds no series for the id " +
                                        std::to_string(id));
        return static_cast<std::uint32_t>(known - market_.series_id.begin());
    };
    for (const block_spec& block : config_.blocks) {
        drivers_[block.anchor] = {index_of(block.anchor)};
        std::vector<std::uint32_t> members;
        members.reserve(block.members.size());
        for (const auto id : block.members)
            members.push_back(index_of(id));
        drivers_[math::series_key(block.name)] = std::move(members);
    }

    built_ = true;
    loop_.set_driver_source(
        [this](std::uint64_t series, double at, std::span<double> out) -> std::size_t {
            return drive(series, at, out);
        });
}

std::size_t run_market::driver_count_of(std::uint64_t series) const {
    const auto known = drivers_.find(series);
    return known == drivers_.end() ? 0 : known->second.size();
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
