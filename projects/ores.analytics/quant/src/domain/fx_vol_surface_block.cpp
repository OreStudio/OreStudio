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
#include "ores.analytics.quant/domain/fx_vol_surface_block.hpp"
#include "ores.analytics.quant/math/fx_vol_surface_correlation.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include <format>

namespace ores::analytics::quant::domain {

std::uint64_t fx_vol_surface_driver_id(std::string_view name, std::size_t index) {
    return math::series_key(std::format("{}/{}", name, index));
}

std::expected<block_spec, std::string> fx_vol_surface_block(const std::string& name,
                                                            std::uint64_t spot,
                                                            std::size_t segments,
                                                            double spot_variance,
                                                            double spot_skew,
                                                            double neighbour) {
    if (name.empty())
        return std::unexpected(std::string("A surface's block needs the feed's name."));

    const auto correlation =
        math::fx_vol_surface_correlation(segments, spot_variance, spot_skew, neighbour);
    if (!correlation)
        return std::unexpected(correlation.error());

    block_spec block{};
    block.name = name;
    block.anchor = spot;
    block.size = segments + 3;
    block.members.reserve(fx_vol_surface_driver_count(segments));
    for (std::size_t i = 0; i < fx_vol_surface_driver_count(segments); ++i)
        block.members.push_back(fx_vol_surface_driver_id(name, i));

    block.correlation.resize(block.size * block.size);
    for (std::size_t i = 0; i < block.size; ++i)
        for (std::size_t j = 0; j < block.size; ++j)
            block.correlation[i * block.size + j] =
                (*correlation)(static_cast<Eigen::Index>(i), static_cast<Eigen::Index>(j));
    return block;
}

}
