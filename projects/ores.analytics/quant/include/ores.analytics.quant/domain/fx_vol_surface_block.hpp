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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_FX_VOL_SURFACE_BLOCK_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_FX_VOL_SURFACE_BLOCK_HPP

#include "ores.analytics.quant/domain/latent_market_config.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstddef>
#include <cstdint>
#include <expected>
#include <string>
#include <string_view>

namespace ores::analytics::quant::domain {

/**
 * @brief The block a synthetic volatility surface declares with the spot it quotes.
 *
 * The surface's drivers are the block's members and the spot is its anchor, so the latent
 * market hands each driver a standard normal whose correlation with the spot's is the one the
 * feed states. The members come in the order the surface generator reads its normals: one
 * forward variance for each segment in expiry order, then the skew driver, then the curvature
 * driver.
 *
 * The driver ids are derived from @p name, so a feed that names its surface the same way twice
 * finds the same series, and a market that gains a surface leaves every other series' draws
 * untouched. The depth of the market is not decided here: the caller sets the cell and the
 * fastest gap, because only the run knows its arrival laws.
 *
 * @param name The feed's source name, which names the block and its drivers.
 * @param spot The anchor spot's series id.
 * @param segments How many forward variances the surface has.
 * @param spot_variance The correlation of spot with every forward variance.
 * @param spot_skew The correlation of spot with the skew driver.
 * @param neighbour The correlation of neighbouring forward variances.
 * @return The block, or why the three numbers do not describe a correlation.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::expected<block_spec, std::string>
fx_vol_surface_block(const std::string& name,
                     std::uint64_t spot,
                     std::size_t segments,
                     double spot_variance,
                     double spot_skew,
                     double neighbour);

/**
 * @brief The series id of one of a surface's drivers.
 *
 * @param name The feed's source name, as the block was given it.
 * @param index The driver's place: one for each forward variance in expiry order, then the skew
 *        driver, then the curvature driver.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::uint64_t
fx_vol_surface_driver_id(std::string_view name, std::size_t index);

/**
 * @brief How many drivers a surface with @p segments forward variances has.
 *
 * One for each segment, plus the skew and the curvature.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT constexpr std::size_t
fx_vol_surface_driver_count(std::size_t segments) noexcept {
    return segments + 2;
}

}

#endif
