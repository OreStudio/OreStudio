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
#ifndef ORES_SYNTHETIC_SERVICE_RUN_MARKET_HPP
#define ORES_SYNTHETIC_SERVICE_RUN_MARKET_HPP

#include "ores.analytics.quant/domain/latent_market.hpp"
#include "ores.analytics.quant/domain/latent_market_config.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include <cstddef>
#include <cstdint>
#include <map>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The one latent market a run owns, and where each feed's drivers are in it.
 *
 * A surface declares a block: its spot is the anchor and its forward variances, skew and
 * curvature are the members, so the market hands each of them a standard normal whose
 * correlation with the spot's is the one the feed states. The spot's own driver is the
 * block's first factor, so a surface and its spot move together through one path and not
 * through two copies of it.
 *
 * The market is built once, from every block the run knows, because a block's loadings are
 * written in one pass while the market is built. A feed that starts after the build has no
 * block and is told so, rather than being driven by nothing.
 *
 * The market is owned by the scheduler's thread and touched only there: the driver source
 * the scheduler calls is a lambda over this object, and it runs inside a tick.
 */
class run_market final {
public:
    /**
     * @brief Records a surface's block.
     *
     * @param spot_series The anchor spot's series id, which is also the id of the series the
     *        spot feed registered.
     * @param surface_name The feed's source name, which names the block and its drivers.
     * @param segments How many forward variances the surface has.
     * @throws std::invalid_argument for three numbers that are not a correlation, or a
     *         surface whose name is already recorded.
     */
    void add_surface(std::uint64_t spot_series,
                     const std::string& surface_name,
                     std::size_t segments,
                     double spot_variance,
                     double spot_skew,
                     double neighbour);

    /**
     * @brief Builds the market and installs its driver source on @p scheduler.
     *
     * The scheduler then asks this object for a series' normals on its own thread, so the
     * market must outlive the scheduler. A run with no block builds a market with no series,
     * and every driver is zero, which is what a run without a surface should see.
     *
     * @param run_seed The seed every factor is keyed by.
     * @param cell_width The width of a factor's cells in simulated time.
     * @param start The simulated time the run starts at.
     * @param fastest_gap The shortest gap the run's arrival laws can produce.
     * @throws std::invalid_argument for a cell or a gap the path cannot resolve, or a second
     *         build of one market.
     */
    void build(ores::analytics::quant::service::tick_scheduler& scheduler,
               std::uint64_t run_seed,
               double cell_width,
               double start,
               double fastest_gap);

    /// Whether @p surface_name has a block in the market.
    [[nodiscard]] bool has_surface(const std::string& surface_name) const;

    /**
     * @brief How many normals @p series reads each tick, or zero when the market drives it none.
     *
     * A spot reads one; a surface reads one for each forward variance and one for each of its
     * skew and curvature. A series the market does not know reads none, and every driver it
     * sees is zero.
     */
    [[nodiscard]] std::size_t driver_count_of(std::uint64_t series) const;

    /// The market the run drives, for a reader that wants to see its factors.
    [[nodiscard]] const ores::analytics::quant::domain::latent_market& market() const noexcept {
        return market_;
    }

    [[nodiscard]] bool built() const noexcept {
        return built_;
    }

private:
    /// The normals one feed reads, and how many it wrote: none for a series the market does
    /// not drive.
    std::size_t drive(std::uint64_t series, double at, std::span<double> out);

    ores::analytics::quant::domain::latent_config config_;
    ores::analytics::quant::domain::latent_market market_;
    /// The market series each feed's series drives, in the feed's own order.
    std::map<std::uint64_t, std::vector<std::uint32_t>> drivers_;
    bool built_ = false;
};

}

#endif
