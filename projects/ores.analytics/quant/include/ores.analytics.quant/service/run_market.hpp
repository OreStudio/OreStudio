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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_RUN_MARKET_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_RUN_MARKET_HPP

#include "ores.analytics.quant/domain/latent_market.hpp"
#include "ores.analytics.quant/domain/latent_market_config.hpp"
#include "ores.analytics.quant/domain/market_board.hpp"
#include "ores.analytics.quant/domain/operational_clock.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include <cstddef>
#include <cstdint>
#include <map>
#include <span>
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief The market of one run: its loop, its noise and its values.
 *
 * Three collaborators, and this class wires them and adds nothing of its own:
 *
 * - the *loop*, a =tick_scheduler=, decides when a series ticks;
 * - the *noise*, a =latent_market=, decides why two series move together;
 * - the *values*, a =market_board=, holds what the feeds publish for each other.
 *
 * The order matters. The loop knows nothing of the noise or the values: it asks one
 * driver hook, which this object installs on the loop it owns, and that is all. The
 * values know nothing of either. So each collaborator is testable alone, and a feed
 * that reads another feed's output says so by reaching =values()= rather than by
 * borrowing the loop.
 *
 * A run is built in two steps, because a block's loadings are written in one pass:
 * =add_block= may be called until =build()=, and =build()= makes the latent market and
 * installs the driver hook before the loop runs. A feed that starts after the build has
 * no block, and is told so rather than driven by nothing.
 *
 * The market belongs to the loop's thread once it runs: the driver hook is a lambda over
 * this object and it runs inside a tick. =add_block= is the exception and must happen off
 * the loop, before it starts.
 */
class ORES_ANALYTICS_QUANT_EXPORT run_market final {
public:
    /**
     * @param clock the run's time, borrowed and outliving the market.
     * @param pace how the market's pace moves.
     * @param run_seed the seed every stream of the run is keyed by.
     * @param board_capacity how many values the run may ever publish: a curve for each
     *        currency, a spot for each quoted pair, and room to grow.
     * @throws std::invalid_argument for a pace the clock refuses, or no capacity.
     */
    run_market(i_run_clock& clock,
               const domain::market_config& pace,
               std::uint64_t run_seed,
               std::size_t board_capacity = 256);

    run_market(const run_market&) = delete;
    run_market& operator=(const run_market&) = delete;

    /// The loop: when a series ticks. A feed adds its body here.
    [[nodiscard]] tick_scheduler& loop() noexcept {
        return loop_;
    }

    /// The loop, for a reader that only sees its state.
    [[nodiscard]] const tick_scheduler& loop() const noexcept {
        return loop_;
    }

    /// The values: what the feeds publish for each other.
    [[nodiscard]] domain::market_board& values() noexcept {
        return values_;
    }

    [[nodiscard]] const domain::market_board& values() const noexcept {
        return values_;
    }

    /**
     * @brief Records a volatility surface's block.
     *
     * The anchor is the series the surface hangs off -- its pair's spot -- so the
     * surface's forward variances, skew and curvature load on the very series the spot
     * publishes.
     *
     * @param anchor_series The anchor spot's series id.
     * @param name The feed's source name, which names the block and its drivers.
     * @param segments How many forward variances the surface has.
     * @throws std::invalid_argument for three numbers that are not a correlation, a name
     *         already recorded, or a block added after the build.
     */
    void add_block(std::uint64_t anchor_series,
                   const std::string& name,
                   std::size_t segments,
                   double anchor_variance,
                   double anchor_skew,
                   double neighbour);

    /// Whether @p name has a block in the market.
    [[nodiscard]] bool has_block(const std::string& name) const;

    /**
     * @brief Builds the noise and installs the driver hook on the loop.
     *
     * @param cell_width The width of a factor's cells in simulated time.
     * @param start The simulated time the run starts at.
     * @param fastest_gap The shortest gap the run's arrival laws can produce.
     * @throws std::invalid_argument for a cell or a gap the path cannot resolve, or a
     *         second build of one market.
     */
    void build(double cell_width, double start, double fastest_gap);

    /**
     * @brief How many normals @p series reads each tick, or zero when the market drives it none.
     */
    [[nodiscard]] std::size_t driver_count_of(std::uint64_t series) const;

    /// The noise, for a reader that wants to see the run's factors.
    [[nodiscard]] const domain::latent_market& noise() const noexcept {
        return market_;
    }

    [[nodiscard]] bool built() const noexcept {
        return built_;
    }

private:
    /// The normals one feed reads, and how many it wrote.
    std::size_t drive(std::uint64_t series, double at, std::span<double> out);

    std::uint64_t run_seed_;
    domain::latent_config config_;
    domain::latent_market market_;
    tick_scheduler loop_;
    domain::market_board values_;
    /// The market series each feed's series drives, in the feed's own order.
    std::map<std::uint64_t, std::vector<std::uint32_t>> drivers_;
    bool built_ = false;
};

}

#endif
