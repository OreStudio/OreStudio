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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_LATENT_MARKET_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_LATENT_MARKET_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/keyed_brownian_path.hpp"
#include <cstdint>
#include <span>
#include <vector>

namespace ores::analytics::quant::domain {

/// One factor's weight on one series' driver.
struct factor_weight {
    std::uint32_t factor;
    double weight;
};

/// What a series' tick reads from the market: the shared part of its driver's
/// increment, and the time that increment covers.
struct driver_increment {
    double shared;
    double elapsed;
};

/**
 * @brief The latent market: shared Brownian factors and the drivers that load on
 * them, held as flat arrays.
 *
 * Every series' driver is a standard Brownian motion built from shared factors
 * and its own motion, as the design note states: the loadings of a series are
 * the weights of its driver on those factors, and the squares of those weights
 * leave the rest to the series' own noise. Two series therefore correlate by
 * the dot product of their loadings, exactly, whatever their tick times are.
 *
 * The data model is the many case, not the few. Factors are a handful and each
 * owns a keyed path and a walk; series are thousands and each owns two doubles,
 * its driver value at its last tick and that tick's time. The loadings live in
 * one flat array, written once while the market is built and read only after, so
 * a tick over many series touches no allocator and walks one contiguous span.
 *
 * A tick reads only the factors its own series loads on, and only the first time
 * in an instant: an event costs a handful of levels, not a walk over every factor
 * in the market. Batching is therefore unnecessary for cost, which is what lets
 * stochastic and self-exciting arrivals interleave freely.
 *
 * The market is owned by the scheduler and touched only on its thread. It is
 * forward only: simulated time never moves backwards, and a series' tick at an
 * earlier time is refused rather than silently re-summed.
 */
struct latent_market {
    std::vector<math::keyed_path_params> factors;
    std::vector<math::keyed_walk> walks;
    /// Each factor's value, and the time it was read at.
    std::vector<double> factor_value;
    std::vector<double> factor_time;

    /// The loadings, series by series, with the span each series owns.
    std::vector<factor_weight> loadings;
    std::vector<std::uint32_t> loading_begin;

    /// Each series' driver value at its last tick, and that tick's time.
    std::vector<double> driver;
    std::vector<double> tick_time;

    /// Each series' price-stream id, the draw it has reached, and the squares of
    /// its loadings, which leave the rest of its variance to its own noise.
    std::vector<std::uint64_t> series_id;
    std::vector<std::uint64_t> own_draw;
    std::vector<double> loading_square;

    /// The simulated time the market has been advanced to.
    double now;
    /// The run's seed, so a market is reproducible by seed.
    std::uint64_t run_seed;
    /// The width of a factor's cells in simulated time.
    double cell_width;
    /// The number of bridge levels inside a cell.
    unsigned depth;
    /// The simulated time the run starts at.
    double start;
};

/**
 * @brief A market with no factors and no series.
 *
 * @param run_seed The seed every factor is keyed by.
 * @param cell_width The width of a factor's cells in simulated time.
 * @param depth The number of bridge levels inside a cell.
 * @param start The simulated time the run starts at.
 */
ORES_ANALYTICS_QUANT_EXPORT latent_market make_latent_market(std::uint64_t run_seed,
                                                             double cell_width,
                                                             unsigned depth,
                                                             double start);

/**
 * @brief Adds a factor, keyed by its own id.
 *
 * The id takes a purpose of its own in the stream, so a factor never collides
 * with a series' price or arrival draws.
 *
 * @return The factor's index.
 */
ORES_ANALYTICS_QUANT_EXPORT std::uint32_t latent_add_factor(latent_market& market,
                                                            std::uint64_t factor_id);

/**
 * @brief Adds a series with no loadings yet.
 *
 * @param series_id The series' price-stream id, from series_key(source name). It
 *        keys the draw a loaded series' own noise comes from.
 * @return The series' index.
 */
ORES_ANALYTICS_QUANT_EXPORT std::uint32_t latent_add_series(latent_market& market,
                                                            std::uint64_t series_id);

/**
 * @brief Sets a series' loadings.
 *
 * Called once, for the series most recently added, so the flat array is built in
 * one pass and never moved again.
 *
 * @throws std::invalid_argument if the series is not the last one added, if a
 *         factor is out of range or repeated, or if the squares of the weights
 *         exceed one, which would leave a driver that is not a standard Brownian
 *         motion.
 */
ORES_ANALYTICS_QUANT_EXPORT void latent_set_loadings(latent_market& market,
                                                     std::uint32_t series,
                                                     std::span<const factor_weight> loadings);

/**
 * @brief Moves simulated time and refreshes every factor's value.
 *
 * A tick does not need this: it reads the factors its series loads on. Call it
 * only when every factor's value is wanted at one instant, for a batch or a
 * report.
 *
 * @throws std::invalid_argument if the time is before the market's now.
 */
ORES_ANALYTICS_QUANT_EXPORT void latent_advance(latent_market& market, double t);

/// A series' driver value at now, from the factors' cached values.
ORES_ANALYTICS_QUANT_EXPORT double latent_driver_value(const latent_market& market,
                                                       std::uint32_t series);

/**
 * @brief Ticks a series at a simulated time.
 *
 * Returns the shared part of the driver's increment over the interval since the
 * series' last tick, and records the new driver value and time. A first tick
 * measures from the run's start. Only the factors the series loads on are read,
 * and a factor already read at this instant is reused, so an event costs its own
 * series' levels and nothing else.
 *
 * The scheduler clamps a late tick to a clock that may not have moved since the
 * series last ticked, so the elapsed time can be zero. That is a normal outcome
 * of a loop catching up: the increment is then zero and the caller skips the
 * tick, because a normal needs a positive interval.
 *
 * A process turns what this returns into a standard normal by adding its own
 * motion, weighted by the square root of one less the squared loadings, and
 * dividing by the square root of the elapsed time.
 *
 * @throws std::invalid_argument if the time is before the market's now.
 */
ORES_ANALYTICS_QUANT_EXPORT driver_increment latent_tick(latent_market& market,
                                                         std::uint32_t series,
                                                         double t);

/**
 * @brief The standard normal a loaded series' step is built on.
 *
 * Ticks the series, draws its own noise from its price stream at the next
 * position, and combines the two:
 * (shared + sqrt(1 - |b|^2) * own) / sqrt(elapsed). A series with every loading
 * zero is handed its drawn normal unchanged, because multiplying by one and
 * dividing by one is not bit for bit the same number. A series with no elapsed
 * time carries no noise and answers zero, so the caller skips the tick.
 *
 * A feed does not call this for a series with no loadings: it calls the process'
 * own next(), which draws from the same stream, and that is what keeps today's
 * path bit for bit.
 */
ORES_ANALYTICS_QUANT_EXPORT double
latent_driver(latent_market& market, std::uint32_t series, double t);

}

#endif
