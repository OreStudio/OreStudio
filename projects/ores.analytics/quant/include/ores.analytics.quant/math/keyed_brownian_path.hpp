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
#ifndef ORES_ANALYTICS_QUANT_MATH_KEYED_BROWNIAN_PATH_HPP
#define ORES_ANALYTICS_QUANT_MATH_KEYED_BROWNIAN_PATH_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include <array>
#include <cstdint>

namespace ores::analytics::quant::math {

/**
 * @brief The fixed parameters of one factor's keyed Brownian path.
 *
 * A plain aggregate with no heap and a trivial copy, so a market holds a
 * contiguous array of them indexed by factor and never copies one per reader.
 * The level table is precomputed here for the same reason QuantLib's
 * BrownianBridge precomputes its weights and standard deviations: a read is then
 * arithmetic alone, with no square root and no table built per call.
 *
 * The construction is a cell grid with a dyadic Brownian bridge inside each
 * cell, as the design note states (Glasserman, 2003, section 3.1; Karatzas and
 * Shreve, 1991, section 2.3). The seed-only path refined by bisection follows
 * Diffrax's VirtualBrownianTree (Apache-2.0, Patrick Kidger and contributors),
 * the precomputed level weights follow QuantLib's BrownianBridge (BSD-style
 * QuantLib licence), and the draws come from Random123's Philox4x32-10
 * (BSD-3-Clause) through rng_stream.
 */
struct keyed_path_params {
    /// The factor's stream: its id in series, purpose market.
    stream_key key;
    /// The width of one cell in simulated time.
    double cell_width;
    /// The number of bridge levels inside a cell, from 1 to 32.
    unsigned depth;
    /// The simulated time the path starts at, where its value is zero.
    double origin;
    /// The cell the path starts in.
    std::int64_t origin_cell;
    /// The unshifted value at the origin, subtracted from every read.
    double origin_value;
    /// The standard deviation of the bridge draw at each level.
    std::array<double, 33> bridge_sd;
};

/**
 * @brief Builds a factor's parameters, checking them.
 *
 * @param origin The simulated time the path starts at, with value zero.
 * @throws std::invalid_argument if the width, the depth or the origin is out of
 * range.
 */
ORES_ANALYTICS_QUANT_EXPORT keyed_path_params make_keyed_path(stream_key key,
                                                              double cell_width,
                                                              unsigned depth,
                                                              double origin);

/// The cell a simulated time falls in. Throws before the origin.
ORES_ANALYTICS_QUANT_EXPORT std::int64_t keyed_path_cell_of(const keyed_path_params& params,
                                                            double t);

/// The finest step, below which the path is interpolated.
ORES_ANALYTICS_QUANT_EXPORT double keyed_path_finest_step(const keyed_path_params& params);

/// The increment of one cell, the draw its nodes are built from.
ORES_ANALYTICS_QUANT_EXPORT double keyed_path_cell_increment(const keyed_path_params& params,
                                                             std::int64_t cell);

/// The value at the start of a cell, unshifted, summing from the origin cell.
ORES_ANALYTICS_QUANT_EXPORT double keyed_path_boundary_at(const keyed_path_params& params,
                                                          std::int64_t cell);

/**
 * @brief The two finest nodes either side of a node, given a cell's boundary.
 *
 * The expensive read: one keyed draw a level, depth draws in all.
 */
ORES_ANALYTICS_QUANT_EXPORT void keyed_path_span(const keyed_path_params& params,
                                                 std::int64_t cell,
                                                 double boundary,
                                                 std::uint64_t node,
                                                 double& from,
                                                 double& to);

/// The value at a time in a cell, given that cell's boundary, unshifted.
ORES_ANALYTICS_QUANT_EXPORT double keyed_path_value_in_cell(const keyed_path_params& params,
                                                            std::int64_t cell,
                                                            double boundary,
                                                            double t);

/**
 * @brief The value at a time, measured from the origin.
 *
 * Stateless and correct in any order, at the cost of summing every cell between
 * the origin and the time asked for. A tick uses keyed_walk_value instead.
 */
ORES_ANALYTICS_QUANT_EXPORT double keyed_path_value_at(const keyed_path_params& params, double t);

/**
 * @brief The mutable part of one factor, advanced as simulated time moves.
 *
 * Fifty-six bytes and no heap, so a market keeps one of these a factor, not one
 * a series: with one clock a tick every series reads a factor at the same time,
 * and a series carries only its own last driver value. The span is the last
 * finest interval read, so a second read in the same tick is an interpolation
 * with no draws at all.
 *
 * Keeping the last span follows torchsde's BrownianInterval (Apache-2.0, Google
 * Research), whose bounded cache of read intervals serves the same repeated
 * query. A single span is enough here because the design reads at one time a
 * tick; a wider cache would cost every factor state the clock makes
 * unnecessary.
 */
struct keyed_walk {
    std::int64_t cell;
    double boundary;
    std::int64_t span_cell;
    std::uint64_t span_node;
    double span_from;
    double span_to;
    bool span_valid;
};

/// Starts a factor's walk at its origin.
ORES_ANALYTICS_QUANT_EXPORT keyed_walk make_keyed_walk(const keyed_path_params& params);

/**
 * @brief The value at a time, measured from the origin.
 *
 * Advances the walk to the time's cell, one draw a skipped cell, and answers
 * from the cached span when the time falls in it.
 *
 * @throws std::invalid_argument if the time is behind the cell the walk holds,
 * because simulated time moves forwards and a tick never reads a factor in the
 * past.
 */
ORES_ANALYTICS_QUANT_EXPORT double
keyed_walk_value(const keyed_path_params& params, keyed_walk& walk, double t);

}

#endif
