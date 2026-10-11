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
#ifndef ORES_SYNTHETIC_API_FEEDS_FX_MIXTURE_CONVERSION_HPP
#define ORES_SYNTHETIC_API_FEEDS_FX_MIXTURE_CONVERSION_HPP

#include "ores.synthetic.api/export.hpp"
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief An FX feed's mixture as three parallel vectors, one entry per
 * component, in the order the engine reads them.
 */
struct fx_mixture {
    std::vector<double> means;
    std::vector<double> stdevs;
    std::vector<double> weights;
};

/**
 * @brief The half-life, in minutes, a mixture switched to the
 * Ornstein-Uhlenbeck engine starts from.
 */
inline constexpr double default_half_life_minutes = 15.0;

/**
 * @brief The reversion speed κ that halves the distance to the long-run level
 * in @p minutes at @p ticks_per_hour: ln(2) over the half-life in updates.
 *
 * Throws std::invalid_argument for a duration or a rate that is not positive.
 */
ORES_SYNTHETIC_API_EXPORT double kappa_for_half_life(double minutes, int ticks_per_hour);

/**
 * @brief The mixture an engine change leaves, read the way the new engine
 * reads it.
 *
 * The geometric engine reads μ and σ as log-returns. The arithmetic engine
 * reads both in price units. The Ornstein-Uhlenbeck engine reads only the
 * first component: its weight as κ and its σ in price units, with μ unused.
 * So a change between the geometric engine and another scales by
 * @p initial_price; a change to Ornstein-Uhlenbeck keeps the first component,
 * with κ from the default half-life at @p ticks_per_hour and μ of zero; and a
 * change from it gives that one component the whole weight. A mixture whose
 * engine does not change comes back as it went.
 *
 * Throws std::invalid_argument for an unknown engine, vectors of different
 * sizes, an initial price that is not positive, or a rate that is not
 * positive.
 */
ORES_SYNTHETIC_API_EXPORT fx_mixture convert_fx_mixture(const fx_mixture& mixture,
                                                        const std::string& from_process_type,
                                                        const std::string& to_process_type,
                                                        double initial_price,
                                                        int ticks_per_hour);

}

#endif
