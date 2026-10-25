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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_ARRIVAL_SPEC_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_ARRIVAL_SPEC_HPP

#include <string>

namespace ores::analytics::quant::domain {

/**
 * @brief The four numbers a caller sets to choose an arrival regime.
 *
 * They travel together: a generation config row holds exactly these, a feed
 * hands them to the factory unchanged, and an editor fills them in. Passing
 * them as one value keeps the factory call and every call site from growing a
 * fourth positional argument, and gives the regime layer one place to add the
 * market sensitivity beside.
 *
 * The names match the config columns, so a reader can follow the value from the
 * database to the process without a translation table.
 */
struct arrival_spec final {
    /// "periodic", "poisson" or "hawkes".
    std::string mode = "periodic";
    /// The long-run mean rate, in ticks per hour, the unit the config uses.
    double ticks_per_hour = 0.0;
    /// For "hawkes": the expected number of further ticks per tick, below one.
    double branching_ratio = 0.0;
    /// For "hawkes": how fast a burst fades, in units of one over a second.
    double decay = 1.0;
};

}

#endif
