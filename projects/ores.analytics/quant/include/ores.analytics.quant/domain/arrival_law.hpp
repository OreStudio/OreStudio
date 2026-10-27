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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_ARRIVAL_LAW_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_ARRIVAL_LAW_HPP

#include "ores.analytics.quant/domain/arrival_spec.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include <variant>

namespace ores::analytics::quant::domain {

/**
 * @brief Evenly spaced arrivals, starting part way into the first gap.
 */
struct periodic_law final {
    /// Arrivals per operational second.
    double rate;
    /// The fraction of a gap left before the next arrival: the drawn phase before
    /// the first event, and one after it.
    double offset;
};

/**
 * @brief Arrivals with independent exponential gaps.
 */
struct poisson_law final {
    /// Arrivals per operational second.
    double rate;
};

/**
 * @brief Self-exciting arrivals with an exponential kernel.
 *
 * The intensity is the baseline plus an excess that each event raises by the
 * jump and that fades at the decay. The branching ratio is jump over decay.
 */
struct hawkes_law final {
    /// mu, in arrivals per operational second.
    double baseline;
    /// alpha, the excess one event adds.
    double jump;
    /// beta, in one over an operational second.
    double decay;
    /// The excess at the last event, after its jump.
    double excess;
};

/**
 * @brief When the next events of one series come, and all the state that decides it.
 *
 * A law runs in the series' operational time: the time in which its rate is the
 * configured one. The market reaches a series by how fast its operational time
 * runs, never by changing a law, so a Hawkes branching ratio is the same in a
 * calm market and a busy one, and a burst fades faster in wall time when the
 * market is busy.
 */
using arrival_law = std::variant<periodic_law, poisson_law, hawkes_law>;

/**
 * @brief Build the law a spec names, at a stationary start.
 *
 * A periodic law draws its phase from the stream. A Hawkes law starts with its
 * stationary mean excess, so the first events of a run are at the long-run rate
 * rather than at the baseline alone.
 *
 * @throws std::invalid_argument when the spec fails
 *         validate_arrival_process_parameters.
 */
ORES_ANALYTICS_QUANT_EXPORT arrival_law make_arrival_law(const arrival_spec& spec,
                                                         math::rng_stream& stream);

/**
 * @brief Draw the operational time from the last event to the next one.
 *
 * A draw never changes the law, so a gap drawn and then not used leaves the law
 * as it was.
 */
ORES_ANALYTICS_QUANT_EXPORT double draw_gap(const arrival_law& law, math::rng_stream& stream);

/**
 * @brief The intensity integrated over @p gap from the law's state at the last event.
 *
 * Call it before record_event moves the law past the event the gap ended in. By the
 * time-rescaling theorem the compensated gaps of a stream that follows its law are
 * independent and Exp(1); a periodic law's are all one.
 */
ORES_ANALYTICS_QUANT_EXPORT double compensator(const arrival_law& law, double gap);

/**
 * @brief The law's intensity @p elapsed operational seconds after its last event.
 */
ORES_ANALYTICS_QUANT_EXPORT double intensity_after(const arrival_law& law, double elapsed);

/**
 * @brief Move a law past an event that came one gap after the last.
 *
 * @param gap the operational time that actually passed since the last event:
 *        the gap draw_gap returned, if the event came then. It is not checked.
 */
ORES_ANALYTICS_QUANT_EXPORT void record_event(arrival_law& law, double gap);

}

#endif
