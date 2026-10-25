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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_I_ARRIVAL_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_I_ARRIVAL_PROCESS_HPP

#include "ores.analytics.quant/export.hpp"

namespace ores::analytics::quant::domain {

/**
 * @brief A point process: the times at which a stream of events arrives.
 *
 * The sibling of i_stochastic_process, and deliberately a separate interface.
 * A value process answers "what is the price now"; an arrival process answers
 * "when does the next event come". No value process can answer the second
 * question, and forcing both onto one interface would make every value process
 * carry a clock it never uses.
 *
 * The unit is the second: next_gap() returns the wait from the last event to
 * the next one, in seconds. The single stream is enough for the tick sources
 * this library feeds; the multivariate form, where events on one stream excite
 * others, is described in the Hawkes note and is not implemented.
 *
 * A process owns its state and its random engine and is not thread-safe, like
 * every value process.
 *
 * Inspired by the tick library (BSD 3-clause, the tick developers, CMAP); see
 * =doc/knowledge/external/tick.org=. This is a reimplementation from the published
 * algorithms, not a copy of its source.
 */
class ORES_ANALYTICS_QUANT_EXPORT i_arrival_process {
public:
    virtual ~i_arrival_process() = default;

    /**
     * @brief Draw the wait to the next arrival, in seconds.
     *
     * The gap is measured from the last arrival, or from the start of the
     * stream before the first one.
     */
    virtual double next_gap() = 0;
};

}

#endif
