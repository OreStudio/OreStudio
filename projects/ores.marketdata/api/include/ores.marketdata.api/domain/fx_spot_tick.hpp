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
#ifndef ORES_MARKETDATA_API_DOMAIN_FX_SPOT_TICK_HPP
#define ORES_MARKETDATA_API_DOMAIN_FX_SPOT_TICK_HPP

#include <chrono>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief A single FX spot mid-price tick, published per-ORE-key via NATS.
 *
 * Serialised as JSON via rfl::json. Lives in ores.marketdata.api so it is
 * shared by the publisher (ores.synthetic.service) and subscriber
 * (ores.marketdata.client) without either depending on the other.
 */
struct fx_spot_tick final {
    /**
     * @brief ORE canonical key, e.g. "FX/RATE/EUR/USD".
     */
    std::string ore_key;

    /**
     * @brief UTC timestamp of the tick.
     */
    std::chrono::system_clock::time_point datetime;

    /**
     * @brief Mid-price, e.g. 1.08456.
     */
    double mid = 0.0;
};

}

#endif
