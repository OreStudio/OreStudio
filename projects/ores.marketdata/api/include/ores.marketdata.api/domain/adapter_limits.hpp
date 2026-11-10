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
#ifndef ORES_MARKETDATA_API_DOMAIN_ADAPTER_LIMITS_HPP
#define ORES_MARKETDATA_API_DOMAIN_ADAPTER_LIMITS_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include <chrono>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The limits the market data service puts on the ticks of every adapter.
 *
 * They are the same for every adapter. The service drops a tick beyond them, and an adapter
 * that wants to test the drop sends a tick beyond them, so both read the limit from here.
 */
struct adapter_limits {
    /// The age beyond which a tick of a live source is stale.
    static constexpr std::chrono::milliseconds max_tick_age{5000};

    /**
     * @brief The age beyond which a quote of an end-of-day source is stale.
     *
     * Such a source sends one value each business day, stamped at its close. The value stays
     * current until the next one replaces it. A Thursday close before Good Friday and Easter
     * Monday is still current on the Tuesday morning, nearly five days later.
     */
    static constexpr std::chrono::milliseconds max_end_of_day_age = std::chrono::days{5};

    /// The age beyond which a quote of a source that offers @p service is stale.
    static constexpr std::chrono::milliseconds max_quote_age(std::string_view service) {
        return service == "end_of_day" ? max_end_of_day_age : max_tick_age;
    }

    /// The largest absolute value a tick may carry.
    static constexpr double max_abs_value = 1e12;
};

}

#endif
