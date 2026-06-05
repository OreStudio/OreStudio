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
#ifndef ORES_TRADING_DOMAIN_CALLABLE_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_CALLABLE_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <chrono>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Callable interest rate swap instrument.
 *
 * Represents a callable interest rate swap where one party has the right
 * to terminate the swap early on specified call dates.
 */
struct callable_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Swap effective start date.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::string start_date;

    /**
     * @brief Swap maturity date.
     *
     * Must be after start_date.
     */
    std::string maturity_date;

    /**
     * @brief Optional JSON array of call dates.
     *
     * ISO 8601 date strings when the party may exercise the call option.
     */
    std::string call_dates_json;

    /**
     * @brief Optional call type: Bermudan or One-Time.
     *
     * Bermudan allows multiple call dates; One-Time allows exactly one.
     */
    std::string call_type;

    /**
     * @brief Optional free-text description.
     *
     * Human-readable notes about this instrument.
     */
    std::string description;

    dq::domain::audit_record audit;
};

}

#endif
