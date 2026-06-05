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
#ifndef ORES_TRADING_DOMAIN_KNOCK_OUT_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_KNOCK_OUT_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <chrono>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Knock-out interest rate swap instrument.
 *
 * Represents a knock-out interest rate swap that terminates automatically
 * if the floating rate breaches a specified barrier level.
 */
struct knock_out_swap_instrument final {
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
     * @brief Barrier level that triggers knock-out.
     *
     * Expressed as a decimal fraction.
     */
    double barrier_level = 0.0;

    /**
     * @brief Barrier type: UpAndOut or DownAndOut.
     *
     * UpAndOut knocks out when rate rises above barrier; DownAndOut when it falls below.
     */
    std::string barrier_type;

    /**
     * @brief Optional JSON array of knock-out observation dates.
     *
     * ISO 8601 date strings for discrete barrier observation.
     */
    std::string knock_out_dates_json;

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
