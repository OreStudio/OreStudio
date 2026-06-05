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
#ifndef ORES_TRADING_DOMAIN_FRA_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FRA_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <chrono>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Forward Rate Agreement (FRA) instrument.
 *
 * Represents a Forward Rate Agreement instrument that fixes a future
 * interest rate for a notional principal amount over a specified period.
 */
struct fra_instrument final {
    instrument_identity identity;

    /**
     * @brief FRA start date.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::string start_date;

    /**
     * @brief FRA end date.
     *
     * Must be after start_date.
     */
    std::string end_date;

    /**
     * @brief ISO 4217 currency code.
     *
     * e.g., USD, EUR, GBP.
     */
    std::string currency;

    /**
     * @brief Floating rate index code.
     *
     * e.g., LIBOR, EURIBOR, SOFR.
     */
    std::string rate_index;

    /**
     * @brief Position direction: Long or Short.
     *
     * Indicates whether the party is a buyer (Long) or seller (Short).
     */
    std::string long_short;

    /**
     * @brief Fixed contract rate.
     *
     * Expressed as a decimal fraction.
     */
    double strike = 0.0;

    /**
     * @brief Notional principal amount.
     *
     * Must be positive.
     */
    double notional = 0.0;

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
