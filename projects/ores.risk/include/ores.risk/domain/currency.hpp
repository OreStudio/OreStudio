/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_RISK_DOMAIN_CURRENCY_HPP
#define ORES_RISK_DOMAIN_CURRENCY_HPP

#include <string>

namespace ores::risk::domain {

/**
 * @brief Represents a currency with its metadata and formatting rules.
 */
struct currency final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief ISO 4217 alphabetic code (e.g., "USD").
     */
    std::string iso_code;

    /**
     * @brief Full name of the currency (e.g., "United States Dollar").
     */
    std::string name;

    /**
     * @brief ISO 4217 numeric code (e.g., "840").
     */
    std::string numeric_code;
    /**
     * @brief Currency symbol (e.g., "$").
     */
    std::string symbol;
    /**
     * @brief Symbol for fractional unit (e.g., "cent").
     */
    std::string fraction_symbol;

    /**
     * @brief Number of fractional units per whole unit (e.g., 100 for cents).
     */
    int fractions_per_unit;

    /**
     * @brief Rounding method for fractional amounts.
     */
    std::string rounding_type;

    /**
     * @brief Decimal places to round to during formatting.
     */
    int rounding_precision;

    /**
     * @brief Format string for display.
     */
    std::string format;

    /**
     * @brief Type classification (e.g., fiat, crypto, major, minor, etc.).
     */
    std::string currency_type;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string recorded_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::string recorded_at;
};

}

#endif
