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
#ifndef ORES_TRADING_DOMAIN_INFLATION_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_INFLATION_SWAP_INSTRUMENT_HPP

#include <chrono>
#include <string>
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/domain/audit_record.hpp"

namespace ores::trading::domain {

/**
 * @brief Inflation-linked interest rate swap instrument.
 *
 * Represents an inflation-linked swap where one leg pays a fixed or floating
 * rate and the other is linked to an inflation index (e.g., CPI, RPI).
 */
struct inflation_swap_instrument final {
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
     * @brief Inflation index code.
     *
     * e.g., UKRPI, USCPI, EUHICPXT.
     */
    std::string inflation_index_code;

    /**
     * @brief Optional base CPI value at inception.
     *
     * Used to calculate the inflation accrual. Must be positive if set.
     */
    std::optional<double> base_cpi;

    /**
     * @brief Optional CPI lag convention.
     *
     * e.g., 3M, 2M. Specifies the publication lag for the inflation index.
     */
    std::string lag_convention;

    /**
     * @brief Optional free-text description.
     *
     * Human-readable notes about this instrument.
     */
    std::string description;

    utility::domain::audit_record audit;
};

}

#endif
