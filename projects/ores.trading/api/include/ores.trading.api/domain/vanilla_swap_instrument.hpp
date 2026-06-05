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
#ifndef ORES_TRADING_DOMAIN_VANILLA_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_VANILLA_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <chrono>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Vanilla interest rate swap or cross-currency swap instrument.
 *
 * Represents a plain vanilla interest rate swap or cross-currency swap
 * instrument linking two legs via the swap_legs table.
 */
struct vanilla_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Swap effective/start date.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::string start_date;

    /**
     * @brief Swap maturity/termination date.
     *
     * Must be after start_date.
     */
    std::string maturity_date;

    /**
     * @brief Optional settlement lag in business days.
     *
     * Number of business days between execution and effective date.
     */
    std::optional<int> settlement_lag;

    /**
     * @brief Optional netting set identifier.
     *
     * Groups instruments for netting purposes.
     */
    std::string netting_set_id;

    /**
     * @brief Optional free-text description.
     *
     * Human-readable notes about this instrument.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
