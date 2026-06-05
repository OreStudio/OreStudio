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
#ifndef ORES_TRADING_DOMAIN_FX_VARIANCE_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FX_VARIANCE_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <string>

namespace ores::trading::domain {

/**
 * @brief FX Variance Swap instrument.
 *
 * Routes ORE product type: FxVarianceSwap.
 * moment_type distinguishes Variance swaps from Volatility swaps.
 */
struct fx_variance_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Variance observation start date (ISO 8601 date string).
     */
    std::string start_date;

    /**
     * @brief Variance observation end date (ISO 8601 date string).
     *
     * Must be after start_date.
     */
    std::string end_date;

    /**
     * @brief Settlement currency.
     */
    std::string currency;

    /**
     * @brief FX pair or index identifier (e.g. TR20H-EUR-USD).
     */
    std::string underlying_code;

    /**
     * @brief Position direction: Long or Short.
     */
    std::string long_short;

    /**
     * @brief Fixed variance or volatility strike. Must be positive.
     */
    double strike = 0.0;

    /**
     * @brief Notional amount for PnL scaling. Must be positive.
     */
    double notional = 0.0;

    /**
     * @brief Whether the product is a Variance or Volatility swap.
     */
    std::string moment_type;

    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
