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
#ifndef ORES_TRADING_DOMAIN_EQUITY_BARRIER_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_EQUITY_BARRIER_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Equity Barrier Option instrument.
 *
 * Represents EquityBarrierOption, EquityDoubleBarrierOption, and
 * EquityEuropeanBarrierOption trades.
 */
struct equity_barrier_option_instrument final {
    instrument_identity identity;

    /**
     * @brief ORE equity Name identifier.
     */
    std::string underlying_name;

    /**
     * @brief ISO 4217 currency code.
     */
    std::string currency;

    /**
     * @brief Contract quantity / notional. Must be positive.
     */
    double notional = 0.0;

    /**
     * @brief Call or Put.
     */
    std::string option_type;

    /**
     * @brief Strike price. Non-negative.
     */
    double strike = 0.0;

    /**
     * @brief ISO 8601 date string.
     */
    std::string expiry_date;

    /**
     * @brief European, American, or Bermudan.
     */
    std::string exercise_type;

    /**
     * @brief Long or Short.
     */
    std::string long_short;

    /**
     * @brief Lower / single barrier level.
     */
    double lower_barrier = 0.0;

    /**
     * @brief UpIn, UpOut, DownIn, DownOut.
     */
    std::string lower_barrier_type;

    /**
     * @brief Double-barrier only.
     */
    std::optional<double> upper_barrier;

    /**
     * @brief Type for upper barrier; empty for single barrier.
     */
    std::string upper_barrier_type;

    std::optional<double> rebate;

    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
