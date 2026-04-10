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
#ifndef ORES_TRADING_DOMAIN_EQUITY_ACCUMULATOR_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_EQUITY_ACCUMULATOR_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Equity Accumulator instrument.
 *
 * Represents EquityAccumulator and EquityTaRF trades.
 */
struct equity_accumulator_instrument final {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this equity accumulator instrument.
     */
    boost::uuids::uuid instrument_id;

    boost::uuids::uuid party_id;
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief ORE product type code: EquityAccumulator or EquityTaRF.
     */
    std::string trade_type_code;

    std::string underlying_name;
    std::string currency;

    /**
     * @brief Strike price.
     */
    double strike = 0.0;

    /**
     * @brief Per-fixing accumulation amount. Must be positive.
     */
    double fixing_amount = 0.0;

    /**
     * @brief ISO 8601 date
     */
    std::string start_date;

    /**
     * @brief ISO 8601 date
     */
    std::string expiry_date;

    /**
     * @brief e.g. Daily, Weekly, Monthly
     */
    std::string fixing_frequency;

    /**
     * @brief Long or Short
     */
    std::string long_short;

    /**
     * @brief Knock-out barrier; absent when not specified.
     */
    std::optional<double> knock_out_level;

    /**
     * @brief TaRF only: target profit level.
     */
    std::optional<double> target_amount;

    /**
     * @brief TaRF: TargetFull or TargetExact; empty for accumulator.
     */
    std::string target_type;

    /**
     * @brief Accumulator, Decumulator, or TaRF.
     */
    std::string payoff_type;

    std::string description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
