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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_ASIAN_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_ASIAN_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Asian option on an equity underlying.
 *
 * Routes the ORE product type EquityAsianOption.
 * underlying_name captures the ORE equity Name identifier;
 * option_type is Call or Put; exercise_type is European or
 * American; average_type is Arithmetic or Geometric.
 * averaging_start_date and averaging_end_date delimit the averaging
 * period; expiry_date is an ISO 8601 date string.
 */
struct equity_asian_option_instrument final {
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
     * @brief Notional amount. Must be positive.
     */
    double notional = 0.0;

    /**
     * @brief Call or Put.
     */
    std::string option_type;

    /**
     * @brief Strike price. Must be non-negative.
     */
    double strike = 0.0;

    /**
     * @brief Expiry date (ISO 8601 date string).
     */
    std::string expiry_date;

    /**
     * @brief European or American.
     */
    std::string exercise_type;

    /**
     * @brief Position direction: Long or Short.
     */
    std::string long_short;

    /**
     * @brief Arithmetic or Geometric.
     */
    std::string average_type;

    /**
     * @brief Averaging period start (ISO 8601 date string).
     */
    std::string averaging_start_date;

    /**
     * @brief Averaging period end (ISO 8601 date string).
     */
    std::string averaging_end_date;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

/**
 * @brief Dispatch-key identifier for equity_asian_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_asian_option_instrument&) {
    return "ores.trading.equity_asian_option_instrument";
}

}

#endif
