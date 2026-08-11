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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_BARRIER_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_BARRIER_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Equity Barrier Option instrument.
 *
 * Routes ORE product types: EquityBarrierOption,
 * EquityDoubleBarrierOption, EquityEuropeanBarrierOption.
 * underlying_name captures the ORE equity Name identifier.
 * lower_barrier/lower_barrier_type hold the single barrier level and
 * its UpIn/UpOut/DownIn/DownOut direction; upper_barrier and
 * upper_barrier_type are double-barrier-only (null/empty for a single
 * barrier); rebate is optional. expiry_date is an ISO 8601 date
 * string.
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
     * @brief Expiry date (ISO 8601 date string).
     */
    std::string expiry_date;

    /**
     * @brief European, American, or Bermudan.
     */
    std::string exercise_type;

    /**
     * @brief Position direction: Long or Short.
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
     * @brief Double-barrier only: upper barrier level.
     */
    std::optional<double> upper_barrier;

    /**
     * @brief Type for upper barrier; empty for single barrier.
     */
    std::string upper_barrier_type;

    /**
     * @brief Optional rebate paid when the barrier is breached.
     */
    std::optional<double> rebate;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

/**
 * @brief Dispatch-key identifier for equity_barrier_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_barrier_option_instrument&) {
    return "ores.trading.equity_barrier_option_instrument";
}

}

#endif
