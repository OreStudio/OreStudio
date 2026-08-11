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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_DIGITAL_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_DIGITAL_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Equity Digital Option instrument.
 *
 * Represents EquityDigitalOption and EquityTouchOption trades.
 * underlying_name captures the ORE equity Name identifier;
 * option_type is Call or Put (digital only); strike is digital
 * only; barrier_level and barrier_type are touch only; the two
 * product families are mutually exclusive, enforced by a cross-column
 * check. expiry_date is an ISO 8601 date string.
 */
struct equity_digital_option_instrument final {
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
     * @brief Payoff notional / contract size. Must be positive.
     */
    double notional = 0.0;

    /**
     * @brief Call or Put; empty for touch options.
     */
    std::string option_type;

    /**
     * @brief Digital only; absent for touch.
     */
    std::optional<double> strike;

    /**
     * @brief Touch only; absent for digital.
     */
    std::optional<double> barrier_level;

    /**
     * @brief e.g. UpIn, DownOut; empty for digital.
     */
    std::string barrier_type;

    /**
     * @brief ISO 8601 date string.
     */
    std::string expiry_date;

    /**
     * @brief Long or Short.
     */
    std::string long_short;

    /**
     * @brief Digital payout; absent when not specified.
     */
    std::optional<double> payout_amount;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

/**
 * @brief Dispatch-key identifier for equity_digital_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_digital_option_instrument&) {
    return "ores.trading.equity_digital_option_instrument";
}

}

#endif
