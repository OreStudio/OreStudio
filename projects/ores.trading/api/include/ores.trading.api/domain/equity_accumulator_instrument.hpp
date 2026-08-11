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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_ACCUMULATOR_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_ACCUMULATOR_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Equity Accumulator instrument.
 *
 * Routes ORE product types: EquityAccumulator and EquityTaRF.
 * underlying_name captures the ORE equity Name identifier.
 * knock_out_level is optional (absent when not specified);
 * target_amount/target_type are TaRF-only (TargetFull or TargetExact)
 * and must be null/empty for a plain accumulator -- enforced by a
 * cross-column check; payoff_type is Accumulator, Decumulator, or
 * TaRF. start_date and expiry_date are ISO 8601 date strings.
 */
struct equity_accumulator_instrument final {
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
     * @brief Strike price.
     */
    double strike = 0.0;

    /**
     * @brief Per-fixing accumulation amount. Must be positive.
     */
    double fixing_amount = 0.0;

    /**
     * @brief Start date (ISO 8601 date string).
     */
    std::string start_date;

    /**
     * @brief Expiry date (ISO 8601 date string).
     */
    std::string expiry_date;

    /**
     * @brief e.g. Daily, Weekly, Monthly.
     */
    std::string fixing_frequency;

    /**
     * @brief Position direction: Long or Short.
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

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

/**
 * @brief Dispatch-key identifier for equity_accumulator_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_accumulator_instrument&) {
    return "ores.trading.equity_accumulator_instrument";
}

}

#endif
