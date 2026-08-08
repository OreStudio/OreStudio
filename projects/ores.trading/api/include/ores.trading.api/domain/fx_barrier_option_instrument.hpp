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
#ifndef ORES_TRADING_API_DOMAIN_FX_BARRIER_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_BARRIER_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief FX Barrier Option instrument.
 *
 * Routes ORE product types: FxBarrierOption, FxDoubleBarrierOption,
 * FxEuropeanBarrierOption, FxKIKOBarrierOption, FxGenericBarrierOption.
 * upper_barrier is optional (single-barrier products leave it NULL).
 */
struct fx_barrier_option_instrument final {
    instrument_identity identity;

    /**
     * @brief Currency being bought.
     */
    std::string bought_currency;

    /**
     * @brief Amount being bought. Must be positive.
     */
    double bought_amount = 0.0;

    /**
     * @brief Currency being sold.
     */
    std::string sold_currency;

    /**
     * @brief Amount being sold. Must be positive.
     */
    double sold_amount = 0.0;

    /**
     * @brief Option type (e.g. Call or Put).
     */
    std::string option_type;

    /**
     * @brief Option expiry date (ISO 8601 date string).
     */
    std::string expiry_date;

    /**
     * @brief Optional settlement instructions.
     */
    std::string settlement;

    /**
     * @brief Barrier style (e.g. UpAndIn, DownAndOut).
     */
    std::string barrier_type;

    /**
     * @brief Lower barrier level. Must be positive.
     */
    double lower_barrier = 0.0;

    /**
     * @brief Upper barrier level (double-barrier products only; NULL otherwise).
     */
    std::optional<double> upper_barrier;

    /**
     * @brief Optional underlying identifier.
     */
    std::string underlying_code;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

/**
 * @brief Dispatch-key identifier for fx_barrier_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_barrier_option_instrument&) {
    return "ores.trading.fx_barrier_option_instrument";
}

}

#endif
