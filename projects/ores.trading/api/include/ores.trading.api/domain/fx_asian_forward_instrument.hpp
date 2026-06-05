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
#ifndef ORES_TRADING_DOMAIN_FX_ASIAN_FORWARD_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FX_ASIAN_FORWARD_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief FX Asian Forward instrument.
 *
 * Routes: FxAverageForward, FxTaRF (Target Accrual Redemption Forward).
 *
 * Complex observation schedules and range bounds are a Phase 2 coverage gap.
 * fx_index captures Underlying.Name for the FX fixing source.
 */
struct fx_asian_forward_instrument final {
    instrument_identity identity;

    /**
     * @brief FX index / underlying name (e.g. FX-TR20H-EUR-USD).
     */
    std::string fx_index;

    // FxAverageForward-specific fields
    /**
     * @brief Reference (fixing) currency for FxAverageForward.
     */
    std::string reference_currency;

    /**
     * @brief Reference notional amount for FxAverageForward.
     */
    std::optional<double> reference_notional;

    /**
     * @brief Settlement currency for FxAverageForward.
     */
    std::string settlement_currency;

    /**
     * @brief Settlement notional amount for FxAverageForward.
     */
    std::optional<double> settlement_notional;

    /**
     * @brief Payment date for FxAverageForward (ISO 8601 date string).
     */
    std::string payment_date;

    /**
     * @brief Position direction for FxAverageForward: Long or Short.
     */
    std::string long_short;

    // FxTaRF-specific fields
    /**
     * @brief Settlement currency for FxTaRF.
     */
    std::string currency;

    /**
     * @brief Per-fixing notional amount for FxTaRF.
     */
    std::optional<double> fixing_amount;

    /**
     * @brief Profit cap (target amount) for FxTaRF.
     */
    std::optional<double> target_amount;

    /**
     * @brief Fixed strike rate for FxTaRF.
     */
    std::optional<double> strike;

    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
