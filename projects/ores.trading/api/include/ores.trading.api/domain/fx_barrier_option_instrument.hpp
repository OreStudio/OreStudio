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
#ifndef ORES_TRADING_DOMAIN_FX_BARRIER_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FX_BARRIER_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief FX Barrier Option instrument.
 *
 * Routes: FxBarrierOption, FxDoubleBarrierOption, FxEuropeanBarrierOption,
 * FxKIKOBarrierOption, FxGenericBarrierOption.
 *
 * lower_barrier holds the primary (or KO-side for KIKO) barrier level.
 * upper_barrier holds the second level for double-barrier and KIKO products;
 * absent for single-barrier types.
 * underlying_code is used by KIKO and generic barrier products.
 */
struct fx_barrier_option_instrument final {
    instrument_identity identity;

    std::string bought_currency;
    double bought_amount = 0.0;
    std::string sold_currency;
    double sold_amount = 0.0;

    /**
     * @brief Option type: Call or Put. Absent for FxGenericBarrierOption
     * with PayoffType=AssetOrNothing.
     */
    std::string option_type;

    /**
     * @brief Option expiry date (ISO 8601 date string).
     */
    std::string expiry_date;

    /**
     * @brief Optional settlement method (e.g. Cash, Physical).
     */
    std::string settlement;

    /**
     * @brief Primary barrier type (e.g. UpAndIn, DownAndOut, KIKO).
     */
    std::string barrier_type;

    /**
     * @brief Primary (or lower) barrier level. Must be positive.
     */
    double lower_barrier = 0.0;

    /**
     * @brief Second barrier level for double-barrier and KIKO products.
     */
    std::optional<double> upper_barrier;

    /**
     * @brief FX pair or index code (e.g. TR20H-EUR-USD). Used by
     * KIKO and generic barrier products.
     */
    std::string underlying_code;

    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
