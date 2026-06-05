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
#ifndef ORES_TRADING_DOMAIN_FX_FORWARD_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FX_FORWARD_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <string>

namespace ores::trading::domain {

/**
 * @brief FX Forward instrument.
 *
 * Represents FxForward and FxSwap (near leg only) trades. The FxSwap far
 * leg (FarDate, FarBoughtAmount, FarSoldAmount) is a documented coverage gap.
 */
struct fx_forward_instrument final {
    instrument_identity identity;

    /**
     * @brief ISO 4217 currency code of the bought leg (e.g. EUR).
     */
    std::string bought_currency;

    /**
     * @brief Amount bought in bought_currency. Must be positive.
     */
    double bought_amount = 0.0;

    /**
     * @brief ISO 4217 currency code of the sold leg (e.g. USD).
     */
    std::string sold_currency;

    /**
     * @brief Amount sold in sold_currency. Must be positive.
     */
    double sold_amount = 0.0;

    /**
     * @brief Settlement / value date (ISO 8601 date string).
     *
     * For FxSwap this is the near leg date only.
     */
    std::string value_date;

    /**
     * @brief Optional settlement method (e.g. Cash, Physical).
     */
    std::string settlement;

    std::string description;

    dq::domain::audit_record audit;
};

}

#endif
