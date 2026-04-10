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
#ifndef ORES_TRADING_DOMAIN_FX_VANILLA_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FX_VANILLA_OPTION_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief FX Vanilla Option instrument (European and American).
 *
 * Represents FxOption trades. Strike is implicit in bought/sold amounts;
 * there is no separate strike field in ORE's FxOptionData.
 */
struct fx_vanilla_option_instrument final {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this FX vanilla option instrument.
     */
    boost::uuids::uuid instrument_id;

    boost::uuids::uuid party_id;
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief ORE product type code: FxOption.
     */
    std::string trade_type_code;

    /**
     * @brief ISO 4217 currency code of the bought leg.
     */
    std::string bought_currency;

    /**
     * @brief Amount bought in bought_currency. Must be positive.
     */
    double bought_amount = 0.0;

    /**
     * @brief ISO 4217 currency code of the sold leg.
     */
    std::string sold_currency;

    /**
     * @brief Amount sold in sold_currency. Must be positive.
     */
    double sold_amount = 0.0;

    /**
     * @brief Option type: Call or Put.
     */
    std::string option_type;

    /**
     * @brief Option expiry date (ISO 8601 date string).
     */
    std::string expiry_date;

    /**
     * @brief Exercise style: European or American.
     */
    std::string exercise_style;

    /**
     * @brief Optional settlement method (e.g. Cash, Physical).
     */
    std::string settlement;

    std::string description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
