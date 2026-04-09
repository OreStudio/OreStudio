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
#ifndef ORES_TRADING_DOMAIN_FX_DIGITAL_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_FX_DIGITAL_OPTION_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief FX Digital Option instrument.
 *
 * Routes: FxDigitalOption, FxDigitalBarrierOption, FxTouchOption
 * (OneTouch / NoTouch), FxDoubleTouchOption.
 *
 * Uses ForeignCurrency / DomesticCurrency naming to match ORE XML semantics.
 * option_type is absent for touch products (no Call/Put distinction).
 * strike is absent for touch products.
 * lower_barrier / upper_barrier capture single or double barrier levels.
 */
struct fx_digital_option_instrument final {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this FX digital option instrument.
     */
    boost::uuids::uuid instrument_id;

    boost::uuids::uuid party_id;
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief ORE product type code. One of: FxDigitalOption,
     * FxDigitalBarrierOption, FxTouchOption, FxDoubleTouchOption.
     */
    std::string trade_type_code;

    /**
     * @brief ForeignCurrency from ORE XML (source / underlying currency).
     */
    std::string foreign_currency;

    /**
     * @brief DomesticCurrency from ORE XML (quote / settlement currency).
     */
    std::string domestic_currency;

    /**
     * @brief Currency in which the payoff is denominated.
     */
    std::string payoff_currency;

    /**
     * @brief Fixed payoff amount. Must be positive.
     */
    double payoff_amount = 0.0;

    /**
     * @brief Option type: Call or Put. Empty for touch options.
     */
    std::string option_type;

    /**
     * @brief Option expiry date (ISO 8601 date string).
     */
    std::string expiry_date;

    /**
     * @brief Position direction: Long or Short.
     */
    std::string long_short;

    /**
     * @brief Strike level. Absent for touch options.
     */
    std::optional<double> strike;

    /**
     * @brief Barrier type (e.g. DownAndIn, DownAndOut, KnockIn, KnockOut).
     * Absent for plain digital options.
     */
    std::string barrier_type;

    /**
     * @brief Primary barrier level. Absent for plain digital options.
     */
    std::optional<double> lower_barrier;

    /**
     * @brief Second barrier level for FxDoubleTouchOption.
     */
    std::optional<double> upper_barrier;

    std::string description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
