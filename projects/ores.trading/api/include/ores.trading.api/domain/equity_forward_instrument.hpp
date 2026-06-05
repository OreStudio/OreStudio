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
#ifndef ORES_TRADING_DOMAIN_EQUITY_FORWARD_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_EQUITY_FORWARD_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Equity Forward instrument.
 *
 * Represents EquityForward trades.
 */
struct equity_forward_instrument final {
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
     * @brief Number of shares / contracts. Must be positive.
     */
    double quantity = 0.0;

    /**
     * @brief Fixed delivery price; absent for at-market forwards.
     */
    std::optional<double> forward_price;

    /**
     * @brief ISO 8601 date string.
     */
    std::string expiry_date;

    /**
     * @brief Long or Short.
     */
    std::string long_short;

    /**
     * @brief Cash or Physical; empty when not specified.
     */
    std::string settlement_type;

    std::string description;

    ores::dq::domain::audit_record audit;
};

}

#endif
