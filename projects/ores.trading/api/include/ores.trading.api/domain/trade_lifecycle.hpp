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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_LIFECYCLE_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_LIFECYCLE_HPP

#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Key dates and timestamps for a trade.
 *
 * One of five field-groups that compose trading::domain::trade via
 * rfl::Flatten<T>.  See trade_identity_field_group.json for the
 * rationale behind the sub-struct decomposition.
 * 
 * This group carries the temporal dimension fields (trade date,
 * effective date, termination date, and execution timestamp).
 * All are optional because not every trade type requires all dates
 * at creation time.
 */
struct trade_lifecycle {
    /**
     * @brief Date on which the trade was agreed (ISO-8601 date string).
     */
    std::optional<std::string> trade_date;

    /**
     * @brief Date from which the trade becomes effective (ISO-8601 date string).
     */
    std::optional<std::string> effective_date;

    /**
     * @brief Scheduled end date of the trade (ISO-8601 date string).
     */
    std::optional<std::string> termination_date;

    /**
     * @brief Exact moment the trade was executed (ISO-8601 timestamp string).
     */
    std::optional<std::string> execution_timestamp;

};

}

#endif
