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
#ifndef ORES_TRADING_DOMAIN_SWAPTION_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_SWAPTION_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Swaption (option on an interest rate swap) instrument.
 *
 * Represents a swaption — an option granting the right to enter into an
 * interest rate swap at a future date. Exercise type may be European,
 * Bermudan, or American.
 */
struct swaption_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this swaption instrument.
     *
     * Surrogate key for the instrument record.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Party that owns this instrument.
     *
     * Set from session variable app.current_party_id.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Optional soft FK to the parent trade.
     *
     * Links instrument to a trade if applicable.
     */
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief Option expiry date.
     *
     * ISO 8601 date string (YYYY-MM-DD).
     */
    std::string expiry_date;

    /**
     * @brief Exercise type: European, Bermudan, or American.
     *
     * Determines when the option may be exercised.
     */
    std::string exercise_type;

    /**
     * @brief Settlement type: Cash or Physical.
     *
     * Determines how the swaption is settled upon exercise.
     */
    std::string settlement_type;

    /**
     * @brief Position direction: Long or Short.
     *
     * Indicates whether the party holds or writes the option.
     */
    std::string long_short;

    /**
     * @brief Optional underlying swap start date.
     *
     * ISO 8601 date string (YYYY-MM-DD). Null if not yet determined.
     */
    std::string start_date;

    /**
     * @brief Optional underlying swap maturity date.
     *
     * ISO 8601 date string (YYYY-MM-DD). Null if not yet determined.
     */
    std::string maturity_date;

    /**
     * @brief Optional free-text description.
     *
     * Human-readable notes about this instrument.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this swaption instrument.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
