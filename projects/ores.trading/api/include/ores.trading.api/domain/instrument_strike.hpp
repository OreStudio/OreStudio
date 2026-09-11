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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_STRIKE_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_STRIKE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief A strike a document states as a price, a yield or a bare number, keyed to the instrument.
 *
 * One row per instrument whose document stated a strike as a price or as
 * a yield, keyed to the instrument.
 *
 * The schema states the strike as a choice of three: a price with its
 * currency, a yield with its compounding, or a number with an optional
 * currency. A bare number reaches the bond option fact row instead, so
 * this table holds the two richer spellings. The three pairs are mutually
 * exclusive in a document, so at most one pair of columns is filled on a
 * row and the other four are null.
 */
struct instrument_strike final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument whose document stated this strike.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief The strike stated as a price.
     */
    std::optional<double> price_value;

    /**
     * @brief Currency the price strike is stated in.
     */
    std::optional<std::string> price_currency;

    /**
     * @brief The strike stated as a yield.
     */
    std::optional<double> yield_value;

    /**
     * @brief Compounding the yield strike is stated under.
     */
    std::optional<std::string> yield_compounding;

    /**
     * @brief The strike stated as a bare number, when it did not reach the option fact row.
     */
    std::optional<double> bare_value;

    /**
     * @brief Currency the bare strike is stated in.
     */
    std::optional<std::string> bare_currency;

    /**
     * @brief Username of the person who last modified this instrument strike.
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

/**
 * @brief Dispatch-key identifier for instrument_strike, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_strike&) {
    return "ores.trading.instrument_strike";
}

}

#endif
