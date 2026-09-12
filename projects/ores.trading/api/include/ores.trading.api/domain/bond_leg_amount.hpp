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
#ifndef ORES_TRADING_API_DOMAIN_BOND_LEG_AMOUNT_HPP
#define ORES_TRADING_API_DOMAIN_BOND_LEG_AMOUNT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One numbered amount of a bond leg: a notional, a rate, a spread, a cap, a floor or a
 * gearing, keyed to the leg and the role it plays.
 *
 * One row per numbered amount a bond leg states, keyed to the leg, the
 * role the amount plays and its ordinal in the document's list.
 *
 * The ORE schema spells the same pair of members six times: a value and
 * the date it starts. A notional is one, and so are a fixed leg's rate,
 * and a floating leg's spread, cap, floor and gearing. The rows differ
 * only in the role they play, so one table holds all six lists and the
 * role column names which one.
 *
 * The value is required on every one of the six, so the column is not
 * null. The start date is optional on all of them and is nullable here.
 * Both are an std::optional question in C++ only for the date, because
 * the schema states the value unconditionally.
 */
struct bond_leg_amount final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument this amount belongs to.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Which leg list of the instrument the leg holding this amount belongs to: bond,
     * trs_funding, repo or ascot_swap.
     */
    std::string leg_role;

    /**
     * @brief Ordinal of the leg within its list, counting from one.
     */
    int leg_number;

    /**
     * @brief Which of the leg's six amount lists this row belongs to: notional, rate, spread, cap,
     * floor or gearing.
     *
     * The container's member names are the values, so a reader rebuilds each list from the role
     * alone.
     */
    std::string amount_role;

    /**
     * @brief Ordinal of this amount within its list.
     *
     * The schema declares every one of the six lists unbounded, and a document's order is the order
     * it stated. The ordinal preserves that order.
     */
    int sequence_number;

    /**
     * @brief The amount, as a decimal.
     */
    double value;

    /**
     * @brief Date the amount takes effect (ISO 8601 date string), when the document states one.
     */
    std::optional<std::string> start_date;

    /**
     * @brief Username of the person who last modified this bond leg amount.
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
 * @brief Dispatch-key identifier for bond_leg_amount, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_leg_amount&) {
    return "ores.trading.bond_leg_amount";
}

}

#endif
