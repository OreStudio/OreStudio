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
#ifndef ORES_REFDATA_API_DOMAIN_PAYMENT_FREQUENCY_HPP
#define ORES_REFDATA_API_DOMAIN_PAYMENT_FREQUENCY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief ORE's canonical payment frequency codes (Annual, Semiannual, Quarterly...), each carrying
 * a period unit/multiplier for schedule building.
 *
 * Reference data table for how often a leg's cashflows occur, sourced
 * from ORE's authoritative frequencyType enumeration
 * (external/ore/xsd/ore_types.xsd) -- Once, Annual, Semiannual,
 * Quarterly, Bimonthly, Monthly, Lunarmonth, Weekly, Daily
 * (the canonical long-form names; ORE also accepts single-letter
 * aliases -- Z/A/S/Q/B/M/L/W/D -- not modelled here since this table's
 * code is the long form other components store and display). Each row
 * carries a period_unit/period_multiplier pair reusing
 * [[id:01E76440-B9A5-4D0A-A32C-B0C4A7484B26][tenor_unit]]'s own vocabulary
 * (DAY/WEEK/MONTH/YEAR, plus the NONE sentinel for Once, which
 * has no periodic step at all -- a single payment at termination), so a
 * caller building a payment schedule (e.g. the IR Curve Template's swap
 * fixed leg) can reuse the exact same period-stepping arithmetic
 * ores::refdata::domain::resolve_end_date() already implements for
 * tenors, rather than re-deriving month/day counts from the code string.
 * Not scoped to any single consumer -- this table exists as reusable
 * reference data, the same category as day_count_fraction_type or
 * business_day_convention_type. Managed by the system tenant.
 *
 * This table replaces ores.trading's older payment_frequency_type
 * entity outright -- there is no case for two types modelling the same
 * ORE enumeration ("payment frequency conventions" vs "payment
 * frequencies" is a flimsy distinction); every ores.trading column
 * that stored a payment-frequency code (swap_leg, credit_instrument,
 * commodity_instrument, equity_swap_instrument) now validates
 * against this table instead. See the parent story's * Decisions for
 * the full reasoning.
 */
struct payment_frequency final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique payment frequency code -- ORE's canonical long-form frequencyType value.
     *
     * Examples: 'Annual', 'Semiannual', 'Quarterly', 'Monthly', 'Once'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the payment frequency.
     */
    std::string name;

    /**
     * @brief Detailed description of the payment frequency.
     */
    std::string description;

    /**
     * @brief Period unit for this frequency's step (references tenor_unit.code: DAY, WEEK, MONTH,
     * YEAR, or NONE for Once, which has no periodic step).
     */
    std::string period_unit;

    /**
     * @brief The count paired with period_unit, e.g. 3 for Quarterly's MONTH unit. Null only for
     * Once (period_unit = NONE).
     */
    std::optional<int> period_multiplier;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this payment frequency.
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
 * @brief Dispatch-key identifier for payment_frequency, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const payment_frequency&) {
    return "ores.refdata.payment_frequency";
}

}

#endif
