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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_FIX_FLOAT_SWAP_TERMS_HPP
#define ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_FIX_FLOAT_SWAP_TERMS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief the terms of a cross-currency fix-float swap that both legs share.
 *
 * The terms a cross-currency fix-float swap states once for both legs, as ORE's
 * SettlementDays, SettlementCalendar, SettlementConvention, EOM and
 * IsResettable. It is a part because its fields hold one invariant: each
 * applies to the whole swap at once, so neither leg can hold it alone. The
 * settlement fields fix the settlement date, the end of month rule rolls the
 * coupons of both legs, and the resettable flag resets the notional of the
 * swap. The fixed and floating legs beside it hold none of them.
 */
struct cross_currency_fix_float_swap_terms final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The surrogate key of this part row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The central convention row this part belongs to. One row per convention, so the
     * generated table adds the unique index on the live rows of (tenant_id, convention_id): a
     * convention fills this form or it does not, and never two of it.
     */
    boost::uuids::uuid convention_id;

    /**
     * @brief The party that owns the convention this part belongs to. Carried so row level security
     * can isolate the part without a join.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Number of business days between a trade and its settlement.
     */
    int settlement_days = 0;

    /**
     * @brief Calendars both legs settle on, comma separated when there is more than one.
     */
    std::string settlement_calendar;

    /**
     * @brief Business day convention the settlement date rolls on, as the canonical code the mapper
     * stores.
     */
    std::string settlement_convention;

    /**
     * @brief Whether the coupons of both legs roll to the end of the month.
     */
    std::optional<bool> eom;

    /**
     * @brief Whether the swap resets its notional, as a mark-to-market cross-currency swap does.
     */
    std::optional<bool> is_resettable;

    /**
     * @brief Username of the person who last modified this cross currency fix float swap terms.
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
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const cross_currency_fix_float_swap_terms&,
                           const cross_currency_fix_float_swap_terms&) = default;
};

/**
 * @brief Dispatch-key identifier for cross_currency_fix_float_swap_terms, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const cross_currency_fix_float_swap_terms&) {
    return "ores.refdata.cross_currency_fix_float_swap_terms";
}

}

#endif
