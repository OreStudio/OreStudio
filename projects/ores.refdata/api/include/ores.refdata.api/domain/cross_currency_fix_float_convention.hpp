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
#ifndef ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_FIX_FLOAT_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a cross-currency swap with one fixed leg and one floating leg.
 *
 * Describes how ORE builds a cross-currency swap whose fixed leg pays one
 * currency and whose floating leg pays an index in another. Corresponds to the
 * <CrossCurrencyFixFloat> element in ORE conventions.xml. The id field is the
 * natural key (ORE <Id> element).
 *
 * Nine fields are required and nine are optional. The corpus sets all nine
 * required in all forty-one elements and sets none of the optional ones, so the
 * optional half of the mapping is proven by a case that builds the element rather
 * than by the corpus walk.
 */
struct cross_currency_fix_float_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief Unique cross-currency fix-float identifier.
     */
    std::string id;

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
     * @brief Currency the fixed leg pays.
     */
    std::string fixed_currency;

    /**
     * @brief Payment frequency of the fixed leg, as the canonical code the mapper stores.
     */
    std::string fixed_frequency;

    /**
     * @brief Business day convention of the fixed leg.
     */
    std::string fixed_convention;

    /**
     * @brief Day count fraction of the fixed leg.
     */
    std::string fixed_day_count_fraction;

    /**
     * @brief Index the floating leg pays.
     */
    std::string index;

    /**
     * @brief Whether coupons roll to the end of the month.
     */
    std::optional<bool> eom;

    /**
     * @brief Whether the fixed leg's rate resets.
     */
    std::optional<bool> is_resettable;

    /**
     * @brief Whether the floating leg's index resets.
     */
    std::optional<bool> float_index_is_resettable;

    /**
     * @brief Whether the spread is included in the coupon that carries it.
     */
    std::optional<bool> include_spread;

    /**
     * @brief Lookback period applied to the index's fixings, as ORE's period code.
     */
    std::optional<std::string> lookback;

    /**
     * @brief Number of days between a fixing and the coupon it feeds.
     */
    std::optional<int> fixing_days;

    /**
     * @brief Number of days before a payment date after which no fixing is read.
     */
    std::optional<int> rate_cutoff;

    /**
     * @brief Whether the floating leg's coupons are averaged rather than compounded.
     */
    std::optional<bool> is_averaged;

    /**
     * @brief Whether the observation dates shift with the coupon dates.
     */
    std::optional<bool> observation_shift;

    /**
     * @brief Username of the person who last modified this cross-currency fix-float convention.
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
    friend bool operator==(const cross_currency_fix_float_convention&,
                           const cross_currency_fix_float_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for cross_currency_fix_float_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const cross_currency_fix_float_convention&) {
    return "ores.refdata.cross_currency_fix_float_convention";
}

}

#endif
