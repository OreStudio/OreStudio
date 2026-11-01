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
#ifndef ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_BASIS_FLAT_LEG_HPP
#define ORES_REFDATA_API_DOMAIN_CROSS_CURRENCY_BASIS_FLAT_LEG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief the flat leg of a cross-currency basis swap, which pays its index with no spread.
 *
 * The leg of a cross-currency basis swap that pays its index flat, as ORE's
 * FlatIndex with the Flat* fixing rules: whether the index is resettable,
 * the tenor, the payment lag, whether the spread is included in the coupon, the
 * lookback, the fixing days, the rate cutoff, whether coupons are averaged, and
 * whether the observation is shifted. They are a part because each states how
 * this one leg builds its coupon; the spread leg beside it states its own. Each
 * column keeps the flat_ prefix of ORE's element, because the screen shows
 * both legs of one convention on one row, and the two legs' columns would
 * otherwise share their names.
 */
struct cross_currency_basis_flat_leg final {
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
     * @brief The index the flat leg pays.
     */
    std::string flat_index;

    /**
     * @brief Whether the flat leg's index is resettable.
     */
    std::optional<bool> flat_index_is_resettable;

    /**
     * @brief The tenor of the leg's index, as ORE's period code.
     */
    std::optional<std::string> flat_tenor;

    /**
     * @brief Business days between the leg's period end and its payment.
     */
    std::optional<int> flat_payment_lag;

    /**
     * @brief Whether the spread is included in the coupon the index builds.
     */
    std::optional<bool> flat_include_spread;

    /**
     * @brief The lookback the fixing takes, as ORE's period code.
     */
    std::optional<std::string> flat_lookback;

    /**
     * @brief Business days before the period start at which the leg fixes.
     */
    std::optional<int> flat_fixing_days;

    /**
     * @brief Business days before the period end at which the leg's rate is cut off.
     */
    std::optional<int> flat_rate_cutoff;

    /**
     * @brief Whether coupons are averaged rather than compounded.
     */
    std::optional<bool> flat_is_averaged;

    /**
     * @brief Whether the observation is shifted to the payment date.
     */
    std::optional<bool> flat_observation_shift;

    /**
     * @brief Username of the person who last modified this cross currency basis flat leg.
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
    friend bool operator==(const cross_currency_basis_flat_leg&,
                           const cross_currency_basis_flat_leg&) = default;
};

/**
 * @brief Dispatch-key identifier for cross_currency_basis_flat_leg, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cross_currency_basis_flat_leg&) {
    return "ores.refdata.cross_currency_basis_flat_leg";
}

}

#endif
