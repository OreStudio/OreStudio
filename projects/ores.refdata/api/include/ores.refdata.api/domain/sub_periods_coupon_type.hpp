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
#ifndef ORES_REFDATA_API_DOMAIN_SUB_PERIODS_COUPON_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_SUB_PERIODS_COUPON_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief How a floating leg's sub-period coupons accrue: compounded or averaged.
 *
 * Reference data table defining how a floating leg's coupons accrue when the
 * leg is divided into sub-periods. Values: 'Compounding' (the sub-period rates
 * compound over the accrual) and 'Averaging' (they average).
 *
 * Two values only, and they are the two the swap_convention and
 * tenor_basis_swap_convention models' own sub_periods_coupon_type column
 * descriptions name. The column was plain text, so a client offered free text
 * where its neighbours offer a pick; this entity is what makes it a pick, in
 * the shape the other convention term lists already have.
 *
 * The values are mutually exclusive -- a leg accrues one way -- and managed by
 * the system tenant.
 */
struct sub_periods_coupon_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique sub-periods coupon type code.
     *
     * Examples: 'Compounding', 'Averaging'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the sub-periods coupon type.
     */
    std::string name;

    /**
     * @brief Detailed description of how the sub-period coupons accrue under this type.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this sub-periods coupon type.
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
    friend bool operator==(const sub_periods_coupon_type&,
                           const sub_periods_coupon_type&) = default;
};

/**
 * @brief Dispatch-key identifier for sub_periods_coupon_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const sub_periods_coupon_type&) {
    return "ores.refdata.sub_periods_coupon_type";
}

}

#endif
