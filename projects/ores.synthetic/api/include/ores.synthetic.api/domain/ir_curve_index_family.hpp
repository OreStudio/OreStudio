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
#ifndef ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_INDEX_FAMILY_HPP
#define ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_INDEX_FAMILY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A rate index family a synthetic IR curve names, such as sofr, euribor or hibor, and the
 * tenors it admits.
 *
 * The rate index families an
 * [[id:8B840E8D-2763-4A1F-8BC2-6C9E354DB673][ir_curve_generation_config]] can name, one row per
 * family, and the tenors each one admits. An overnight family, such as sonia, names no tenor. A
 * term family, such as euribor, names one, and the curve then projects that tenor's rate and
 * discounts off the overnight curve of its currency. A family can be both: hibor is an overnight
 * index for CNH and a term index for HKD. A dated tenor, such as SOFR's FOMC, is neither: it names
 * a curve whose pillars fall on meeting dates.
 *
 * This replaces the two lists the curve config's checks and the DQ publish
 * function wrote out by hand, so a new family is a seed row rather than four
 * edits. Scoped to ores.synthetic, as the yield curve process types are,
 * because synthetic IR curve generation is its only consumer. Managed by the
 * system tenant.
 */
struct ir_curve_index_family final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique family code, in the lower case the curve config stores it.
     *
     * Examples: 'sofr', 'euribor', 'hibor'.
     */
    std::string code;

    /**
     * @brief Human-readable name of the family, as a screen shows it, such as HIBOR.
     */
    std::string name;

    /**
     * @brief What the family is, and where it is published.
     */
    std::string description;

    /**
     * @brief Whether a curve of this family may name no tenor: an overnight index curve that
     * discounts off itself.
     */
    bool admits_overnight = false;

    /**
     * @brief Whether a curve of this family may name a term tenor, such as 3M: a projection curve
     * that discounts off the overnight curve of its currency.
     */
    bool admits_term = false;

    /**
     * @brief The one tenor code that names a meeting-dated curve of this family, such as FOMC for
     * SOFR, or empty when the family has none. A dated curve discounts off itself, as an overnight
     * curve does.
     */
    std::string dated_tenor;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this IR curve index family.
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
    friend bool operator==(const ir_curve_index_family&, const ir_curve_index_family&) = default;
};

/**
 * @brief Dispatch-key identifier for ir_curve_index_family, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ir_curve_index_family&) {
    return "ores.synthetic.ir_curve_index_family";
}

}

#endif
