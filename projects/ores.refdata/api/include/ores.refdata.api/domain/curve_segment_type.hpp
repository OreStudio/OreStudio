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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_SEGMENT_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_SEGMENT_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One ORE yield curve segment type and the segment element it is written under.
 *
 * A yield curve in curveconfig.xml is a list of segments, and each segment
 * states its Type: Deposit, FRA, Cross Currency Basis Swap, Average
 * OIS and so on. The schema fixes the set: an enumeration for the Simple,
 * Direct, TenorBasis, CrossCurrency, ZeroSpread and DiscountRatio
 * segments, and a fixed value for the other six. Twenty-one types in all.
 *
 * The type is a seeded lookup rather than text, so a new segment type is a row
 * and a segment that names an unknown type is refused. The segment element is a
 * column of the type, because each type belongs to exactly one element.
 */
struct curve_segment_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The segment's Type exactly as ORE spells it: 'Deposit', 'Cross Currency Basis Swap',
     * 'Average OIS'. It chooses the instrument a yield curve segment is bootstrapped from.
     */
    std::string code;

    /**
     * @brief The segment element the type is written under: 'Simple' for 'Deposit', 'CrossCurrency'
     * for 'FX Forward'. Every type belongs to one kind, so a segment stores only its type and the
     * kind follows from it.
     */
    std::string segment_kind;

    /**
     * @brief What the segment is built from, in one line, for a reader who does not know ORE.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this curve segment type.
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
    friend bool operator==(const curve_segment_type&, const curve_segment_type&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_segment_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_segment_type&) {
    return "ores.refdata.curve_segment_type";
}

}

#endif
