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
#ifndef ORES_ORE_API_DOMAIN_SERIES_KEY_SHAPE_HPP
#define ORES_ORE_API_DOMAIN_SERIES_KEY_SHAPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::ore::domain {

/**
 * @brief How an ORE market data key splits into a series and a point, per series type.
 *
 * The key grammar of ORE market data, one row per series type. Every ORE
 * key follows the skeleton TYPE/METRIC/[QUALIFIER...]/[POINT_ID], and
 * this table says where the split falls: qualifier_depth counts the
 * segments after the metric that identify the series and stay stable
 * across market dates, and every remaining segment is the point (a tenor,
 * a strike, a surface coordinate).
 *
 * The table belongs to ores.ore because the grammar it records is
 * ORE's, not ours: ORE defines the file format and we only read it. It
 * replaces a compiled C++ table, so a type ORE adds later, or one a user
 * brings, is an inserted row rather than a rebuild. A type with no row is
 * not an error -- its key folds whole into the qualifier and still
 * reconstructs verbatim -- so an uncatalogued type never aborts an
 * import.
 *
 * Two invariants hold. The reader rejects an empty table, because an
 * empty table silently degrades every key into a series of its own. A row
 * that claims a point dimension and also carries a default point is
 * contradictory and is rejected, both by the reader and by the check
 * constraint below.
 */
struct series_key_shape final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The ORE key type: the first segment of a market data key, verbatim.
     *
     * Examples: 'DISCOUNT', 'SWAPTION', 'OI_FUTURE', 'GENERIC-MD'.
     */
    std::string series_type;

    /**
     * @brief Number of slash-separated segments after the metric that identify the series. Every
     * remaining segment forms the point.
     *
     * For 'DISCOUNT', a depth of 2 splits DISCOUNT/RATE/EUR/CURVE/2Y into the series EUR/CURVE and
     * the point 2Y.
     */
    int qualifier_depth = 0;

    /**
     * @brief True when keys of this type carry a point of their own -- a tenor or a surface
     * coordinate. False for a type with no such dimension, whose whole remainder folds into the
     * qualifier.
     */
    bool has_point_dimension = false;

    /**
     * @brief The point recorded for an observation whose key carries none. Every observation stores
     * a point, so a type with a single point needs one name for it.
     *
     * FX spot is the case that matters: its single point is a real tenor, SPOT. A type whose single
     * point is not a tenor at all carries an empty string, which is the honest "no coordinate"
     * value rather than an invented tenor. A row with a point dimension always carries the empty
     * string here, because its points come from the keys.
     */
    std::string default_point;

    /**
     * @brief The key grammar of the type and the reason for its shape, written as a worked example
     * where one helps.
     *
     * Example: 'FXFWD/RATE/ccy1/ccy2/tenor'.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this series key shape.
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
    friend bool operator==(const series_key_shape&, const series_key_shape&) = default;
};

/**
 * @brief Dispatch-key identifier for series_key_shape, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_key_shape&) {
    return "ores.ore.series_key_shape";
}

}

#endif
