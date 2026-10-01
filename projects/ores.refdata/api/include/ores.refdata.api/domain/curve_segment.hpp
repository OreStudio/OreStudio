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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_SEGMENT_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_SEGMENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One segment of a curve recipe: the instrument the curve is bootstrapped from.
 *
 * A curve is bootstrapped from one or more segments, and each segment names the
 * kind of instrument it is built from. The corpus uses ten kinds -- Simple in
 * 1198 entries, CrossCurrency in 500, ZeroSpread in 66, Direct in 50,
 * AverageOIS in 24, IborFallback in 20, DiscountRatio in 15 and four more --
 * in one Segments block per curve.
 *
 * The kinds look different and share a skeleton: every one of them carries a
 * Type and a Conventions, and each adds its own settings -- ProjectionCurve
 * for a simple segment, DiscountCurve and SpotRate for a cross-currency one,
 * ReferenceCurve for a zero spread. The kind is a column, the Type and
 * Conventions are columns because every kind has them, and the rest is written
 * into extras as a stated list of name and value pairs, so the ten kinds need
 * one table rather than ten.
 *
 * Each segment's own quote list belongs to the segment, in curve_quote.
 */
struct curve_segment final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the segment.
     */
    boost::uuids::uuid id;

    /**
     * @brief The curve recipe this segment bootstraps.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief The segment's element name in the document: Simple, CrossCurrency, ZeroSpread and the
     * rest. It is what the export writes the segment back as.
     */
    std::string kind;

    /**
     * @brief The segment's own Type -- FRA, Tenor Basis Swap, Deposit and so on -- which chooses
     * the instrument the segment is built from. Named segment_type because type alone reads as the
     * row's own type.
     */
    std::optional<std::string> segment_type;

    /**
     * @brief The convention set the segment's instruments are priced with, by name.
     */
    std::optional<std::string> conventions;

    /**
     * @brief The segment's remaining settings, one per semicolon, each a pipe-separated name and
     * value: ProjectionCurve, DiscountCurve, SpotRate, PillarChoice, Priority, MinDistance and
     * whatever a kind adds next.
     */
    std::optional<std::string> extras;

    /**
     * @brief The order the document wrote the segment in, which the export restores.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this curve segment.
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
    friend bool operator==(const curve_segment&, const curve_segment&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_segment, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_segment&) {
    return "ores.refdata.curve_segment";
}

}

#endif
