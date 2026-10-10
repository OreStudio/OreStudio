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

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One segment of a yield curve recipe: the instruments the curve is bootstrapped from.
 *
 * A yield curve is bootstrapped from one or more segments, and each states its
 * Type: Deposit, Cross Currency Basis Swap, Average OIS and eighteen more.
 * The type fixes the segment element it is written under, so the row holds only
 * the type and refers to curve_segment_type, and a type written under the wrong
 * element cannot be stored.
 *
 * The twelve segment elements share a skeleton and differ in a few settings
 * each: projection curves for simple and tenor basis segments, a discount curve
 * and a spot rate for cross currency ones, a reference curve for zero spread
 * ones. Each setting is its own typed column, null where the segment's element
 * has no such setting. The lists a segment holds are child rows: its quotes in
 * curve_quote, and the index curves and default curves of the bond and default
 * based segments in curve_segment_curve.
 *
 * The convention a segment names may be any of ORE's convention kinds, which are
 * one table each, so a function that looks in all of them checks the reference.
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
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The curve entry this row belongs to.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief The segment's Type, as curve_segment_type.code names it. It also fixes the segment
     * element the row is written back as.
     */
    std::string segment_type;

    /**
     * @brief The segment's place among the curve's segments of the same element. The binding keeps
     * one list per element, so that is the order the export restores.
     */
    int position = 0;

    /**
     * @brief The convention the segment's instruments are priced with, by its id.
     */
    std::optional<std::string> conventions;

    /**
     * @brief Which date of each instrument becomes the curve's pillar.
     */
    std::optional<std::string> pillar_choice;

    /**
     * @brief The segment's priority when two segments' pillars are too close.
     */
    std::optional<int> priority;

    /**
     * @brief The least number of days between this segment's pillars and the next's.
     */
    std::optional<int> min_distance;

    /**
     * @brief The curve the segment's floating index is projected on.
     */
    std::optional<std::string> projection_curve;

    /**
     * @brief The curve a cross currency segment discounts its other currency on.
     */
    std::optional<std::string> discount_curve;

    /**
     * @brief The FX spot quote a cross currency segment converts with.
     */
    std::optional<std::string> spot_rate;

    /**
     * @brief The projection curve of a cross currency segment's domestic leg.
     */
    std::optional<std::string> projection_curve_domestic;

    /**
     * @brief The projection curve of a cross currency segment's foreign leg.
     */
    std::optional<std::string> projection_curve_foreign;

    /**
     * @brief The projection curve of a tenor basis segment's pay leg.
     */
    std::optional<std::string> projection_curve_pay;

    /**
     * @brief The projection curve of a tenor basis segment's receive leg.
     */
    std::optional<std::string> projection_curve_receive;

    /**
     * @brief The projection curve of a tenor basis segment's longer tenor.
     */
    std::optional<std::string> projection_curve_long;

    /**
     * @brief The projection curve of a tenor basis segment's shorter tenor.
     */
    std::optional<std::string> projection_curve_short;

    /**
     * @brief The curve a zero spread, bond yield shifted or yield plus default segment is built
     * over, and the first curve of a weighted average.
     */
    std::optional<std::string> reference_curve;

    /**
     * @brief The second curve of a weighted average segment.
     */
    std::optional<std::string> reference_curve_2;

    /**
     * @brief The weight of a weighted average segment's first curve.
     */
    std::optional<double> weight_1;

    /**
     * @brief The weight of a weighted average segment's second curve.
     */
    std::optional<double> weight_2;

    /**
     * @brief The Ibor index an Ibor fallback segment builds.
     */
    std::optional<std::string> ibor_index;

    /**
     * @brief The risk free curve an Ibor fallback segment falls back to.
     */
    std::optional<std::string> rfr_curve;

    /**
     * @brief The risk free index an Ibor fallback segment falls back to.
     */
    std::optional<std::string> rfr_index;

    /**
     * @brief The spread an Ibor fallback segment adds to the risk free rate.
     */
    std::optional<ores::utility::decimal::decimal> spread;

    /**
     * @brief The base curve of a discount ratio segment.
     */
    std::optional<std::string> base_curve;

    /**
     * @brief The currency of a discount ratio segment's base curve.
     */
    std::optional<std::string> base_curve_currency;

    /**
     * @brief The numerator curve of a discount ratio segment.
     */
    std::optional<std::string> numerator_curve;

    /**
     * @brief The currency of a discount ratio segment's numerator curve.
     */
    std::optional<std::string> numerator_curve_currency;

    /**
     * @brief The denominator curve of a discount ratio segment.
     */
    std::optional<std::string> denominator_curve;

    /**
     * @brief The currency of a discount ratio segment's denominator curve.
     */
    std::optional<std::string> denominator_curve_currency;

    /**
     * @brief Whether a fitted bond or bond yield shifted segment extrapolates flat.
     */
    std::optional<bool> extrapolate_flat;

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
