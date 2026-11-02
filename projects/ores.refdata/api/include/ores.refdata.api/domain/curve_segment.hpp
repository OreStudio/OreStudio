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
 * The twelve segment elements share a skeleton and differ in their settings, so
 * the row holds only what every element has: its definition, its type and its
 * place. Each component of settings an element states is a part of its own,
 * joined to the segment by its id, and present only for the elements that state
 * it: the convention in curve_segment_convention, the pillar rules in
 * curve_segment_pillar_rule, a simple segment's projection curve in
 * curve_segment_projection, a cross currency segment's terms in
 * curve_segment_cross_currency_terms, and six more, one per component. The
 * lists a segment holds are child rows too: its quotes in curve_quote, and the
 * index curves and default curves of the bond and default based segments in
 * curve_segment_curve.
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
