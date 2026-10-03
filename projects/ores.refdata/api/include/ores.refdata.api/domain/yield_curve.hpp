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
#ifndef ORES_REFDATA_API_DOMAIN_YIELD_CURVE_HPP
#define ORES_REFDATA_API_DOMAIN_YIELD_CURVE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE yield curve entry.
 *
 * The settings of one YieldCurve entry of a curveconfig.xml, one row per
 * curve_definition in the YieldCurves section. The entry's identity is on the
 * definition, its segments in curve_segment, its bootstrap settings in
 * curve_bootstrap_config; this row holds the rest, each as the column its
 * schema type calls for.
 *
 * Extrapolation and ExcludeT0FromInterpolation are ORE booleans, which accept
 * Y, true and True among other spellings. They are held as the spelling the
 * document used, so the export writes it back unchanged. The day counter is held
 * the same way and refers to day_counter.
 */
struct yield_curve final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The curve entry this row belongs to.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief The curve's currency, as the document writes it.
     */
    std::string currency;

    /**
     * @brief The curve the entry's instruments are discounted on, by its CurveId. An entry may name
     * itself.
     */
    std::string discount_curve;

    /**
     * @brief What the curve interpolates: Zero, Discount or Forward.
     */
    std::optional<std::string> interpolation_variable;

    /**
     * @brief How the curve interpolates between its pillars.
     */
    std::optional<std::string> interpolation_method;

    /**
     * @brief The number of segments that use the first of a mixed interpolation's methods.
     */
    std::optional<int> mixed_interpolation_cutoff;

    /**
     * @brief The day counter the curve is built on, as the document spells it. ORE writes it as
     * YieldCurveDayCounter.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief The bootstrap tolerance.
     */
    std::optional<double> tolerance;

    /**
     * @brief Whether the curve extrapolates beyond its last pillar, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> extrapolation;

    /**
     * @brief How the curve extrapolates beyond its last pillar.
     */
    std::optional<std::string> extrapolation_method;

    /**
     * @brief Whether the curve leaves today out of its interpolation, as the ORE boolean the
     * document wrote.
     */
    std::optional<std::string> exclude_t0_from_interpolation;

    /**
     * @brief Whether the entry carries a Report element. Every one in the corpus is empty, so
     * presence is the fact the export needs.
     */
    bool has_report = false;

    /**
     * @brief The PillarDates of the entry's Report, when it gives them.
     */
    std::optional<std::string> report_pillar_dates;

    /**
     * @brief Username of the person who last modified this yield curve.
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
    friend bool operator==(const yield_curve&, const yield_curve&) = default;
};

/**
 * @brief Dispatch-key identifier for yield_curve, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const yield_curve&) {
    return "ores.refdata.yield_curve";
}

}

#endif
