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
#ifndef ORES_REFDATA_API_DOMAIN_INFLATION_CURVE_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_INFLATION_CURVE_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE inflation curve entry.
 *
 * The settings of one InflationCurve entry of a curveconfig.xml, one row per
 * curve_definition in the InflationCurves section: the nominal curve it is
 * built over, whether it is zero coupon or year on year, its lag and frequency,
 * and its seasonality. The quotes it is built from are rows of curve_quote, and
 * the seasonality factors rows of inflation_seasonality_factor.
 *
 * The schema also allows a list of segments, each a convention and quotes. No
 * corpus document writes one, and the mapper refuses an entry that does rather
 * than lose it.
 */
struct inflation_curve_config final {
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
     * @brief The nominal yield curve the inflation curve is built over.
     */
    std::string nominal_term_structure;

    /**
     * @brief Whether the curve is zero coupon (ZC) or year on year (YY).
     */
    std::string inflation_type;

    /**
     * @brief The convention the curve's swaps are priced with, by its id.
     */
    std::optional<std::string> conventions;

    /**
     * @brief Whether the entry writes a Quotes element.
     */
    bool has_quotes = false;

    /**
     * @brief Whether the curve extrapolates, as the ORE boolean the document wrote.
     */
    std::optional<std::string> extrapolation;

    /**
     * @brief The curve's calendar, as the document spells it; a joined calendar is one expression.
     */
    std::string calendar;

    /**
     * @brief The day counter the curve is built on, as the document spells it.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief The observation lag of the index, as a period such as 3M.
     */
    std::string lag;

    /**
     * @brief The frequency the index is published at, as the document spells it.
     */
    std::string frequency;

    /**
     * @brief The base rate of a year on year curve, as the document writes it.
     */
    std::optional<std::string> base_rate;

    /**
     * @brief The bootstrap tolerance.
     */
    std::optional<double> tolerance;

    /**
     * @brief Whether the entry writes a Seasonality element.
     */
    bool has_seasonality = false;

    /**
     * @brief The base date of the seasonality adjustment.
     */
    std::optional<std::string> seasonality_base_date;

    /**
     * @brief The frequency of the seasonality factors.
     */
    std::optional<std::string> seasonality_frequency;

    /**
     * @brief Whether the curve starts from the last fixing date, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> use_last_fixing_date;

    /**
     * @brief What the curve interpolates.
     */
    std::optional<std::string> interpolation_variable;

    /**
     * @brief How the curve interpolates.
     */
    std::optional<std::string> interpolation_method;

    /**
     * @brief Username of the person who last modified this inflation curve config.
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
    friend bool operator==(const inflation_curve_config&, const inflation_curve_config&) = default;
};

/**
 * @brief Dispatch-key identifier for inflation_curve_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const inflation_curve_config&) {
    return "ores.refdata.inflation_curve_config";
}

}

#endif
