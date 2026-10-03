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
#ifndef ORES_REFDATA_API_DOMAIN_EQUITY_CURVE_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_EQUITY_CURVE_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE equity curve entry.
 *
 * The settings of one EquityCurve entry of a curveconfig.xml, one row per
 * curve_definition in the EquityCurves section: the equity's spot quote and
 * forecasting curve, how its forward curve is given, and how its dividends
 * interpolate. The quotes the forward curve is built from are rows of
 * curve_quote.
 *
 * Three corpus entries write an empty Quotes element, so has_quotes holds
 * whether the element is there. 101 write an empty Calendar; the column then
 * holds the empty text the document wrote. A calendar that is not empty is
 * checked by ores_refdata_validate_calendar_fn.
 */
struct equity_curve_config final {
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
     * @brief The equity's currency, as the document writes it.
     */
    std::string currency;

    /**
     * @brief The equity's calendar, as the document spells it; a joined calendar is one expression.
     */
    std::optional<std::string> calendar;

    /**
     * @brief The yield curve the equity's forward is projected with.
     */
    std::string forecasting_curve;

    /**
     * @brief How the forward curve is given: ForwardPrice, DividendYield and the other values ORE's
     * schema allows.
     */
    std::string equity_type;

    /**
     * @brief The exercise style of the options the curve is implied from, when it is.
     */
    std::optional<std::string> exercise_style;

    /**
     * @brief The quote of the equity's spot price.
     */
    std::string spot_quote;

    /**
     * @brief Whether the entry writes a Quotes element, which may be empty.
     */
    bool has_quotes = false;

    /**
     * @brief The day counter the curve is built on, as the document spells it.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief Whether the entry writes a DividendInterpolation element.
     */
    bool has_dividend_interpolation = false;

    /**
     * @brief What the dividend curve interpolates.
     */
    std::optional<std::string> dividend_interpolation_variable;

    /**
     * @brief How the dividend curve interpolates.
     */
    std::optional<std::string> dividend_interpolation_method;

    /**
     * @brief Whether the dividend curve extrapolates, as the ORE boolean the document wrote.
     */
    std::optional<std::string> dividend_extrapolation;

    /**
     * @brief Whether the curve extrapolates, as the ORE boolean the document wrote.
     */
    std::optional<std::string> extrapolation;

    /**
     * @brief Username of the person who last modified this equity curve config.
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
    friend bool operator==(const equity_curve_config&, const equity_curve_config&) = default;
};

/**
 * @brief Dispatch-key identifier for equity_curve_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_curve_config&) {
    return "ores.refdata.equity_curve_config";
}

}

#endif
