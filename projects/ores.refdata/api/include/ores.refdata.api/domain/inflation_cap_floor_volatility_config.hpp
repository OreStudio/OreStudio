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
#ifndef ORES_REFDATA_API_DOMAIN_INFLATION_CAP_FLOOR_VOLATILITY_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_INFLATION_CAP_FLOOR_VOLATILITY_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE inflation cap and floor volatility entry.
 *
 * The settings of one InflationCapFloorVolatility entry of a curveconfig.xml,
 * one row per curve_definition in the InflationCapFloorVolatilities section:
 * a zero coupon or year on year inflation cap and floor surface over tenors and
 * strikes, with the index and curves it is built from. Its Report is a row of
 * curve_report_configuration and its BootstrapConfig a row of
 * curve_bootstrap_config, both keyed to the entry.
 */
struct inflation_cap_floor_volatility_config final {
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
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The curve entry this row belongs to.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief Whether the surface is zero coupon (ZC) or year on year (YY).
     */
    std::string inflation_type;

    /**
     * @brief Whether the quotes are prices or volatilities.
     */
    std::string quote_type;

    /**
     * @brief Whether the volatility is normal, lognormal or shifted lognormal.
     */
    std::string volatility_type;

    /**
     * @brief Whether the surface extrapolates, as the ORE boolean the document wrote.
     */
    std::string extrapolation;

    /**
     * @brief The tenors, as ORE's comma separated text.
     */
    std::string tenors;

    /**
     * @brief The number of settlement days.
     */
    std::optional<int> settlement_days;

    /**
     * @brief The cap strikes, as ORE's comma separated text.
     */
    std::optional<std::string> cap_strikes;

    /**
     * @brief The floor strikes, as ORE's comma separated text.
     */
    std::optional<std::string> floor_strikes;

    /**
     * @brief The strikes of a volatility quoted surface, as ORE's comma separated text.
     */
    std::optional<std::string> strikes;

    /**
     * @brief The surface's calendar, as the document spells it.
     */
    std::string calendar;

    /**
     * @brief The day counter of the surface, as the document spells it.
     */
    std::string day_counter;

    /**
     * @brief The business day convention of the surface's dates.
     */
    std::string business_day_convention;

    /**
     * @brief The inflation index.
     */
    std::string index;

    /**
     * @brief The inflation curve of the index.
     */
    std::string index_curve;

    /**
     * @brief Whether the index is interpolated, as the ORE boolean the document wrote.
     */
    std::optional<std::string> index_interpolated;

    /**
     * @brief The observation lag, as a period such as 3M.
     */
    std::string observation_lag;

    /**
     * @brief The yield curve the surface discounts on.
     */
    std::string yield_term_structure;

    /**
     * @brief The index the quotes are named after, when it differs from the index.
     */
    std::optional<std::string> quote_index;

    /**
     * @brief The convention the surface's instruments follow, by its id.
     */
    std::optional<std::string> conventions;

    /**
     * @brief Username of the person who last modified this inflation cap floor volatility config.
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
    friend bool operator==(const inflation_cap_floor_volatility_config&,
                           const inflation_cap_floor_volatility_config&) = default;
};

/**
 * @brief Dispatch-key identifier for inflation_cap_floor_volatility_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const inflation_cap_floor_volatility_config&) {
    return "ores.refdata.inflation_cap_floor_volatility_config";
}

}

#endif
