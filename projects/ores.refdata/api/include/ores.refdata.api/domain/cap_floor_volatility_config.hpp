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
#ifndef ORES_REFDATA_API_DOMAIN_CAP_FLOOR_VOLATILITY_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_CAP_FLOOR_VOLATILITY_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE cap and floor volatility entry.
 *
 * The settings of one CapFloorVolatility entry of a curveconfig.xml, one row
 * per curve_definition in the CapFloorVolatilities section: the cap and floor
 * surface's tenors and strikes, the index and discount curve it is stripped with,
 * and how it interpolates. Its Report is a row of curve_report_configuration,
 * its BootstrapConfig a row of curve_bootstrap_config and its
 * ParametricSmileConfiguration a row of curve_parametric_smile.
 *
 * A ProxyConfig builds the surface from another one; it is one per entry, so
 * its fields are the proxy_ columns, and has_proxy_config says whether the
 * entry wrote it. The schema allows both UseEffeciveVolatility, a misspelling
 * kept for old documents, and UseEffectiveVolatility; each has its own column so
 * either round trips.
 */
struct cap_floor_volatility_config final {
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
     * @brief Whether the input volatility is normal, lognormal or shifted lognormal.
     */
    std::optional<std::string> volatility_type;

    /**
     * @brief The volatility type the surface is converted to.
     */
    std::optional<std::string> output_volatility_type;

    /**
     * @brief The shift of the model.
     */
    std::optional<double> model_shift;

    /**
     * @brief The shift of the output volatility.
     */
    std::optional<double> output_shift;

    /**
     * @brief How the surface extrapolates.
     */
    std::optional<std::string> extrapolation;

    /**
     * @brief How the surface interpolates.
     */
    std::optional<std::string> interpolation_method;

    /**
     * @brief Whether ATM quotes are included, as the ORE boolean the document wrote.
     */
    std::optional<std::string> include_atm;

    /**
     * @brief The day counter of the surface, as the document spells it.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief The surface's calendar, as the document spells it.
     */
    std::optional<std::string> calendar;

    /**
     * @brief The business day convention of the surface's dates.
     */
    std::optional<std::string> business_day_convention;

    /**
     * @brief The cap and floor tenors, as ORE's comma separated text.
     */
    std::optional<std::string> tenors;

    /**
     * @brief The strikes, as ORE's comma separated text.
     */
    std::optional<std::string> strikes;

    /**
     * @brief Whether missing quotes are allowed, as the ORE boolean the document wrote.
     */
    std::optional<std::string> optional_quotes;

    /**
     * @brief The IBOR index the caps and floors fix against.
     */
    std::optional<std::string> ibor_index;

    /**
     * @brief The index the caps and floors fix against, the newer form of IborIndex.
     */
    std::optional<std::string> index;

    /**
     * @brief The rate computation period of an overnight index cap.
     */
    std::optional<std::string> rate_computation_period;

    /**
     * @brief The settlement days of an overnight index cap.
     */
    std::optional<int> on_cap_settlement_days;

    /**
     * @brief The curve the caps and floors are discounted on.
     */
    std::optional<std::string> discount_curve;

    /**
     * @brief The ATM tenors, as ORE's comma separated text.
     */
    std::optional<std::string> atm_tenors;

    /**
     * @brief The number of settlement days.
     */
    std::optional<int> settlement_days;

    /**
     * @brief Whether the surface interpolates on term or optionlet volatilities.
     */
    std::optional<std::string> interpolate_on;

    /**
     * @brief How the surface interpolates in time.
     */
    std::optional<std::string> time_interpolation;

    /**
     * @brief How the surface interpolates in strike.
     */
    std::optional<std::string> strike_interpolation;

    /**
     * @brief Whether the quotes are term or optionlet volatilities.
     */
    std::optional<std::string> input_type;

    /**
     * @brief Whether the quote names include the index name, as the ORE boolean the document wrote.
     */
    std::optional<std::string> quote_includes_index_name;

    /**
     * @brief Whether the first period is flat, as the ORE boolean the document wrote.
     */
    std::optional<std::string> flat_first_period;

    /**
     * @brief The schema's misspelt UseEffeciveVolatility element, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> use_effecive_volatility;

    /**
     * @brief Whether effective volatilities are used, as the ORE boolean the document wrote.
     */
    std::optional<std::string> use_effective_volatility;

    /**
     * @brief Whether the entry writes a ProxyConfig element.
     */
    bool has_proxy_config = false;

    /**
     * @brief The cap and floor volatility a proxy surface is built from.
     */
    std::optional<std::string> proxy_source_curve_id;

    /**
     * @brief The index of the proxy's source.
     */
    std::optional<std::string> proxy_source_index;

    /**
     * @brief The rate computation period of the proxy's source.
     */
    std::optional<std::string> proxy_source_rate_computation_period;

    /**
     * @brief The index of the proxy's target.
     */
    std::optional<std::string> proxy_target_index;

    /**
     * @brief The rate computation period of the proxy's target.
     */
    std::optional<std::string> proxy_target_rate_computation_period;

    /**
     * @brief The overnight cap settlement days of the proxy's target.
     */
    std::optional<int> proxy_target_on_cap_settlement_days;

    /**
     * @brief The factor the proxy's volatilities are scaled by.
     */
    std::optional<double> proxy_scaling_factor;

    /**
     * @brief Username of the person who last modified this cap floor volatility config.
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
    friend bool operator==(const cap_floor_volatility_config&,
                           const cap_floor_volatility_config&) = default;
};

/**
 * @brief Dispatch-key identifier for cap_floor_volatility_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cap_floor_volatility_config&) {
    return "ores.refdata.cap_floor_volatility_config";
}

}

#endif
