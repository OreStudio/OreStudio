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
#ifndef ORES_REFDATA_API_DOMAIN_BOND_FUTURE_VOLATILITY_HPP
#define ORES_REFDATA_API_DOMAIN_BOND_FUTURE_VOLATILITY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE bond future volatility entry.
 *
 * The settings of one BondFutureVolatility entry of a curveconfig.xml, one
 * row per curve_definition in the BondFutureVolatilities section: the future
 * contract and the yield curve the surface is built with. Its volatility configurations, written
 * directly or inside a VolatilityConfig element, are rows of curve_volatility_config.
 *
 * No corpus document writes this section; it is modelled from the schema so a
 * document that does is held rather than refused. A OneDimSolverConfig is
 * refused, as for equity and commodity volatilities.
 */
struct bond_future_volatility final {
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
     * @brief The bond future contract.
     */
    std::string contract_name;

    /**
     * @brief The day counter of the surface, as the document spells it.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief The surface's calendar, as the document spells it.
     */
    std::optional<std::string> calendar;

    /**
     * @brief The yield curve the surface is built with.
     */
    std::optional<std::string> yield_curve_id;

    /**
     * @brief The factor a quoted strike is divided by.
     */
    std::optional<double> strike_factor;

    /**
     * @brief Whether only puts or only calls are used.
     */
    std::optional<std::string> use_only_put_call;

    /**
     * @brief Whether out of the money quotes are preferred, as the ORE boolean the document wrote.
     */
    std::optional<std::string> prefer_out_of_the_money;

    /**
     * @brief Whether the options are treated as European, as the ORE boolean the document wrote.
     */
    std::optional<std::string> treat_as_european;

    /**
     * @brief Whether the entry writes a VolatilityConfig element.
     */
    bool has_volatility_config = false;

    /**
     * @brief Username of the person who last modified this bond future volatility.
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
    friend bool operator==(const bond_future_volatility&, const bond_future_volatility&) = default;
};

/**
 * @brief Dispatch-key identifier for bond_future_volatility, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_future_volatility&) {
    return "ores.refdata.bond_future_volatility";
}

}

#endif
