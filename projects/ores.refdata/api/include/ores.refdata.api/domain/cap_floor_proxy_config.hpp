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
#ifndef ORES_REFDATA_API_DOMAIN_CAP_FLOOR_PROXY_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_CAP_FLOOR_PROXY_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The proxy surface a cap and floor volatility entry is built from.
 *
 * +#+updated: 2026-10-30
 *
 * A cap and floor volatility entry may name a proxy surface instead of its own
 * quotes, and ORE states it as a ProxyConfig element inside the entry's
 * CapFloorVolatility:
 *
 * #+begin_example
 * <ProxyConfig>
 *   <Source>
 *     <CurveId>...</CurveId>
 *     <Index>...</Index>
 *     <RateComputationPeriod>...</RateComputationPeriod>
 *   </Source>
 *   <Target>
 *     <Index>...</Index>
 *     <RateComputationPeriod>...</RateComputationPeriod>
 *     <ONCapSettlementDays>...</ONCapSettlementDays>
 *   </Target>
 *   <ScalingFactor>...</ScalingFactor>
 * </ProxyConfig>
 * #+end_example
 *
 * That block was seven nullable columns of the entry, meaningful only when the
 * entry named a proxy. A row of this part holds it, and a row exists only when the
 * entry names one, so the presence of the part is the flag and no has_proxy_config
 * column is needed.
 *
 * The swaption entry's proxy block is a different element: its source and target
 * carry ShortSwapIndexBase and SwapIndexBase where this one carries an index
 * and a rate computation period. It is its own part, swaption_proxy_config, for
 * that reason: the two share a shape, not a type.
 */
struct cap_floor_proxy_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The surrogate key of this part row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The volatility entry this proxy belongs to. One row per entry, so the generated table
     * adds the unique index on the live rows of (tenant_id, cap_floor_volatility_config_id): an
     * entry names a proxy or it does not, and never two. History is unaffected, because the index
     * is partial on valid_to infinity.
     */
    boost::uuids::uuid cap_floor_volatility_config_id;

    /**
     * @brief The party that owns the entry this proxy belongs to. Carried so row level security can
     * isolate the part without a join, and stamped from the session that writes the document.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The cap and floor volatility the proxy surface is built from.
     */
    std::optional<std::string> source_curve_id;

    /**
     * @brief The index of the proxy's source.
     */
    std::optional<std::string> source_index;

    /**
     * @brief The rate computation period of the proxy's source.
     */
    std::optional<std::string> source_rate_computation_period;

    /**
     * @brief The index of the proxy's target.
     */
    std::optional<std::string> target_index;

    /**
     * @brief The rate computation period of the proxy's target.
     */
    std::optional<std::string> target_rate_computation_period;

    /**
     * @brief The overnight cap settlement days of the proxy's target.
     */
    std::optional<int> target_on_cap_settlement_days;

    /**
     * @brief The factor the proxy's volatilities are scaled by.
     */
    std::optional<double> scaling_factor;

    /**
     * @brief Username of the person who last modified this cap floor proxy config.
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
    friend bool operator==(const cap_floor_proxy_config&, const cap_floor_proxy_config&) = default;
};

/**
 * @brief Dispatch-key identifier for cap_floor_proxy_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cap_floor_proxy_config&) {
    return "ores.refdata.cap_floor_proxy_config";
}

}

#endif
