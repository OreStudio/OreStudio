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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_STRIKE_SURFACE_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_STRIKE_SURFACE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The strike and expiry grid a StrikeSurface kind states.
 *
 * ORE's volatilityStrikeSurfaceConfig states a grid of strikes against expiries,
 * how it interpolates each way, and how it extrapolates past the grid. Those are
 * the kind's own fields: the four terms every kind states and the priority are on
 * curve_volatility_kind, and a row of this part exists only when the entry holds
 * a kind of this type.
 */
struct curve_volatility_strike_surface final {
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
     * @brief The kind this surface belongs to. One surface per kind, so the generated table adds
     * the unique index on the live rows of (tenant_id, curve_volatility_kind_id).
     */
    boost::uuids::uuid curve_volatility_kind_id;

    /**
     * @brief The party that owns the entry this kind belongs to. Carried so row level security can
     * isolate the part without a join.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The strikes the surface is quoted at, as ORE's comma separated text.
     */
    std::string strikes;

    /**
     * @brief The expiries the surface is quoted at, as ORE's comma separated text.
     */
    std::string expiries;

    /**
     * @brief How the surface interpolates along time.
     */
    std::string time_interpolation;

    /**
     * @brief How the surface interpolates across strikes.
     */
    std::string strike_interpolation;

    /**
     * @brief Whether the surface is extrapolated at all.
     */
    bool extrapolation = false;

    /**
     * @brief How the surface extrapolates along time.
     */
    std::string time_extrapolation;

    /**
     * @brief Whether the variance rather than the volatility is extrapolated.
     */
    std::optional<bool> time_extrapolation_variance;

    /**
     * @brief How the surface extrapolates across strikes.
     */
    std::string strike_extrapolation;

    /**
     * @brief Username of the person who last modified this curve volatility strike surface.
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
    friend bool operator==(const curve_volatility_strike_surface&,
                           const curve_volatility_strike_surface&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_volatility_strike_surface, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_volatility_strike_surface&) {
    return "ores.refdata.curve_volatility_strike_surface";
}

}

#endif
