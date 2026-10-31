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
#ifndef ORES_REFDATA_API_DOMAIN_FX_VOLATILITY_SMILE_HPP
#define ORES_REFDATA_API_DOMAIN_FX_VOLATILITY_SMILE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The smile a two dimensional FX volatility surface is quoted with.
 *
 * ORE's fxVolatility is one xs:all with a mandatory Dimension and every
 * smile element optional, so the schema says only that the smile fields may be
 * absent. It cannot say that they are meaningless in one dimension: an ATM surface
 * has no smile, and a smile surface has one. That is the deficiency this part
 * corrects, because a part row exists only where the surface has a smile.
 *
 * The part holds SmileType, SmileInterpolation, SmileDelta,
 * SmileExtrapolation, TimeInterpolation, TimeWeighting and
 * ButterflyErrorTolerance. Dimension stays on the entry, because it does not
 * only decide whether a smile exists: it also decides how the entry's Deltas are
 * read.
 */
struct fx_volatility_smile final {
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
     * @brief The entry this smile belongs to. One smile per entry, so the generated table adds the
     * unique index on the live rows of (tenant_id, fx_volatility_config_id).
     */
    boost::uuids::uuid fx_volatility_config_id;

    /**
     * @brief The party that owns the entry this smile belongs to. Carried so row level security can
     * isolate the part without a join.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief How the smile is quoted.
     */
    std::optional<std::string> smile_type;

    /**
     * @brief How the smile interpolates across its deltas.
     */
    std::optional<std::string> smile_interpolation;

    /**
     * @brief The delta convention the smile is quoted in.
     */
    std::optional<std::string> smile_delta;

    /**
     * @brief How the smile extrapolates past its last delta.
     */
    std::optional<std::string> smile_extrapolation;

    /**
     * @brief How the smile interpolates along time.
     */
    std::optional<std::string> time_interpolation;

    /**
     * @brief The time weighting the smile's time interpolation uses.
     */
    std::optional<std::string> time_weighting;

    /**
     * @brief The tolerance the smile's butterfly arbitrage check allows.
     */
    std::optional<double> butterfly_error_tolerance;

    /**
     * @brief Username of the person who last modified this FX volatility smile.
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
    friend bool operator==(const fx_volatility_smile&, const fx_volatility_smile&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_volatility_smile, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_volatility_smile&) {
    return "ores.refdata.fx_volatility_smile";
}

}

#endif
