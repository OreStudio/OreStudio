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
#ifndef ORES_REFDATA_API_DOMAIN_INFLATION_CAP_FLOOR_VOLATILITY_STRIKES_HPP
#define ORES_REFDATA_API_DOMAIN_INFLATION_CAP_FLOOR_VOLATILITY_STRIKES_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The strikes one axis of an inflation cap and floor surface is quoted at.
 *
 * ORE's inflationCapFloorVolatility states three strike lists: CapStrikes,
 * FloorStrikes and Strikes. They are alternatives, not three facts: a cap quote
 * is struck on caps, a floor quote on floors, and a surface on both sides at once.
 * Three optional columns in one row would let a row state a cap list and a floor
 * list and leave the reader to work out which it meant.
 *
 * The part is a row per axis the entry states, so the choice is structural. An
 * entry with caps and floors holds two rows, a surface holds one, and an entry that
 * states no list holds none.
 */
struct inflation_cap_floor_volatility_strikes final {
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
     * @brief The entry this strike list belongs to. An entry holds one row for each axis it states,
     * so the key is this column with axis, and the generated table adds the unique index on the
     * pair over the live rows.
     */
    boost::uuids::uuid inflation_cap_floor_volatility_config_id;

    /**
     * @brief Which list this row is: Cap, Floor or Both. Both is the schema's Strikes, the one list
     * a surface states for its two sides.
     */
    std::string axis;

    /**
     * @brief The party that owns the entry this row belongs to. Carried so row level security can
     * isolate the part without a join.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The strikes this axis is quoted at, as ORE's comma separated text.
     */
    std::string strikes;

    /**
     * @brief Username of the person who last modified this inflation cap floor volatility strikes.
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
    friend bool operator==(const inflation_cap_floor_volatility_strikes&,
                           const inflation_cap_floor_volatility_strikes&) = default;
};

/**
 * @brief Dispatch-key identifier for inflation_cap_floor_volatility_strikes, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const inflation_cap_floor_volatility_strikes&) {
    return "ores.refdata.inflation_cap_floor_volatility_strikes";
}

}

#endif
