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
 * Template: cpp_field_group.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_SWAP_LEG_IDENTITY_HPP
#define ORES_TRADING_API_DOMAIN_SWAP_LEG_IDENTITY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>

namespace ores::trading::domain {

/**
 * @brief Identity fields of a rates swap leg.
 *
 * Extracted as a plain nested sub-struct to keep each rfl-reflected literal
 * under the MSVC C1202 ceiling. A leg carries its own surrogate key, the trade
 * it belongs to and its ordinal within that instrument, so the leg identity is
 * not the instrument identity: it is the trade's key plus the leg's own two
 * members. The terms stay flat on the entity. See the decomposition section of
 * doc/knowledge/architecture/data_oriented_design.org.
 *
 * The group is the swap-leg sibling of ores.trading.composite_leg_identity:
 * the shared swap_legs table carries one key and one parent column, and its
 * ordinal is leg_number rather than the basket's leg_sequence.
 */
struct swap_leg_identity {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this leg row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this leg record.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The trade the leg belongs to. The trade id identifies both the trade and its
instrument, so it is the parent key rather than a separate instrument key.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief 1-based ordinal of this leg within the parent instrument.
     */
    int leg_number = 1;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const swap_leg_identity&, const swap_leg_identity&) = default;
};

}

#endif
