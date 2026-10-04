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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_IDENTITY_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_IDENTITY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief Common identity fields shared by all instrument types.
 *
 * Extracted as a plain nested sub-struct to keep each rfl-reflected literal
 * under the MSVC C1202 ceiling. See the decomposition section of
 * doc/knowledge/architecture/data_oriented_design.org.
 */
struct instrument_identity {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief ORE product type code (soft FK to ores_trading_trade_types_tbl).
     */
    std::string trade_type_code;

    /**
     * @brief Party that owns this instrument record.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The trade this instrument belongs to, and the instrument's own key.

The trade id identifies both the trade and its instrument, so there is one
identity rather than two and the two cannot disagree.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const instrument_identity&, const instrument_identity&) = default;
};

}

#endif
