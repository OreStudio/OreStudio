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
 * Extracted as a plain nested sub-struct to keep each rfl::Literal below
 * the MSVC C1202 threshold. See doc/investigations/msvc_c1202_rfl_complexity.org.
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
     * @brief Workspace this record belongs to; defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief UUID uniquely identifying this instrument.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief ORE product type code (soft FK to ores_trading_trade_types_tbl).
     */
    std::string trade_type_code;

    /**
     * @brief Party that owns this instrument record.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Soft back-reference to the trade this instrument belongs to, when known.
     */
    std::optional<boost::uuids::uuid> trade_id;
};

}

#endif
