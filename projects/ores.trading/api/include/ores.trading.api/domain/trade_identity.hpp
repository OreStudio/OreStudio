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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_IDENTITY_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_IDENTITY_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Ownership and surrogate key fields for a trade.
 *
 * One of five field-groups that compose trading::domain::trade via
 * rfl::Flatten<T>.  Splitting trade into small sub-structs keeps each
 * per-struct rfl field-uniqueness check (O(n²)) shallow enough to avoid
 * the MSVC C1202 recursive-template-depth error.
 * 
 * This group carries the identity / surrogate-key fields that
 * uniquely identify the trade record within a tenant.
 */
struct trade_identity {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief UUID surrogate key for this trade record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Optimistic-locking version counter.
     */
    int version = 0;

    /**
     * @brief Internal party that owns this trade (denormalised from book_id).
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Optional external trade identifier (e.g. UTI prefix or legacy system ID).
     */
    std::string external_id;

};

}

#endif
