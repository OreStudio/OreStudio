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
#ifndef ORES_REFDATA_DOMAIN_PARTY_ID_SCHEME_HPP
#define ORES_REFDATA_DOMAIN_PARTY_ID_SCHEME_HPP

#include <chrono>
#include <string>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Classification of external identifier types for parties.
 *
 * Reference data table defining valid party identifier scheme types.
 * Examples: 'LEI', 'BIC', 'MIC', 'DUNS'.
 * 
 * Party ID schemes are managed by the system tenant. The optional
 * coding_scheme_code field cross-references the DQ coding scheme table.
 */
struct party_id_scheme final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique scheme code.
     *
     * Examples: 'LEI', 'BIC', 'MIC'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the identifier scheme.
     */
    std::string name;

    /**
     * @brief Detailed description of the identifier scheme.
     */
    std::string description;

    /**
     * @brief Cross-reference to DQ coding scheme table.
     *
     * Optional link to ores_dq_coding_schemes_tbl for validation.
     */
    std::string coding_scheme_code;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order;

    /**
     * @brief Username of the person who last modified this party ID scheme.
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
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
