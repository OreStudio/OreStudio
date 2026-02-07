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
#ifndef ORES_REFDATA_DOMAIN_PARTY_IDENTIFIER_HPP
#define ORES_REFDATA_DOMAIN_PARTY_IDENTIFIER_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::refdata::domain {

/**
 * @brief An external identifier for a party under a specific scheme.
 *
 * External identifiers for parties, such as LEI codes, BIC/SWIFT codes,
 * national registration numbers, and tax identifiers. Each party can have
 * multiple identifiers across different schemes.
 */
struct party_identifier final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief UUID uniquely identifying this party identifier.
     *
     * Surrogate key for the party identifier record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party this identifier belongs to.
     *
     * References the parent party record.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The identification scheme for this identifier.
     *
     * References the party_id_scheme lookup table (e.g. LEI, BIC).
     */
    std::string id_scheme;

    /**
     * @brief The identifier value.
     *
     * The actual identifier string within the scheme.
     */
    std::string id_value;

    /**
     * @brief Optional description of this identifier.
     *
     * Free text description providing additional context.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this party identifier.
     */
    std::string recorded_by;

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
