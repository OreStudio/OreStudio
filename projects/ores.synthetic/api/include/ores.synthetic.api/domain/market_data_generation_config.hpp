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
#ifndef ORES_SYNTHETIC_DOMAIN_MARKET_DATA_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_DOMAIN_MARKET_DATA_GENERATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::synthetic::domain {

/**
 * @brief A named configuration for generating synthetic market data.
 *
 * The top-level container that owns one or more typed sub-configurations (FX
 * spot now; vol surface, interest-rate curves later). It is the recipe for how
 * synthetic market data is produced. Scoped to a tenant and a party so each
 * party manages its own configurations and its own generated data.
 *
 * Distinct from synthetic's non-market generators (e.g. organisation
 * generation) — this type concerns market data specifically.
 */
struct market_data_generation_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for this configuration.
     */
    boost::uuids::uuid id{};

    /**
     * @brief Owning party; the configuration and the data it generates belong
     * to this party within the tenant.
     */
    boost::uuids::uuid party_id{};

    /**
     * @brief Stable source name carried as the provenance of generated
     * observations (e.g. "synthetic.gmm123"). Unique per tenant and party.
     */
    std::string name;

    /**
     * @brief Free-text description of the configuration.
     */
    std::string description;

    /**
     * @brief Whether the configuration is active and eligible for generation.
     */
    bool enabled = false;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string modified_by;

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
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
