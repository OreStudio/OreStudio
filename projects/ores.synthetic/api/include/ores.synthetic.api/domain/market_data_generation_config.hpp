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
#ifndef ORES_SYNTHETIC_API_DOMAIN_MARKET_DATA_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_API_DOMAIN_MARKET_DATA_GENERATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A named, party-scoped recipe for generating synthetic market data.
 *
 * A top-level container that owns one or more typed sub-configurations (FX spot
 * now; vol surface, interest-rate curves later). It is the recipe for how
 * synthetic market data is produced. Scoped to a tenant and a party so each
 * party manages its own configurations and its own generated data.
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
     * @brief Surrogate UUID uniquely identifying this configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this configuration belongs to. Not a natural key: a party
     * can legitimately own several containers (e.g. "Basic" and "Realistic" published from
     * different DQ datasets), so no uniqueness is enforced beyond the surrogate id.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Stable name for this configuration, unique per tenant and party.
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
     * @brief The DQ dataset this container was published from (ores_dq_datasets_tbl.id), if any.
     * Distinguishes containers created by separate publish-from-dq calls for the same (tenant,
     * party) -- e.g. "Basic" vs "Realistic" -- from a manually-created config, which has no dataset
     * of origin.
     */
    std::optional<boost::uuids::uuid> dataset_id;

    /**
     * @brief Username of the person who last modified this market data generation config.
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

/**
 * @brief Dispatch-key identifier for market_data_generation_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_data_generation_config&) {
    return "ores.synthetic.market_data_generation_config";
}

}

#endif
