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
#ifndef ORES_MARKETDATA_API_DOMAIN_CRM_ENABLED_DERIVED_PAIR_HPP
#define ORES_MARKETDATA_API_DOMAIN_CRM_ENABLED_DERIVED_PAIR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief A curated derived (non-edge) currency pair a consumer may request via the CRM on-demand
 * endpoint.
 *
 * A currency pair that is *not* a [[id:1907531F-E2AF-4BF7-84A4-6D69CB9EDFD7][spanning-tree]] edge
 * (see crm_driver_pair) but is nonetheless curated as available for on-demand triangulation via
 * marketdata.v1.crm.rate -- see [[id:DC08D216-348D-4511-A42D-4016EBBF38F7][the architecture
 * decision]] to never broadcast the full reachable derived set, only serve a config-driven list on
 * request. Owned by a crm_topology_config alongside its driver pairs.
 *
 * Scoped to a tenant and a party, matching its parent config.
 */
struct crm_enabled_derived_pair final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this enabled derived pair.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this enabled derived pair belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent crm_topology_config this enabled derived pair belongs to.
     */
    boost::uuids::uuid config_id;

    /**
     * @brief Base currency ISO code of the derived pair (soft FK to currencies).
     */
    std::string base_currency_code;

    /**
     * @brief Quote currency ISO code of the derived pair (soft FK to currencies).
     */
    std::string quote_currency_code;

    /**
     * @brief Whether this derived pair is currently available on request.
     */
    bool enabled = false;

    /**
     * @brief Username of the person who last modified this CRM enabled derived pair.
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
 * @brief Dispatch-key identifier for crm_enabled_derived_pair, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const crm_enabled_derived_pair&) {
    return "ores.marketdata.crm_enabled_derived_pair";
}

}

#endif
