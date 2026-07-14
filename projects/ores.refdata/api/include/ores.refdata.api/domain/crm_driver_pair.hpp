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
#ifndef ORES_REFDATA_API_DOMAIN_CRM_DRIVER_PAIR_HPP
#define ORES_REFDATA_API_DOMAIN_CRM_DRIVER_PAIR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A directly-quoted currency pair (a spanning-tree edge) owned by a crm_topology_config.
 *
 * A single [[id:1907531F-E2AF-4BF7-84A4-6D69CB9EDFD7][spanning-tree]] edge for its parent
 * crm_topology_config: the directly-quoted pair a producer publishes ticks for. The enabled rows
 * for a (tenant, party) are read by ores.marketdata.service and fed straight into
 * ores.analytics.quant::topology_builder::build as its ccy_pair_input list -- see
 * [[file:../../../doc/agile/versions/v0/sprint_23/crm_implementation/task_wire_crm_into_marketdata_ingest.org][the
 * wiring task]].
 *
 * Scoped to a tenant and a party, matching its parent config.
 */
struct crm_driver_pair final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this driver pair.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this driver pair belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent crm_topology_config this driver pair belongs to.
     */
    boost::uuids::uuid config_id;

    /**
     * @brief Base currency ISO code of the driver pair (soft FK to currencies).
     */
    std::string base_currency_code;

    /**
     * @brief Quote currency ISO code of the driver pair (soft FK to currencies).
     */
    std::string quote_currency_code;

    /**
     * @brief Whether this edge currently participates in the topology. Disabling (rather than
     * deleting) preserves history, matching feed_binding.
     */
    bool enabled = false;

    /**
     * @brief Username of the person who last modified this CRM driver pair.
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
 * @brief Dispatch-key identifier for crm_driver_pair, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const crm_driver_pair&) {
    return "ores.refdata.crm_driver_pair";
}

}

#endif
