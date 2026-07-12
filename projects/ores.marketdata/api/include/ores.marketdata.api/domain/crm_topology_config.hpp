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
#ifndef ORES_MARKETDATA_API_DOMAIN_CRM_TOPOLOGY_CONFIG_HPP
#define ORES_MARKETDATA_API_DOMAIN_CRM_TOPOLOGY_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief A named, party-scoped CRM topology config (pivot currency) driving ores.analytics.quant's
 * rate_engine.
 *
 * A top-level container that owns the [[id:1907531F-E2AF-4BF7-84A4-6D69CB9EDFD7][spanning-tree
 * topology]]'s pivot currency and the set of [[id:B38B3869-02FD-4CC7-99BD-9A77904ACA19][CRM]]
 * driver pairs (crm_driver_pair, a separate, config_id-referencing entity) that hang off it.
 * ores.marketdata.service reads the enabled config per (tenant, party) and its enabled driver
 * pairs, feeds them through ores.analytics.quant::topology_builder::build, and keeps one
 * service::rate_engine per (tenant, party) up to date -- see
 * [[file:../../../doc/agile/versions/v0/sprint_23/crm_implementation/task_wire_crm_into_marketdata_ingest.org][the
 * wiring task]]. Qt UI is deferred to a later task (ores.cpp.qt.enabled: false above), same as
 * market_series.
 *
 * Scoped to a tenant and a party so each party runs its own, independent
 * CRM -- two parties in the same tenant may have entirely different
 * topologies and never share an engine.
 */
struct crm_topology_config final {
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
     * @brief Owning party (legal entity) this CRM topology belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Stable name for this configuration, unique per tenant and party (e.g. "primary").
     */
    std::string name;

    /**
     * @brief ISO code of the pivot (aggregation) currency every driver pair's path runs through
     * (soft FK to currencies) -- see [[id:1907531F-E2AF-4BF7-84A4-6D69CB9EDFD7][CRM graph topology
     * and spanning tree]].
     */
    std::string pivot_currency_code;

    /**
     * @brief Whether this is the active CRM topology for this (tenant, party) --
     * ores.marketdata.service runs at most one active rate_engine per (tenant, party); a second
     * config may exist disabled (e.g. while being edited) without disrupting the running engine.
     */
    bool enabled = false;

    /**
     * @brief Username of the person who last modified this CRM topology config.
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
 * @brief Dispatch-key identifier for crm_topology_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const crm_topology_config&) {
    return "ores.marketdata.crm_topology_config";
}

}

#endif
