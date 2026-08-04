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
#ifndef ORES_MARKETDATA_API_DOMAIN_OBSERVATION_LINEAGE_HPP
#define ORES_MARKETDATA_API_DOMAIN_OBSERVATION_LINEAGE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Per-observation provenance -- which config/version/source produced a derived observation's
 * generation.
 *
 * Per-observation provenance for a *derived* market_observation row: which
 * derivation config/version produced it, and which upstream source
 * series/as-of it read. Written only alongside a derived observation --
 * never for the common OBSERVED case, and never as a column on
 * market_observations_tbl itself (a TimescaleDB hypertable explicitly
 * documented as carrying no audit columns because tick-level volumes make
 * that impractical). A row's existence *is* the "derived" marker.
 *
 * Deliberately generic, not curve-specific: this is what lets CRM's own
 * derived-cross publishing (currently pull-only -- see the CRM
 * architecture decision to never broadcast the full derived set as
 * ticks) reuse this table unchanged whenever it starts persisting,
 * stamping derivation_kind to 'CRM_DERIVATION' exactly as the IR
 * curve bootstrapper stamps it to 'IR_CURVE_BOOTSTRAP'.
 *
 * Standard bitemporal entity (GIST exclusion, soft-update/delete
 * triggers), not a hypertable: unlike market_observations, lineage rows
 * are written only for the minority of observations that are derived, so
 * tick-level volume concerns that justify market_observations's own
 * hypertable/no-audit-columns treatment do not apply here. A rerun of a
 * derivation over the same tenor/point natural key closes the prior
 * generation's lineage row and inserts a new one -- the same soft-update
 * convention every other bitemporal entity in this codebase already
 * gets, mirroring (not literally reusing) how market_observations
 * itself handles a rerun.
 */
struct observation_lineage final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this lineage row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns the observation this lineage row describes. Included in the natural
     * key because market_observations_tbl itself carries party_id as part of its own current-row
     * uniqueness index -- omitting it here would leave this table unable to uniquely identify the
     * observation it describes whenever a series isn't 1:1 with a single party.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Reference to ores_marketdata_market_series_tbl(id) -- the derived series this
     * observation belongs to.
     */
    boost::uuids::uuid series_id;

    /**
     * @brief The market_observation row's own observation_datetime this lineage row describes.
     */
    std::chrono::system_clock::time_point observation_datetime;

    /**
     * @brief Tenor or compound surface identifier, matching the market_observation row's own
     * point_id. Unlike market_observations.point_id (which is genuinely nullable there), this
     * column is not null with an empty-string sentinel for scalar series -- a stricter rule than
     * market_observations, not a literal match of its schema.
     */
    std::string point_id = "";

    /**
     * @brief The derivation config (soft reference; table depends on the owning
     * market_series.derivation_kind) that produced this observation.
     */
    boost::uuids::uuid derivation_config_id;

    /**
     * @brief The derivation config's version this observation was produced under.
     */
    int derivation_config_version = 0;

    /**
     * @brief The upstream source data's as-of timestamp this derivation read (e.g. the raw
     * instrument grid's as-of for a curve bootstrap, or the oldest contributing driver tick's
     * timestamp for a CRM triangulation).
     */
    std::chrono::system_clock::time_point source_as_of;

    /**
     * @brief Serialised JSON array of the upstream market_series ids this observation was derived
     * from (e.g. the raw series for a Funding curve bootstrap; the raw series plus the discount
     * curve's own output series for a Projection curve bootstrap; the walked driver series for a
     * CRM triangulation).
     */
    std::string source_series_ids;

    /**
     * @brief Username of the person who last modified this observation lineage.
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
 * @brief Dispatch-key identifier for observation_lineage, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const observation_lineage&) {
    return "ores.marketdata.observation_lineage";
}

}

#endif
