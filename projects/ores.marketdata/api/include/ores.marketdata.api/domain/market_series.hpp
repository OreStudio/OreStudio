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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Catalog entry identifying what is being observed, by its canonical oresmd URI.
 *
 * A catalog entry for a market data series — it records what is being observed:
 * a yield curve, vol surface, spot rate, fixing index, or similar. Standard
 * temporal reference data; changes infrequently so a regular table with GIST
 * exclusion is appropriate.
 *
 * Identity is the canonical oresmd URI (e.g.
 * oresmd://ir/eur?tenor3m&typequote&quoteir_swap&metricrate&point5y).
 * Classification (asset class, subclass, scalar-ness) derives from the URI in
 * the oresmd layer, not from stored columns. The URI is read and written
 * end-to-end: the import boundaries project ORE keys into URIs via
 * oresmd_projections::from_ore_key= / from_index_name, and the oresmd
 * parser canonicalises before persistence.
 *
 * derivation_kind/derivation_config_id/derivation_config_version mark
 * whether this series is directly observed (the sentinel OBSERVED) or
 * derived by a named mechanism (e.g. IR_CURVE_BOOTSTRAP,
 * CRM_DERIVATION) -- so any published series answers "was this observed
 * or computed, and by what" without guessing from the source tag on its
 * observations. Per-observation lineage (which source series/as-of a
 * specific derived point came from) is a separate concern, tracked by
 * observation_lineage, not this catalog-level marker.
 */
struct market_series final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this market series.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this market series.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Canonical oresmd URI identifying this series (e.g.
     * oresmd://ir/eur?tenor3m&typequote&quoteir_swap&metricrate&point5y=). One URI per series; the
     * natural key is (party, uri). Classification (asset class, subclass, scalar-ness) derives from
     * the URI in the oresmd layer, so no classification columns are stored — a reader needs the
     * oresmd parser, not extra columns, to know what a series is.
     */
    std::string oresmd_uri;

    /**
     * @brief References derivation_kind.code -- whether this series is directly observed (the
     * sentinel OBSERVED, the default) or derived by a named mechanism (IR_CURVE_BOOTSTRAP,
     * CRM_DERIVATION, ...). Paired with derivation_config_id/derivation_config_version: a row's own
     * derivation_kind is the discriminator that says whether those two fields are meaningful or
     * hold their nil/zero sentinel.
     */
    std::string derivation_kind = "OBSERVED";

    /**
     * @brief The recipe/config that produced this series, when derived --
     * ores_utility_nil_uuid_fn() (the sentinel, not null) when derivation_kind is 'OBSERVED'.
     * Deliberately not a hard FK: the table this id resolves against depends on derivation_kind
     * (the IR curve bootstrap config for IR_CURVE_BOOTSTRAP, the CRM topology config for
     * CRM_DERIVATION), so it is a soft, self-describing reference, the same shape ir_curve_tick
     * already uses for its own producer/config identity.
     */
    boost::uuids::uuid derivation_config_id = boost::uuids::nil_uuid();

    /**
     * @brief The derivation config's version this series was produced under -- 0 (the sentinel)
     * when derivation_kind is 'OBSERVED'.
     */
    int derivation_config_version = 0;

    /**
     * @brief Username of the person who last modified this market series.
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
 * @brief Dispatch-key identifier for market_series, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_series&) {
    return "ores.marketdata.market_series";
}

}

#endif
