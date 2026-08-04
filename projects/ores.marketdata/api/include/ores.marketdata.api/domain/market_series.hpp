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

#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/domain/series_subclass.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Catalog entry identifying what is being observed (series type, metric, qualifier, asset
 * class).
 *
 * A catalog entry for a market data series — it records what is being observed:
 * a yield curve, vol surface, spot rate, fixing index, or similar. Standard
 * temporal reference data; changes infrequently so a regular table with GIST
 * exclusion is appropriate.
 *
 * Every ORE market data key follows the skeleton TYPE / METRIC / QUALIFIER;
 * asset_class and series_subclass carry the coarse taxonomy for filtering.
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
     * @brief ORE market data type token (e.g. FXSpot, YieldCurve, FXVolatility).
     */
    std::string series_type;

    /**
     * @brief Metric within the series type (e.g. SPOT, DISCOUNT, FLAT_FWD_VOLATILITY).
     */
    std::string metric;

    /**
     * @brief Free-text qualifier disambiguating the series within type+metric (e.g. EUR,
     * EUR-EURIBOR-3M, or an empty string for scalars).
     */
    std::string qualifier;

    /**
     * @brief Coarse asset class taxonomy: FX, RATES, CREDIT, EQUITY, COMMODITY, INFLATION, BOND,
     * CROSS_ASSET.
     */
    domain::asset_class asset_class = domain::asset_class::fx;

    /**
     * @brief Subclass within the asset class (e.g. SPOT, VOLATILITY, YIELD, SPREAD).
     */
    domain::series_subclass series_subclass = domain::series_subclass::spot;

    /**
     * @brief True when the series has no point dimension (e.g. an FX spot rate or a single fixing),
     * false when it is curve/surface/matrix data.
     */
    bool is_scalar = false;

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
